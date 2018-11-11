{-# LANGUAGE OverloadedStrings #-}

-- | Routes module
--
-- Contains the routing logic for the application.
-- TODO: Reorganize this module

module Routes where

import           Control.Concurrent.MVar        ( MVar
                                                , readMVar
                                                , modifyMVar
                                                )
import           Control.Monad                  ( void )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Hashable                  ( Hashable )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as M
import qualified Data.Text                     as TS
import qualified Data.Text.Lazy                as TL
import           Data.Text.Lazy.Encoding        ( decodeUtf8 )
import           Web.Scotty              hiding ( notFound )
import           Network.HTTP.Types

import           Types
import           Lib
import           UrlEncoded

type ActivityMap = HashMap Int Activity
type PersonMap   = HashMap TS.Text DbPerson

routes :: MVar ActivityMap -> MVar PersonMap -> ScottyM ()
routes as ps = do
  get "/" $ html =<< liftIO (TL.pack <$> readFile "web/build/index.html")

  staticRoutes

  -- TODO: Return session cookie that is valid for some time
  post "/login" $ rescueBadRequest $ do
    login <- liftNothing =<< fromUrlEncoded <$> bodyStrict
    whenExists (getItemById (loginEmail login) ps) $ \p ->
      if checkPassword (loginPassword login) p
        then json (person p)
        else unauthorized

  post "/register" $ rescueBadRequest $ do
    regReq <- liftNothing =<< fromUrlEncoded <$> bodyStrict
    let pId = (email . person) regReq
    whenNotExists (getItemById pId ps) $ do
      persons <- updateItems ps
        $ \persons -> M.insert pId <$> newPerson regReq <*> pure persons
      void $ storeState "persons.state" persons
      json (person regReq)

  get "/persons" $ do
    persons <- fmap person <$> getItems ps
    json persons

  get "/person/:personId" $ do
    personId <- param "personId"
    p        <- fmap person <$> getItemById personId ps
    maybe notFound json p

  get "/activities" $ do
    activities <- getItems as
    persons    <- fmap person <$> getItems ps
    json (activities, persons)

  get "/activity/:activityId" $ do
    activityId <- read <$> param "activityId"
    persons    <- fmap person <$> getItems ps
    activity   <- getItemById activityId as
    flip (maybe notFound) activity $ \act ->
      let actHost      = M.lookup (host act) persons
          actAttendees = map (`M.lookup` persons) (attendees act)
      in  json (act, actHost, actAttendees)

  post "/activity/" $ rescueBadRequest $ do
    let addNewActivity :: Activity -> ActivityMap -> ActivityMap
        addNewActivity a am = M.insert (newId am) a am
    newActivity <- liftNothing =<< fromUrlEncoded <$> bodyStrict
    activities  <- updateItems as (pure . addNewActivity newActivity)
    void $ storeState "activities.state" activities
    text "OK"

  put "/activity/:activityId" $ rescueBadRequest $ do
    activityId <- read <$> param "activityId"
    updatedActivity <- liftNothing =<< fromUrlEncoded <$> bodyStrict
    activities <- updateItems as $ pure . M.insert activityId updatedActivity
    void $ storeState "activities.state" activities
    text "OK"

staticRoutes :: ScottyM ()
staticRoutes = do
  defineStaticRoute "img"
  defineStaticRoute "js"
  defineStaticRoute "css"

defineStaticRoute :: String -> ScottyM ()
defineStaticRoute type' =
  get (capture $ "/static/" <> type' <> "/:resource") $ do
    resource <- param "resource"
    setHeader "Content-Type" =<< contentType type'
    file ("web/build/static/js/" <> resource)
 where
  contentType "css" = pure "text/css"
  contentType "js"  = pure "application/javascript"
  contentType "img" = pure "image/png"
  contentType _     = raise "defineStaticRoute: not a defined Content-Type"

notFound :: ActionM ()
notFound = status notFound404 >> text "404 Not found"

unauthorized :: ActionM ()
unauthorized = status unauthorized401 >> text "401 Login failed"

badRequest :: TL.Text -> ActionM ()
badRequest msg = status badRequest400 >> text msg

rescueBadRequest :: ActionM () -> ActionM ()
rescueBadRequest act = rescue act (const $ badRequest "Failed to parse form")

updateActivity :: Int -> Activity -> ActivityMap -> ActivityMap
updateActivity = M.insert

getAttendees :: [TS.Text] -> PersonMap -> PersonMap
getAttendees ids = M.filterWithKey (\pid _ -> pid `elem` ids)

getItemById
  :: (Eq k, Hashable k, MonadIO m)
  => k -- ^ Id of item to retrieve
  -> MVar (HashMap k v)
  -> m (Maybe v)
getItemById k mvar = liftIO $ M.lookup k <$> readMVar mvar

getItems :: MonadIO m => MVar a -> m a
getItems mvar = liftIO $ readMVar mvar

updateItems :: MonadIO m => MVar a -> (a -> IO a) -> m a
updateItems mvar f = liftIO $ modifyMVar mvar (twice f)
 where
  twice :: (a -> IO a) -> (a -> IO (a, a))
  twice fn a = do
    fa <- fn a
    pure (fa, fa)

newId :: HashMap Int a -> Int
newId = (+ 1) . maximum . M.keys

bodyStrict :: ActionM TS.Text
bodyStrict = TL.toStrict . decodeUtf8 <$> body

-- Lifts Nothings into a Scotty error
liftNothing :: Maybe a -> ActionM a
liftNothing Nothing  = raise "liftNothing: Nothing"
liftNothing (Just a) = pure a

whenNotExists :: ActionM (Maybe a) -> ActionM () -> ActionM ()
whenNotExists actM actNothing = do
  m <- actM
  maybe actNothing (const $ badRequest "Already exists") m
