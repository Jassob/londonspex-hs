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
import           Data.Maybe                     ( isJust )
import qualified Data.Text                     as TS
import qualified Data.Text.Lazy                as TL
import           Data.Text.Lazy.Encoding        ( decodeUtf8 )
import           Web.Scotty              hiding ( notFound )
import           Network.HTTP.Types

import           Types
import           Lib

type ActivityMap = HashMap Int Activity
type PersonMap   = HashMap TS.Text DbPerson

routes :: MVar ActivityMap -> MVar PersonMap -> ScottyM ()
routes as ps = do
  get "/" $ html =<< liftIO (TL.pack <$> readFile "web/build/index.html")

  staticRoutes

  -- TODO: Return session cookie that is valid for some time
  post "/login" $ do
    loginReq <- decodeUrlEncode . TL.toStrict . decodeUtf8 <$> body
    flip (maybe (status badRequest400)) loginReq $ \login -> do
      maybeP <- getItemById (loginEmail login) ps
      flip (maybe unauthorized) maybeP $ \p ->
        if checkPassword (loginPassword login) p then json (person p) else unauthorized

  get "/persons" $ do
    persons <- fmap person <$> getItems ps
    json persons

  post "/register" $ do
    newPerson     <- jsonData
    alreadyExists <- isJust <$> getItemById ((email . person) newPerson) ps
    if alreadyExists
      then status badRequest400 >> text "User already exists"
      else do
        persons <- updateItems
          ps
          (pure . M.insert ((email . person) newPerson) newPerson)
        void $ storeState "persons.state" persons
        text "OK"

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

  post "/activity/" $ do
    let addNewActivity :: Activity -> ActivityMap -> ActivityMap
        addNewActivity a am = M.insert (newId am) a am
    newActivity <- jsonData
    activities  <- updateItems as (pure . addNewActivity newActivity)
    void $ storeState "activities.state" activities
    text "OK"

  put "/activity/:activityId" $ do
    activityId <- read <$> param "activityId"
    updatedActivity <- jsonData
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
