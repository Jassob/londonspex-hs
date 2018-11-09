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
import           Data.List                      ( foldl' )
import           Data.Text.Lazy                 ( Text, pack, unpack )
import           Web.Scotty                     ( ScottyM
                                                , ActionM
                                                , capture
                                                , raise
                                                , get
                                                , post
                                                , put
                                                , html
                                                , file
                                                , param
                                                , status
                                                , text
                                                , json
                                                , jsonData
                                                , setHeader
                                                )
import           Network.HTTP.Types             ( notFound404 )
import           Types

type ActivityMap = HashMap Int Activity
type PersonMap   = HashMap Int Person

routes :: MVar ActivityMap -> MVar PersonMap -> ScottyM ()
routes as ps = do
  get "/" $ html =<< liftIO (pack <$> readFile "web/build/index.html")

  staticRoutes

  get "/persons" $ do
    persons <- getItems ps
    json persons

  post "/person" $ do
    let insertIfNotFound :: Person -> PersonMap -> PersonMap
        insertIfNotFound p pm
          | M.null $ M.filter (== p) pm = M.insert (newId pm) p pm
          | otherwise                   = pm
    newPerson <- jsonData
    persons <- updateItems ps (pure . insertIfNotFound newPerson)
    void $ storeState "persons.state" persons
    text "OK"

  get "/person/:personId" $ do
    personId <- param "personId"
    person   <- getItemById personId ps
    maybe notFound json person

  get "/activities" $ do
    activities <- getItems as
    persons    <- getItems ps
    json (activities, persons)

  get "/activity/:activityId" $ do
    activityId <- read <$> param "activityId"
    persons    <- getItems ps
    activity   <- getItemById activityId as
    flip (maybe notFound) activity $ \act ->
      let actHost      = M.lookup (host act) persons
          actAttendees = map (`M.lookup` persons) (attendees act)
      in  json (act, actHost, actAttendees)

  post "/activity/" $ do
    let addNewActivity :: Activity -> ActivityMap -> ActivityMap
        addNewActivity a am = M.insert (newId am) a am
    newActivity <- jsonData
    activities <- updateItems as (pure . addNewActivity newActivity)
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
defineStaticRoute type' = get (capture $ "/static/" <> type' <> "/:resource") $ do
  resource <- param "resource"
  setHeader "Content-Type"  =<< contentType type'
  file ("web/build/static/js/" <> resource)
  where contentType "css" = pure "text/css"
        contentType "js"  = pure "application/javascript"
        contentType "img" = pure "image/png"
        contentType _     = raise "defineStaticRoute: not a defined Content-Type"


notFound :: ActionM ()
notFound = status notFound404 >> text "404 Not found"

updateActivity :: Int -> Activity -> ActivityMap -> ActivityMap
updateActivity = M.insert

getAttendees :: [Int] -> PersonMap -> PersonMap
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
