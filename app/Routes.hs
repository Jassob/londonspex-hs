{-# LANGUAGE OverloadedStrings #-}

-- | Routes module
--
-- Contains the routing logic for the application.
-- TODO: Reorganize this module

module Routes where

import           Control.Concurrent.MVar        ( MVar, readMVar, modifyMVar_ )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
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

routes :: MVar (HashMap Int Activity) -> MVar (HashMap Int Person) -> ScottyM ()
routes as ps = do
  get "/" $ html =<< liftIO (pack <$> readFile "web/build/index.html")

  staticRoutes

  get "/static/js/:resource" $ do
    resource <- param "resource"
    setHeader "Content-Type" "application/javascript"
    file ("web/build/static/js/" ++ resource)

  get "/static/css/:resource" $ do
    resource <- param "resource"
    setHeader "Content-Type" "text/css"
    file ("web/build/static/css/" ++ resource)

  get "/register" $ do
    undefined

  get "/activities" $ do
    activities <- liftIO (readMVar as)
    persons <- liftIO (readMVar ps)
    json (activities, persons)

  get "/activity/:activityId" $ do
    activityId <- read <$> param "activityId"
    activities <- liftIO $ readMVar as
    persons <- liftIO $ readMVar ps
    maybe notFound json $ getActivity activityId activities persons

  post "/activity/" $ do
    request <- jsonData :: ActionM Activity
    json request

  put "/activity/:activityId" $ do
    activityId <- read <$> param "activityId"
    activities <- liftIO $ readMVar as
    persons <- liftIO $ readMVar ps
    updatedActivity <- jsonData :: ActionM Activity
    let activities' = updateActivity activityId updatedActivity activities
    liftIO $ modifyMVar_ as (const $ pure activities')


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

getActivity :: Int -> ActivityMap -> PersonMap -> Maybe (Activity, Person, PersonMap)
getActivity activityId activities persons = do
  activity <- M.lookup activityId activities
  host <- M.lookup (host activity) persons
  pure (activity, host, getAttendees (attendees activity) persons)

notFound :: ActionM ()
notFound = status notFound404 >> text "404 Not found"

updateActivity :: Int -> Activity -> ActivityMap -> ActivityMap
updateActivity activityId updated as = M.insert activityId updated as


getAttendees :: [Int] -> PersonMap -> PersonMap
getAttendees ids persons = M.filterWithKey (\pid _ -> pid `elem` ids) persons
