{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Concurrent.MVar        ( MVar
                                                , newMVar
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.Maybe                     ( fromMaybe )
import           Data.Monoid                    ( mempty )
import           Data.Text.Lazy                 ( )
import           Network.Wai.Middleware.Cors    ( simpleCors )
import           Web.Scotty                     ( scotty
                                                , middleware
                                                )

import           Lib
import           Routes

main :: IO ()
main = do
  (activities, persons) <- initState
  scotty 3000 $ do
    middleware simpleCors
    routes activities persons

-- | Reads the last state.
--
-- Reads activities from "activities.state" and persons from
-- "persons.state" and stores each state in a MVar.
--
-- If these for some reason are not readable it
-- returns an empty structure.
initState :: MonadIO m => m (MVar ActivityMap, MVar PersonMap)
initState = do
  activities <- fromMaybe mempty <$> readState "activities.state"
  persons    <- fromMaybe mempty <$> readState "persons.state"
  aMVar      <- liftIO $ newMVar activities
  pMVar      <- liftIO $ newMVar persons
  pure (aMVar, pMVar)
