{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Concurrent.MVar        ( MVar
                                                , newMVar
                                                , modifyMVar
                                                , readMVar
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as M
import           Data.Maybe                     ( fromMaybe
                                                , isNothing
                                                )
import           Data.Monoid                    ( mempty )
import           Data.Text.Lazy                 ( )
import           Network.HTTP.Types.Status      ( notFound404 )
import           Network.Wai.Middleware.Cors    ( simpleCors )
import           Web.Scotty                     ( scotty
                                                , middleware )

import           Lib
import           Types
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
initState :: MonadIO m => m (MVar (HashMap Int Activity), MVar (HashMap Int Person))
initState = do
  activities <- fromMaybe mempty <$> readState "activities.state"
  persons    <- fromMaybe mempty <$> readState "persons.state"
  aMVar    <- liftIO $ newMVar activities
  pMVar    <- liftIO $ newMVar persons
  pure (aMVar, pMVar)
