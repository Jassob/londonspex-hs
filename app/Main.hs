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
import           Web.Scotty

import           Lib
import           Types
import           Routes

main :: IO ()
main = do
  (activities, persons) <- initState
  scotty 3000 $ routes activities persons

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

testActivities :: HashMap Int Activity
testActivities = M.fromList
  [ ( 1
    , Activity
      { title        = "Musikal"
      , description  = "Gå på musikalen Ringaren i Notre Dame"
      , date         = "2018-11-13 09:10"
      , host         = 1
      , attendees    = [2]
      , meetingPoint = "Nope"
      , editPassword = "nope"
      }
    )
  ]

testPersons :: HashMap Int Person
testPersons = M.fromList
  [ ( 1
    , Person
      { name           = "Jacob"
      , email          = "jassob@bob.chalmersspexet.se"
      , phone          = Nothing
      , hashedPassword = ""
      }
    )
  , ( 2
    , Person
      { name           = "Josefine"
      , email          = "korre@bob.chalmersspexet.se"
      , phone          = Nothing
      , hashedPassword = ""
      }
    )
  ]
