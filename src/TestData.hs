{-# LANGUAGE OverloadedStrings #-}
-- | Test module
-- Contains test data for the application.
module TestData where

import Data.HashMap.Strict ( HashMap )
import qualified Data.HashMap.Strict as M

import Types

testActivities :: HashMap Int Activity
testActivities = M.fromList
  [ ( 1
    , Activity
      { title        = "Musikal"
      , description  = "Gå på musikalen Ringaren i Notre Dame"
      , date         = "2018-11-13 09:10"
      , host         = 1
      , attendees    = [2,3]
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
  , ( 3
    , Person
      { name  = "Bolle"
      , email = "bolle@bob.chalmersspexet.se"
      , phone = Nothing
      , hashedPassword = ""
      }
    )
  ]
