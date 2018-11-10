{-# LANGUAGE OverloadedStrings #-}
-- | Test module
-- Contains test data for the application.
module TestData where

import Data.HashMap.Strict ( HashMap, fromList )
import Data.Text           ( Text )

import Types
import Lib

testActivities :: HashMap Int Activity
testActivities = fromList
  [ ( 1
    , Activity
      { title        = "Musikal"
      , description  = "Gå på musikalen Ringaren i Notre Dame"
      , date         = "2018-11-13 09:10"
      , host         = "jassob@bob.chalmersspexet.se"
      , attendees    = [ "korre@bob.chalmersspexet.se"
                       , "bolle@bob.chalmersspexet.se"
                       ]
      , meetingPoint = "Nope"
      , editPassword = "nope"
      }
    )
  ]

testPersons :: HashMap Text DbPerson
testPersons = fromList
  [ ( "jassob@bob.chalmersspexet.se"
    , DbPerson
      { person = Person
        { name           = "Jacob"
        , email          = "jassob@bob.chalmersspexet.se"
        , phone          = Nothing
        }
      , hashedPassword = "sha256|17|+He3kGVjqBWfZDlSGBNT4g==|27jUKfrUnmPcu5wUBegpnFd8V+FnDc0SoDEAcsFDtd8="
      }
    )
  , ( "korre@bob.chalmersspexet.se"
    , DbPerson
      { person = Person
        { name           = "Josefine"
        , email          = "korre@bob.chalmersspexet.se"
        , phone          = Nothing
        }
      , hashedPassword = ""
      }
    )
  , ( "bolle@bob.chalmersspexet.se"
    , DbPerson
      { person = Person
        { name  = "Bolle"
        , email = "bolle@bob.chalmersspexet.se"
        , phone = Nothing
        }
      , hashedPassword = ""
      }
    )
  ]

testLogin :: LoginPayload
testLogin = LoginPayload
  { loginEmail = "jassob@bob.chalmersspexet.se"
  , loginPassword = "jonsson"
  }
