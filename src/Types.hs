{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types module
--
-- Contains various types used in the project.

module Types where

import           Prelude                 hiding ( fail )
import           Data.Aeson.Types               ( ToJSON
                                                , FromJSON
                                                )
import           Data.Text
import           GHC.Generics

import           UrlEncoded

-- | A Person represents an actual person that can either participate
-- in activities or host activities.
data Person = Person
  { name :: Text
  , email :: Text
  , phone :: Maybe Text
  } deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)

-- | A DbPerson represents the Database view of a person, it consists
-- of a person and a password.
data DbPerson = DbPerson
  { person :: Person
  , hashedPassword :: Text
  } deriving (Show, Read, Generic, ToJSON, FromJSON)

instance ToPairs DbPerson where
  toPairs (DbPerson (Person n e ph) pwd) =
    maybe [] ((: []) . (,) "phone") ph ++
      [ ("name", n)
      , ("email", e)
      , ("password", pwd)
      ]

instance FromPairs DbPerson where
  fromPairs ps = do
    let ph = lookup "phone" ps
    n <- lookup "name" ps
    e <- lookup "email" ps
    pwd <- lookup "password" ps
    pure $ DbPerson (Person n e ph) pwd

-- | A JSON payload sent from the frontend when logging in
data LoginPayload = LoginPayload
  { loginEmail :: Text
  , loginPassword :: Text
  } deriving (Show, Read, Generic, FromJSON, ToJSON)

instance ToPairs LoginPayload where
  toPairs (LoginPayload e p) = [("loginEmail", e), ("loginPassword", p)]

instance FromPairs LoginPayload where
  fromPairs ps = do
    e <- lookup "loginEmail" ps
    p <- lookup "loginPassword" ps
    pure $ LoginPayload e p

type PersonId = Text

-- | An Activity represents an event that is hosted by one Person
data Activity = Activity
  { title :: Text
  , description :: Text
  , date :: Text
  , host :: PersonId
  , attendees :: [PersonId]
  , meetingPoint :: Text
  , editPassword :: Text
  } deriving (Show, Read, Generic, FromJSON, ToJSON)

instance ToPairs Activity where
  toPairs a =
    [ ("title", title a)
    , ("description", description a)
    , ("date", date a)
    , ("host", host a)
    , ("meetingPoint", meetingPoint a)
    , ("editPassword", editPassword a)
    ]

instance FromPairs Activity where
  fromPairs ps = do
    t <- lookup "title" ps
    desc <- lookup "description" ps
    d <- lookup "date" ps
    h <- lookup "host" ps
    as <- lookup "attendees" ps
    mp <- lookup "meetingPoint" ps
    pwd <- lookup "editPassword" ps
    pure $ Activity t desc d h (read (unpack as)) mp pwd
