{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass#-}

-- | Types module
--
-- Contains various types used in the project.

module Types where

import           Data.Aeson.Types               ( ToJSON
                                                , FromJSON
                                                )
import           Data.Text
import           GHC.Generics

-- | A Person represents an actual person that can either participate
-- in activities or host activities.
data Person = Person
  { name :: Text
  , email :: Text
  , phone :: Maybe Text
  } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

-- | A DbPerson represents the Database view of a person, it consists
-- of a person and a password.
data DbPerson = DbPerson
  { person :: Person
  , hashedPassword :: Text
  } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)

-- | A JSON payload sent from the frontend when logging in
data LoginPayload = LoginPayload
  { loginEmail :: Text
  , loginPassword :: Text
  } deriving (Show, Read, Generic, ToJSON, FromJSON)

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
  } deriving (Show, Read, Eq, Generic, ToJSON, FromJSON)
