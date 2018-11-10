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
  , hashedPassword :: Text
  } deriving (Show, Read, Generic, ToJSON, FromJSON)

instance Eq Person where
  p1 == p2 = email p1 == email p2


type PersonId = Int

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
