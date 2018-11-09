{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types module
--
-- Contains various types used in the project.

module Types where

import           Data.Aeson.Types               ( ToJSON(..)
                                                , FromJSON(..)
                                                , genericToEncoding
                                                , defaultOptions
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
  } deriving (Show, Read, Generic)

instance Eq Person where
  p1 == p2 = email p1 == email p2


instance ToJSON Person where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Person

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
  } deriving (Show, Read, Eq, Generic)

instance ToJSON Activity where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Activity
