module Lib where

import           Control.Exception              ( IOException
                                                , handle
                                                )
import           Control.Monad.IO.Class         ( MonadIO
                                                , liftIO
                                                )
import           Crypto.PasswordStore           ( verifyPassword )
import           Data.Aeson                     ( ToJSON
                                                , FromJSON
                                                , encodeFile
                                                , decodeFileStrict
                                                )
import           Data.Text                      ( Text
                                                , splitOn
                                                , replace
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )

import           Types

storeState :: (MonadIO m, ToJSON a, FromJSON a) => FilePath -> a -> m Bool
storeState fp a = liftIO . handle handler $ encodeFile fp a >> pure True
 where
  handler :: IOException -> IO Bool
  handler _ = pure False

readState :: (MonadIO m, ToJSON a, FromJSON a) => FilePath -> m (Maybe a)
readState fp = liftIO . handle handler $ decodeFileStrict fp
 where
  handler :: IOException -> IO (Maybe a)
  handler = const $ pure Nothing

checkPassword :: Text -> Person -> Bool
checkPassword pwd = verifyPassword (encodeUtf8 pwd) . encodeUtf8 . hashedPassword
