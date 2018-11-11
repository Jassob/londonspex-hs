{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A type class implementation of encoding and decoding of values to
-- and from application/x-www-form-urlencoded.
--
-- TODO: Read RFC (https://www.w3.org/TR/html401/interact/forms.html#h-17.13.4.1)
-- and (http://www.ietf.org/rfc/rfc1738.txt)
module UrlEncoded
  ( ToPairs(..)
  , FromPairs(..)
  , UrlEncoded(..)
  , encodeUrl
  , decodeUrl
  , pairs
  , unpairs
  )
where

import           Data.List                      ( foldl' )
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as TS

-- | Converts a value to a list of key-value pairs
class ToPairs a where
  toPairs :: a -> [(Text, Text)]

-- | Attempts to convert a list of key-value pairs to a value
class FromPairs a where
  fromPairs :: [(Text, Text)] -> Maybe a

-- | Provides methods for encoding and decoding to and from
-- application/x-www-form-urlencoded strings.
class UrlEncoded a where
  -- | Attempt to decode a application/x-www-form-urlencoded string to a value
  toUrlEncoded :: a -> Text

  -- | Encode a value as a application/x-www-form-urlencoded string
  fromUrlEncoded :: Text -> Maybe a

instance (ToPairs a, FromPairs a) => UrlEncoded a where
  toUrlEncoded = encodeUrl . unpairs . toPairs
  fromUrlEncoded = fromPairs . pairs . decodeUrl

-- | Builds up a replace function ([from] -> [to]) from a
-- lookup table of [(from, to)]
translateUrl :: ((Text, Text) -> Text -> Text) -> Text -> Text
translateUrl f = foldl' (.) id . fmap f $ urlEncodeCharMap

-- | Replaces urlencoded chars with their plain-text equivalent
decodeUrl :: Text -> Text
decodeUrl = translateUrl ((uncurry . flip) TS.replace)

-- | Replaces occurrences of certain chars with their urlencoded equivalent.
encodeUrl :: Text -> Text
encodeUrl = translateUrl $ uncurry TS.replace

-- | Parses a Text string 'key1=value1&key2=value2' in to a list of
-- (key, value) pairs.
--
-- Note: This function does not check if first character is a '?'
pairs :: Text -> [(Text, Text)]
pairs = fmap ((\[a, b] -> (a, b)) . TS.splitOn "=") . TS.splitOn "&"

-- | Flattens a list of (key, value) pairs into a Text string
-- 'key1=value1&key2=value2'.
--
-- Note: This function does not prepend a '?' character in front.
unpairs :: [(Text, Text)] -> Text
unpairs = foldl' go ""
 where
  go :: Text -> (Text, Text) -> Text
  go acc (key, value) =
    let acc' = if TS.null acc then acc else acc <> "&"
    in  acc' <> key <> "=" <> value

-- | List of key-value pairs that represents which symbols to escape
-- with their URI equivalents.
urlEncodeCharMap :: [(Text, Text)]
urlEncodeCharMap =
  [ ("?", "%3F")
  , ("=", "%3D")
  , ("@", "%40")
  , ("&", "%26")
  , ("{", "%7B")
  , ("}", "%7D")
  , (" ", "%20")
  , ("\"", "%22")
  ]
