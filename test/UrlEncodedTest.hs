{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for UrlDecode
module UrlEncodedTest where

import           Data.Text
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range

import           UrlEncoded

tests :: IO Bool
tests = checkParallel $$discover

prop_urlDecodeId :: Property
prop_urlDecodeId = property $ do
  str <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
  decodeUrl (encodeUrl str) === str

prop_pairsId :: Property
prop_pairsId = property $ do
  a <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
  b <- forAll $ Gen.text (Range.linear 0 100) Gen.unicode
  let p = PTC a b
  (fromPairs . toPairs) p === Just p

data PairTestCase = PTC
  { key1 :: Text
  , key2 :: Text
  } deriving (Show, Eq)

instance ToPairs PairTestCase where
  toPairs (PTC a b) = [("key1", a), ("key2",  b)]

instance FromPairs PairTestCase where
  fromPairs pairs = do
    a <- lookup "key1" pairs
    b <- lookup "key2" pairs
    return (PTC a b)
