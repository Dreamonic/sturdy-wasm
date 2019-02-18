module Generators (
    SafeKeyword (..)
    , genKeyword
    , genNonZero
    , genNonNeg
    , genNegative
    , genNonEmptyString
    , genIDChar
    , genIDString
    , genChar
) where

import Test.QuickCheck
import Test.Hspec
import Tokens
-- Generators --

-- | Generates a random Keyword following specification.
genKeyword :: Gen String
genKeyword = do
    x <- listOf $ elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "!#$%&*+-./" ++ ":<=>?@\\~_|")
    y <- elements ['a' .. 'z']
    return $ y:x

newtype SafeKeyword = SafeKeyword String -- ^ A String starting with [a-z].
    deriving Show

instance Arbitrary SafeKeyword where
    arbitrary = SafeKeyword <$> genKeyword

-- | Generates a random non-zero integer.
genNonZero :: Gen Integer
genNonZero = arbitrary `suchThat` (/= 0)

-- | Generates a random non-negative integer.
genNonNeg :: Gen Integer
genNonNeg = arbitrary `suchThat` (>= 0)

-- | Generate negative integers.
genNegative :: Gen Integer
genNegative = arbitrary `suchThat` (< 0)

-- | Generate a non empty String.
genNonEmptyString :: Gen String
genNonEmptyString = arbitrary `suchThat` (/= "")

-- | Generate a random idchar.
genIDChar :: Gen Char
genIDChar = elements idchar

-- | Generate a random string of idchars.
genIDString :: Gen String
genIDString = listOf genIDChar

-- | Generate a word character
genChar :: Gen Char
genChar = choose ('a', 'z')