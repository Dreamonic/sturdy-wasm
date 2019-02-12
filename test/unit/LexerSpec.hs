module LexerSpec (spec) where

import Test.QuickCheck
import Test.Hspec
import Lexer

spec :: Spec
spec = do
    describe "tokenizeString" $ do
        it "A value encapsulated in \"\" should be a Str" $ 
            property $ \x -> tokenizeString ("\"" ++ (x::String) ++ "\"") `shouldBe` Str x
        it "A value starting with $ should be an ID" $ 
            property $ \x -> tokenizeString ('$':(x::String)) `shouldBe` ID x
        it "A value starting with a-z should be a Keyword" $
            property $ \(SafeKeyword str) -> tokenizeString str `shouldBe` Keyword str

genKeyword :: Gen String
genKeyword = do
    x <- listOf $ elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "!#$%&*+-./" ++ ":<=>?@\\~_|")
    y <- elements ['a' .. 'z']
    return $ y:x

newtype SafeKeyword = SafeKeyword String
    deriving Show

instance Arbitrary SafeKeyword where
    arbitrary = SafeKeyword <$> genKeyword