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
    describe "tokenizeAll" $ do
        it "Lexing a module" $
            tokenizeAll "(module (memory 1) (func))" `shouldBe`
            [LP, Keyword "module", LP, Keyword "memory", UnsignedN 1, RP, LP, Keyword "func", RP, RP]
        it "Lexing a function" $
            tokenizeAll "(func (param i32) (param f32) (local f64) get_local 0 get_local 1 get_local 2)" `shouldBe`
            [LP, Keyword "func", LP, Keyword "param", Keyword "i32", RP, LP, Keyword "param", Keyword "f32",RP,LP,Keyword "local",Keyword "f64",RP,Keyword "get_local",UnsignedN 0, Keyword "get_local",UnsignedN 1, Keyword "get_local",UnsignedN 2,RP]

genKeyword :: Gen String
genKeyword = do
    x <- listOf $ elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "!#$%&*+-./" ++ ":<=>?@\\~_|")
    y <- elements ['a' .. 'z']
    return $ y:x

newtype SafeKeyword = SafeKeyword String
    deriving Show

instance Arbitrary SafeKeyword where
    arbitrary = SafeKeyword <$> genKeyword