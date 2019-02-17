module LexerSpec (spec) where

import Test.QuickCheck
import Test.Hspec
import Generators
import Tokens
import LexerRegex

-- Test suites --

spec :: Spec
spec = do 
    tstokenizeS
    tsTokenize


-- Linking tests to Test Suites --

tstokenizeS :: Spec
tstokenizeS = describe "tokenizeS" $ do
    testtokenizeSKeyword
    testtokenizeSUnsignedN
    testtokenizeSSignedN
    testtokenizeSFloatN
    testtokenizeSStr
    testtokenizeSID
    testtokenizeSLP
    testtokenizeSRP
    testtokenizeSNumberWithChar
    testtokenizeSNumberEndingStr
    testtokenizeSInvalidKeywordLB
    testtokenizeSInvalidKeywordRB

tsTokenize :: Spec
tsTokenize = describe "tokenize" $ do
    tsTokenizeAllModule
    tsTokenizeAllFunction


-- Tests --

--- tokenizeS ---

testtokenizeSStr = it "A value encapsulated in \"\" should be a Str" $ 
    property $ \x -> tokenizeS ("\"" ++ (x::String) ++ "\"") `shouldBe` Str x

testtokenizeSID = it "A value starting with $ should be an ID" $ 
    forAll genKeyword $ \x -> tokenizeS ('$':x) `shouldBe` ID x

testtokenizeSKeyword = it "A value starting with a-z should be a Keyword" $
    property $ \(SafeKeyword str) -> tokenizeS str `shouldBe` Keyword str

testtokenizeSUnsignedN = it "A value existing of only [0-9] should be an UnsignedN" $
    forAll genNonNeg $ \x -> tokenizeS (show (x::Integer)) `shouldBe` UnsignedN x

testtokenizeSSignedN = it "A value starting with a - followed by only [0-9] values should be a SignedN" $
    forAll genNegative $ \x -> tokenizeS (show (x::Integer)) `shouldBe` SignedN x

testtokenizeSFloatN = it "A float value should be a FloatN" $
    property $ \x -> forAll genNonNeg $ \y -> tokenizeS (show (x::Integer) ++ '.' : show y) `shouldBe` FloatN (read (show x ++ '.' : show y))

testtokenizeSLP = it "Tokenize a left paranthesis" $
    tokenizeS "(" `shouldBe` LP

testtokenizeSRP = it "Tokenize a right paranthesis" $
    tokenizeS ")" `shouldBe` RP

testtokenizeSNumberWithChar = it "A number cannot have a char in the middle" $
    property $ \(x, z) -> forAll genChar $ \y -> tokenizeS (show (x::Integer) ++ (y::Char) : show (z::Integer)) `shouldBe` Reserved (show x ++ y : show z)

testtokenizeSNumberEndingStr = it "A number cannot have a char appended" $
    forAll arbitrary $ \x -> forAll genChar $ \y -> tokenizeS (show (x::Integer) ++ [y]) `shouldBe` Reserved (show x ++ [y])

testtokenizeSInvalidKeywordLB = it "A keyword cannot contain '{'" $
    property $ \(SafeKeyword x, y) -> tokenizeS (x ++ '{' : y::String) `shouldBe` Reserved (x ++ '{' : y)

testtokenizeSInvalidKeywordRB = it "A keyword cannot contain '}'" $
    property $ \(SafeKeyword x, y) -> tokenizeS (x ++ '}' : y::String) `shouldBe` Reserved (x ++ '}' : y)


    --- tokenize ---

tsTokenizeAllModule = it "Lexing a module" $
    tokenize "(module (memory 1) (func))" `shouldBe`
    [LP, Keyword "module", LP, Keyword "memory", UnsignedN 1, RP, LP, Keyword "func", RP, RP]

tsTokenizeAllFunction = it "Lexing a function" $
    tokenize "(func (param i32) (param f32) (local f64)\nget_local 0\nget_local 1\nget_local 2)" `shouldBe`
    [LP, Keyword "func", LP, Keyword "param", Keyword "i32", RP, LP, Keyword "param", Keyword "f32",RP,LP,Keyword "local",Keyword "f64",RP,
    Keyword "get_local",UnsignedN 0, Keyword "get_local",UnsignedN 1, Keyword "get_local",UnsignedN 2,RP]
