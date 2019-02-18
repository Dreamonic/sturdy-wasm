module LexerParserSpec (spec) where

import Test.QuickCheck
import Test.Hspec
import Lexer
import Parser

-- Test suites --
spec :: Spec
spec = do
    tsFunctions


-- Linking tests to Test Suites --

tsFunctions = describe "functions" $ do
    testSimpleAddition


-- Tests --  

programAdd = "(func $add (param $a i32) (param $b i32) \n\
                \(result i32)\n\
                \get_local $a\n\
                \get_local $b\n\
                \i32.add)"

testSimpleAddition = it "Parse a function which does addition" $
    parse Parser.function (tokenize programAdd) `shouldBe` 
    [(Func "add" [Param "a" I32,Param "b" I32] (Block [Result I32] [LocalGet "a",LocalGet "b",Numeric (Add I32)]),[])]