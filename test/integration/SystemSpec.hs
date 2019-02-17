module SystemSpec (spec) where

import Test.QuickCheck
import Test.Hspec
import Lexer
import Parser
import Executor

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

functionAdd = fst $ head $ parse Parser.function (tokenize programAdd)

testSimpleAddition = it "Parse a function which does addition" $
    property $ \(x,y) -> execFunc functionAdd [I32Val (x::Integer), I32Val (y::Integer)] `shouldBe` [I32Val (x+y)]
     