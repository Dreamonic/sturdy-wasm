module SystemSpec (spec) where

import Test.QuickCheck
import Test.Hspec
import Lexer
import Parser
import Executor
import qualified Data.Map as Map

-- Test suites --
spec :: Spec
spec = do
    tsFunctions


-- Linking tests to Test Suites --

tsFunctions = describe "functions" $ do
    testSimpleAddition
    testStep
    testT
    testT2
    testT3
    testT4
    testT5


-- Tests --  

programAdd = "(func $add (param $a i32) (param $b i32) \n\
                \(result i32)\n\
                \get_local $a\n\
                \get_local $b\n\
                \i32.add)"

functionAdd = parseFunc Parser.function programAdd

testSimpleAddition = it "Parse a function which does addition" $
    property $ \(x,y) -> execFunc functionAdd [I32Val (x::Integer), I32Val (y::Integer)] `shouldBe` [I32Val (x+y)]
     
testStep = it "Test the step function for add" $
    step (Config (FrameT EmptyInst Map.empty) (Code [I32Val 1, I32Val 2] [Plain (Numeric (Add I32))])) `shouldBe`
    (Config (FrameT EmptyInst Map.empty) (Code [I32Val 3] []))

programT = "(func $add \n\
            \(result i32)\n\
            \i32.const 2\n\
            \i32.const 3\n\
            \i32.add)"

functionT = parseFunc Parser.function programT

testT = it "Test simple function" $
    execRed functionT `shouldBe` (Config (FrameT EmptyInst Map.empty) (Code [I32Val 5] []))

programT2 = "(func $add (param $a i32)\n\
        \(result i32)\n\
        \get_local $a\n\
        \i32.const 3\n\
        \i32.add)"

functionT2 = parseFunc Parser.function programT2

testT2 = it "Test using local variables" $
    execute (Config (FrameT EmptyInst Map.empty) (Code [I32Val 2] [Invoke (Closure EmptyInst functionT2)]))
    `shouldBe` (Config (FrameT EmptyInst Map.empty) (Code [I32Val 5] []))

programT3 = "(func $add (param $a i32)\n\
    \(result i32)\n\
    \i32.const 1\n\
    \set_local $a\n\
    \get_local $a\n\
    \i32.const 3\n\
    \i32.add)"

functionT3 = parseFunc Parser.function programT3

testT3 = it "Test setting local variables" $
    execute (Config (FrameT EmptyInst Map.empty) (Code [I32Val 3] [Invoke (Closure EmptyInst functionT3)]))
    `shouldBe` (Config (FrameT EmptyInst Map.empty) (Code [I32Val 4] []))

programT4 = "(func $add\n\
    \(result i32)\n\
    \i32.const 1\n\
    \i32.const 1\n\
    \i32.eq)"

functionT4 = parseFunc Parser.function programT4

testT4 = it "Test equals" $
    execute (Config (FrameT EmptyInst Map.empty) (Code [] [Invoke (Closure EmptyInst functionT4)]))
    `shouldBe` (Config (FrameT EmptyInst Map.empty) (Code [I32Val 1] []))

programT5 = "(func $add\n\
    \(result i32)\n\
    \i32.const 1\n\
    \i32.const 3\n\
    \i32.eq)"

functionT5 = parseFunc Parser.function programT5

testT5 = it "Test equals" $
    execute (Config (FrameT EmptyInst Map.empty) (Code [] [Invoke (Closure EmptyInst functionT5)]))
    `shouldBe` (Config (FrameT EmptyInst Map.empty) (Code [I32Val 0] []))
