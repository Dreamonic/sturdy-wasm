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
    testStep
    testT
    testT2
    testT3
    testT4
    testT5
    testT6
    testT7
    -- testPrint


-- Tests --  


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
    eval (Config (FrameT EmptyInst Map.empty) (Code [I32Val 2] [Invoke (Closure EmptyInst functionT2)]))
    `shouldBe` [I32Val 5]

programT3 = "(func $add (param $a i32)\n\
    \(result i32)\n\
    \i32.const 1\n\
    \set_local $a\n\
    \get_local $a\n\
    \i32.const 3\n\
    \i32.add)"

functionT3 = parseFunc Parser.function programT3

testT3 = it "Test setting local variables" $
    eval (Config (FrameT EmptyInst Map.empty) (Code [I32Val 3] [Invoke (Closure EmptyInst functionT3)]))
    `shouldBe` [I32Val 4]

programT4 = "(func $add\n\
    \(result i32)\n\
    \i32.const 1\n\
    \i32.const 1\n\
    \i32.eq)"

functionT4 = parseFunc Parser.function programT4

testT4 = it "Test equals" $
    eval (Config (FrameT EmptyInst Map.empty) (Code [] [Invoke (Closure EmptyInst functionT4)]))
    `shouldBe` [I32Val 1]

programT5 = "(func $add\n\
    \(result i32)\n\
    \i32.const 1\n\
    \i32.const 3\n\
    \i32.eq)"

functionT5 = parseFunc Parser.function programT5

testT5 = it "Test equals" $
    eval (Config (FrameT EmptyInst Map.empty) (Code [] [Invoke (Closure EmptyInst functionT5)]))
    `shouldBe` [I32Val 0]


programT7 = "(func $add (param $a i32)\n\
    \(result i32)\n\
    \(block\n\
    \i32.const 2\n\
    \set_local $a\n\
    \br 0\n\
    \i32.const 1\n\
    \set_local $a)\n\
    \get_local $a\n\
    \i32.const 3\n\
    \i32.add)"

functionT7 = parseFunc Parser.function programT7

testT7 = it "Test block" $
    eval (Config (FrameT EmptyInst Map.empty) (Code [I32Val 3] [Invoke (Closure EmptyInst functionT7)]))
    `shouldBe` [I32Val 5]


programT6 = "(func $add (param $x i32) (result i32)\n\
    \(block\n\
    \(loop\n\
        \get_local $x\n\
        \i32.const 1\n\
        \i32.add\n\
        \set_local $x\n\
        \get_local $x\n\
        \i32.const 4\n\
        \i32.eq\n\
        \br_if 1\n\
        \br 0\n\
    \)\n\
    \)\n\
    \get_local $x\n\
    \)"

functionT6 = do 
    parseFunc Parser.function programT6

testPrint = it "Print" $ functionT6 `shouldBe` Func "" [] (Block [] [])

testT6 = it "Loop test" $
    eval (Config (FrameT EmptyInst Map.empty) (Code [I32Val 0] [Invoke (Closure EmptyInst functionT6)]))
    `shouldBe` [I32Val 4]