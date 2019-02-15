module ExecutorSpec (spec) where

import Test.QuickCheck
import Test.Hspec
import Parser
import Executor
import Control.Exception.Base

-- Test suites --

spec :: Spec
spec = do
    tsExecFunc

-- Linking tests to Test Suites --

tsExecFunc :: Spec
tsExecFunc = describe "execFunc" $ do
    testExecFunc
    testExecAddition
    testExecSubstraction
    testExecMultiplication
    --testExecDivision
    --testExecDivByZero
    

-- Tests --
foo = Func "foo" [Param "a" I32, Param "b" I32, Param "c" I32,
    Param "d" I32] (Block [Result I32] [
    LocalGet "a",
    LocalGet "b",
    Numeric (Div I32 Signed),
    LocalGet "c",
    LocalGet "d",
    Numeric (Mul I32),
    Numeric (Sub I32)
    ])

testExecFunc = it "Test some function with multiple math operators" $ 
    execFunc foo [I32Val 8, I32Val 2, I32Val 5, I32Val 7] `shouldBe` [I32Val (-31)]

addition = Func "add" [Param "a" I32, Param "b" I32]
    (Block [Result I32] [
        LocalGet "a",
        LocalGet "b",
        Numeric (Add I32)
    ])

testExecAddition = it "Test addition with 2 values" $
    property $ \(x,y) -> execFunc addition [I32Val (x::Integer), I32Val (y::Integer)] `shouldBe` [I32Val (x+y)]

substraction = Func "sub" [Param "a" I32, Param "b" I32]
    (Block [Result I32] [
        LocalGet "a",
        LocalGet "b",
        Numeric (Sub I32)
    ])

testExecSubstraction = it "Test substraction with 2 values" $
    property $ \(x,y) -> execFunc substraction [I32Val (x::Integer), I32Val (y::Integer)] `shouldBe` [I32Val (x-y)]

multiplication = Func "mul" [Param "a" I32, Param "b" I32]
    (Block [Result I32] [
        LocalGet "a",
        LocalGet "b",
        Numeric (Mul I32)
    ])

testExecMultiplication = it "Test multiplication with 2 values" $
    property $ \(x,y) -> execFunc multiplication [I32Val (x::Integer), I32Val (y::Integer)] `shouldBe` [I32Val (x*y)]


division = Func "div" [Param "a" I32, Param "b" I32]
    (Block [Result I32] [
        LocalGet "a",
        LocalGet "b",
        Numeric (Div I32 Signed)
    ])


testExecDivision = it "Test division with 2 values" $
    forAll arbitrary $ \x -> forAll genNonZero $ \y -> execFunc division [I32Val x, I32Val y] `shouldBe` [I32Val (quot x y)]

--testExecDivByZero = it "Test division by 0" $
--   property $ \x -> execFunc division [I32Val (x::Integer), I32Val 0] `shouldThrow` DivideByZero

-- Generators --

-- Generates a random Keyword following specification.
genNonZero :: Gen Integer
genNonZero = arbitrary `suchThat` (/= 0)