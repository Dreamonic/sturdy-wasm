module InstructionSpec.RelationalSpec(spec) where

import Test.QuickCheck
import Test.Hspec
import Parser
import Instruction.Relational
import Generators
import qualified Data.Map as Map

-- Test suites --

spec :: Spec
spec = do
    tsEquals
    tsNotEquals

-- Linking tests to Test Suites --

tsEquals :: Spec
tsEquals = describe ".eq" $ do
    testEqualsEqual
    testEqualsNotEqual
    testEquals
    testEqualsStackContext
    testEqualsHeapContext

tsNotEquals :: Spec
tsNotEquals = describe ".ne" $ do
    testNotEqualsEqual
    testNotEqualsNotEqual
    testNotEquals
    testNotEqualsStackContext
    testNotEqualsHeapContext


-- Tests --

testEqualsEqual = it "Equal integer values should result in a 'I32Val 1'" $
    property $ \x -> 
        execRelational 
            (Numeric (Eql I32)) 
            [I32Val (x::Integer), I32Val (x::Integer)]
            (Map.fromList []) 
    `shouldBe` 
        ([I32Val 1], Map.fromList [])

testEqualsNotEqual = it "1 /= 0" $
    execRelational
        (Numeric (Eql I32)) 
        [I32Val 1, I32Val 0]
        (Map.fromList []) 
    `shouldBe`
        ([I32Val 0], Map.fromList [])

testEquals = it "Equals on random values" $
    property $ \(x,y) -> 
        execRelational 
            (Numeric (Eql I32)) 
            [I32Val (x::Integer), I32Val (y::Integer)]
            (Map.fromList []) 
    `shouldBe` 
        ([I32Val (toBool (x==y))], Map.fromList [])

testEqualsStackContext = it "Equals method should not influence stack members, other than top 2" $
    property $ \(x,y) -> 
    forAll (listOf genWasmVal) $ \z ->
        execRelational 
            (Numeric (Eql I32)) 
            ([I32Val (x::Integer), I32Val (y::Integer)] ++ z)
            (Map.fromList []) 
        `shouldBe` 
            (I32Val (toBool (x == y)) : z, Map.fromList [])

testEqualsHeapContext = it "Equals method should not influence heap members" $
    property $ \(x,y) -> 
        execRelational 
            (Numeric (Eql I32)) 
            [I32Val (x::Integer), I32Val (y::Integer)]
            (Map.fromList [("a", I32Val 0)]) 
        `shouldBe` 
            ([I32Val (toBool (x == y))], Map.fromList [("a", I32Val 0)])

testNotEqualsEqual = it "Equal integer values should result in a 'I32Val 1'" $
    property $ \x -> 
        execRelational 
            (Numeric (Ne I32)) 
            [I32Val (x::Integer), I32Val (x::Integer)]
            (Map.fromList []) 
    `shouldBe` 
        ([I32Val 0], Map.fromList [])

testNotEqualsNotEqual = it "1 /= 0" $
    execRelational
        (Numeric (Ne I32)) 
        [I32Val 1, I32Val 0]
        (Map.fromList []) 
    `shouldBe`
        ([I32Val 1], Map.fromList [])

testNotEquals = it "Equals on random values" $
    property $ \(x,y) -> 
        execRelational 
            (Numeric (Ne I32)) 
            [I32Val (x::Integer), I32Val (y::Integer)]
            (Map.fromList []) 
    `shouldBe` 
        ([I32Val (toBool (x/=y))], Map.fromList [])

testNotEqualsStackContext = it "Equals method should not influence stack members, other than top 2" $
    property $ \(x,y) -> 
    forAll (listOf genWasmVal) $ \z ->
        execRelational 
            (Numeric (Ne I32)) 
            ([I32Val (x::Integer), I32Val (y::Integer)] ++ z)
            (Map.fromList []) 
        `shouldBe` 
            (I32Val (toBool (x /= y)) : z, Map.fromList [])

testNotEqualsHeapContext = it "Equals method should not influence heap members" $
    property $ \(x,y) -> 
        execRelational 
            (Numeric (Ne I32)) 
            [I32Val (x::Integer), I32Val (y::Integer)]
            (Map.fromList [("a", I32Val 0)]) 
        `shouldBe` 
            ([I32Val (toBool (x /= y))], Map.fromList [("a", I32Val 0)])

-- Utility method --

toBool :: Bool -> Integer
toBool cond = if cond then 1 else 0