module TestPrograms.TypeCheckerTests
where

import Test.QuickCheck
import Test.Hspec

import TestPrograms.Programs

import Syntax
import Types
import Interp.Util (CheckType)
import Generators

allTests :: [CheckType -> SpecWith ()]
allTests = [testSimpleFunction, testReadLocalVars, testSetLocalVars, testEquals,
            testEqualsFalse, testBlock, testBranch, testBranchIfTrue,
            testBranchIfFalse, testTee, testLoop, testIfElse, testNestedBlocks,
            testFunctionCalls, testEvenOdd, testIntsAndFloats,
            testPreciseIntsAndFloats]

testSimpleFunction checkFunc = it "Test simple function" $
    checkFunc "$add" [] programSimpleFunction `shouldBe` (Right [I32])

testReadLocalVars checkFunc = it "Test using local variables" $
    checkFunc "$add" [I32] programReadLocalVars `shouldBe` (Right [I32])

testSetLocalVars checkFunc = it "Test setting local variables" $
    checkFunc "$add" [I32] programSetLocalVars `shouldBe` (Right [I32])

testEquals checkFunc = it "Test equals" $
    checkFunc "$eq" [] programEquals `shouldBe` (Right [I32])

testEqualsFalse checkFunc = it "Test equals false case" $
    checkFunc "$neq" [] programEqualsFalse `shouldBe` (Right [I32])

testBlock checkFunc = it "Test block" $
    checkFunc "$bl" [I32] programBlock `shouldBe` (Right [I32])

testBranch checkFunc = it "Test branch" $
    checkFunc "$foo" [I32] programBranch `shouldBe` (Right [I32])

testBranchIfTrue checkFunc = it "Test branchIf true case" $
    checkFunc "$foo" [I32] programBranchIf `shouldBe` (Right [I32])

testBranchIfFalse checkFunc = it "Test branchIf false case" $
    checkFunc "$foo" [I32] programBranchIf `shouldBe` (Right [I32])

testTee checkFunc = it "Test setting local variables with tee" $
    checkFunc "$foo" [I32] programTee `shouldBe` (Right [I32])

testLoop checkFunc = it "Test loop" $
    checkFunc "$foo" [I32] programLoop `shouldBe` (Right [I32])

testIfElse checkFunc = it "If else test" $
    checkFunc "$foo" [I32] programIfElse `shouldBe` (Right [I32])

testNestedBlocks checkFunc = it "Test nested blocks" $
    checkFunc "$foo" [I32] programNestedBlocks `shouldBe` (Right [I32])

testFunctionCalls checkFunc = it "Test function calls" $
    checkFunc "$quadruple" [I32] programFunctionCalls `shouldBe` (Right [I32])

testEvenOdd checkFunc = it "Test complex function calls" $
    checkFunc "$even" [I32] programEvenOdd `shouldBe` (Right [I32])

testIntsAndFloats checkFunc = it "Test program using I32 and f32" $
    checkFunc "$foo" [I32, F32] programIntsAndFloats `shouldBe` (Right [F32])

testPreciseIntsAndFloats checkFunc = it "Test program using I64 and f64" $
    checkFunc "$foo" [F64, I64] programPreciseIntsAndFloats `shouldBe` (Right
        [F64])
