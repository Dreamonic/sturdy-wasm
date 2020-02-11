module TestPrograms.ConcreteTests
where

import Test.QuickCheck
import Test.Hspec

import TestPrograms.Programs

import Syntax
import Types
import Interp.Util (ExecType(..))
import Generators

allTests :: [ExecType -> SpecWith ()]
allTests = [testSimpleFunction, testReadLocalVars, testSetLocalVars, testEquals,
            testEqualsFalse, testBlock, testBranch, testBranchIfTrue,
            testBranchIfFalse, testTee, testLoop, testIfElse, testNestedBlocks,
            testFunctionCalls, testEvenOdd]

testSimpleFunction execFunc = it "Test simple function" $
    execFunc "$add" [] programSimpleFunction `shouldBe` (Right [I32Val 5])

testReadLocalVars execFunc = it "Test using local variables" $
    execFunc "$add" [I32Val 2] programReadLocalVars `shouldBe` (Right
        [I32Val 5])

testSetLocalVars execFunc = it "Test setting local variables" $
    execFunc "$add" [I32Val 3] programSetLocalVars `shouldBe` (Right
        [I32Val 4])

testEquals execFunc = it "Test equals" $
    execFunc "$eq" [] programEquals `shouldBe` (Right [I32Val 1])

testEqualsFalse execFunc = it "Test equals false case" $
    execFunc "$neq" [] programEqualsFalse `shouldBe` (Right [I32Val 0])

testBlock execFunc = it "Test block" $
    execFunc "$bl" [I32Val 3] programBlock `shouldBe` (Right [I32Val 3])

testBranch execFunc = it "Test branch" $
    execFunc "$foo" [I32Val 3] programBranch `shouldBe` (Right [I32Val 5])

testBranchIfTrue execFunc = it "Test branchIf true case" $
    execFunc "$foo" [I32Val 1] programBranchIf `shouldBe` (Right [I32Val 8])

testBranchIfFalse execFunc = it "Test branchIf false case" $
    execFunc "$foo" [I32Val 0] programBranchIf `shouldBe` (Right [I32Val 15])

testTee execFunc = it "Test setting local variables with tee" $
    execFunc "$foo" [I32Val 0] programTee `shouldBe` (Right [I32Val 1])

testLoop execFunc = it "Test loop" $
    execFunc "$foo" [I32Val 0] programLoop `shouldBe` (Right [I32Val 4])

testIfElse execFunc = it "If else test" $
    execFunc "$foo" [I32Val 0] programIfElse `shouldBe` (Right [I32Val 3])

testNestedBlocks execFunc = it "Test nested blocks" $
    execFunc "$foo" [I32Val (-1)] programNestedBlocks `shouldBe`
        (Right [I32Val 0])

testFunctionCalls execFunc = it "Test function calls" $
    property $ \x -> execFunc "$quadruple" [I32Val (x::Integer)]
        programFunctionCalls `shouldBe` (Right [I32Val (4 * (x::Integer))])

testEvenOdd execFunc = it "Test complex function calls" $
    forAll genNonNeg $ \x -> execFunc "$even" [I32Val (x::Integer)]
        programEvenOdd `shouldBe` (Right [(boolToWasm (even (x::Integer)))])
