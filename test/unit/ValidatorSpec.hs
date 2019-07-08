module ValidatorSpec (spec) where

import Test.QuickCheck
import Test.Hspec
import SimpleValidator
import Validator
import Parser

-- | Helper function for checking result of validation
eval :: M a -> Bool
eval m = case unpack m emptyCtx of
    Left _ -> False 
    Right _ -> True
    
spec :: Spec
spec = do 
    basic
    blocks
    branching
    control

basic = describe "basic" $ do
    testSimpleFunction
    testInvalidAdd

blocks = describe "blocks" $ do
    testValidBlock
    testInvalidBlockResult
    testInvalidBlockEndStack

branching = describe "branching" $ do
    testValidBranchingSingleLevel
    testValidBranchingOuterLevel
    testInvalidBranching

control = describe "control" $ do
    testValidIf
    testInvalidIfBranch

testSimpleFunction = do
    let fn = Func "" [] [Result I32] [Const (I32Val 1),Const (I32Val 1),Binary I32 Add]
    it "Basic function should validate correctly" $
        eval (checkFunc fn) `shouldBe` True

testInvalidAdd = do
    let fn = Func "" [] [Result I32] [Const (I32Val 1),Const (I64Val 1),Binary I32 Add]
    it "Adding different types should fail" $
        eval (checkFunc fn) `shouldBe` False

testValidBlock = do
    let fn = Func "" [] [Result I32] [Const (I32Val 1),Block [I32] [Const (I32Val 2)],Binary I32 Add]
    it "Block should validate correctly" $
        eval (checkFunc fn) `shouldBe` True

testInvalidBlockResult = do
    let fn = Func "" [] [Result I32] [Const (I32Val 1),Block [I64] [Const (I32Val 2)],Binary I32 Add]
    it "Block with invalid result type should fail" $
        eval (checkFunc fn) `shouldBe` False

testInvalidBlockEndStack = do
    let fn = Func "" [] [Result I32] [Const (I32Val 1),Block [I32] [Const (I32Val 2), Const (I32Val 3)],Binary I32 Add]
    it "Block with mismatched result and instruction body type should fail" $
        eval (checkFunc fn) `shouldBe` False

testValidBranchingSingleLevel = do
    let fn = Func "" [] [Result I32] [Const (I32Val 1),Block [I32] [Const (I32Val 2), Br 0],Binary I32 Add]
    it "Branching a single level out of a block should validate correctly" $
        eval (checkFunc fn) `shouldBe` True

testValidBranchingOuterLevel = do
    let fn = Func "" [] [Result I32] [Const (I32Val 1),Block [I32] [Const (I32Val 2), Br 1],Binary I32 Add]
    it "Branching out of a function should validate correctly" $
        eval (checkFunc fn) `shouldBe` True

testInvalidBranching = do
    let fn = Func "" [] [Result I32] [Const (I32Val 1),Block [I32] [Const (I32Val 2), Br 2],Binary I32 Add]
    it "Branching out too high out of scope should fail" $
        eval (checkFunc fn) `shouldBe` False

testValidIf = do
    let fn = Func "" [] [Result I32] [Const (I32Val 1), If [I32] [Const (I32Val 1)] [Const (I32Val 2)]]
    it "If statement with correct branches should validate correctly" $
        eval (checkFunc fn) `shouldBe` True

testInvalidIfBranch = do
    let fn = Func "" [] [Result I32] [Const (I32Val 1), If [I32] [Const (I64Val 1)] [Const (I32Val 2)]]
    it "If statement with an incorrectly function type of a single branch should fail" $
        eval (checkFunc fn) `shouldBe` False

-- test = do
--     let fn = 
--     it "" $
--     eval (checkFunc fn) `shouldBe` False