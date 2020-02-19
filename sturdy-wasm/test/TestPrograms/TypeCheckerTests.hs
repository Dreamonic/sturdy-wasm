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
            testPreciseIntsAndFloats, testIllegalReturn, testIllegalAdd,
            testIllegalSub, testIllegalMul, testIllegalEq, testOutOfScopeVarGet,
            testOutOfScopeVarSet, testIllegalSetLocal, testIllegalBlockReturn,
            testIllegalPopInBlock, testIllegalLoopReturn, testOutOfBoundsBranch,
            testInfiniteLoop, testIllegalBranchReturn, testOutOfBoundsBranchIf,
            testIllegalBranchIfReturn, testOutOfScopeFuncCall,
            testIllegalFuncCallReturn, testIllegalFuncCallParameter,
            testInfiniteRecursion, testIllegalIfCondition, testIllegalIfReturn,
            testIllegalElseReturn, testNestedIfs, testManyNestedIfs,
            testNestedBlockBranch, testOnePass, testOnePassNested]

-- error message templates

emptyValStack :: String
emptyValStack = "Cannot pop from an empty value stack."

emptyFrStack :: String
emptyFrStack = "Cannot pop from an empty frame stack."

varOutOfScope :: String -> String
varOutOfScope var = "Variable " ++ var ++ " not in scope."

funcOutOfScope :: String -> String
funcOutOfScope f = "No function " ++ f ++ " in module."

expectType :: WasmType -> WasmType -> String
expectType actTy expTy = "Expected type " ++ (show expTy) ++ " but got " ++
    (show actTy) ++ "."

expectTypeList :: [WasmType] -> [WasmType] -> String
expectTypeList actTys expTys = "Expected types " ++ (show expTys) ++ " but got "
    ++ (show actTys) ++ "."



testSimpleFunction checkFunc = it "Check simple function" $
    checkFunc "$add" [] programSimpleFunction `shouldBe` (Right [I32])

testReadLocalVars checkFunc = it "Check using local variables" $
    checkFunc "$add" [I32] programReadLocalVars `shouldBe` (Right [I32])

testSetLocalVars checkFunc = it "Check setting local variables" $
    checkFunc "$add" [I32] programSetLocalVars `shouldBe` (Right [I32])

testEquals checkFunc = it "Check equals" $
    checkFunc "$eq" [] programEquals `shouldBe` (Right [I32])

testEqualsFalse checkFunc = it "Check equals false case" $
    checkFunc "$neq" [] programEqualsFalse `shouldBe` (Right [I32])

testBlock checkFunc = it "Check block" $
    checkFunc "$bl" [I32] programBlock `shouldBe` (Right [I32])

testBranch checkFunc = it "Check branch" $
    checkFunc "$foo" [I32] programBranch `shouldBe` (Right [I32])

testBranchIfTrue checkFunc = it "Check branchIf true case" $
    checkFunc "$foo" [I32] programBranchIf `shouldBe` (Right [I32])

testBranchIfFalse checkFunc = it "Check branchIf false case" $
    checkFunc "$foo" [I32] programBranchIf `shouldBe` (Right [I32])

testTee checkFunc = it "Check setting local variables with tee" $
    checkFunc "$foo" [I32] programTee `shouldBe` (Right [I32])

testLoop checkFunc = it "Check loop" $
    checkFunc "$foo" [I32] programLoop `shouldBe` (Right [I32])

testIfElse checkFunc = it "Check If" $
    checkFunc "$foo" [I32] programIfElse `shouldBe` (Right [I32, I32])

testNestedBlockBranch checkFunc = it "Check branching from nested blocks" $
    checkFunc "$foo" [I32] programNestedBlockBranch `shouldBe` (Right [I32])

testNestedBlocks checkFunc = it "Check nested blocks" $
    checkFunc "$foo" [] programNestedBlocks `shouldBe`
        (Right [I32, I32, I32, I32, I32])

testFunctionCalls checkFunc = it "Check function calls" $
    checkFunc "$quadruple" [I32] programFunctionCalls `shouldBe` (Right [I32])

testEvenOdd checkFunc = it "Check complex function calls" $
    checkFunc "$even" [I32] programEvenOdd `shouldBe` (Right [I32])

testIntsAndFloats checkFunc = it "Check program using i32 and f32" $
    checkFunc "$foo" [F32, I32] programIntsAndFloats `shouldBe` (Right [F32])

testPreciseIntsAndFloats checkFunc = it "Check program using i64 and f64" $
    checkFunc "$foo" [I64, F64] programPreciseIntsAndFloats `shouldBe` (Right
        [F64])

testIllegalReturn checkFunc = it "Fail when doing an illegal func return" $
    checkFunc "$foo" [] programIllegalReturn `shouldBe` (Left $
        expectTypeList [I32] [I64])

testIllegalAdd checkFunc = it "Fail when giving add instr illegal input" $
    checkFunc "$foo" [F64] programIllegalAdd `shouldBe` (Left $
        expectType F32 F64)

testIllegalSub checkFunc = it "Fail when giving sub instr illegal input" $
    checkFunc "$foo" [F64] programIllegalSub `shouldBe` (Left $
        expectType F32 F64)

testIllegalMul checkFunc = it "Fail when giving mul instr illegal input" $
    checkFunc "$foo" [F64] programIllegalMul `shouldBe` (Left emptyValStack)

testIllegalEq checkFunc = it "Fail when giving eq instr illegal input" $
    checkFunc "$foo" [F64] programIllegalEq `shouldBe` (Left $
        expectType F32 F64)

testOutOfScopeVarGet checkFunc = it "Fail when reading out-of-scope var" $
    checkFunc "$foo" [I32] programOutOfScopeVarGet `shouldBe` (Left $
        varOutOfScope "$somethingelse")

testOutOfScopeVarSet checkFunc = it "Fail when writing out-of-scope var" $
    checkFunc "$foo" [I32] programOutOfScopeVarSet `shouldBe` (Left $
        varOutOfScope "$somethingelse")

testIllegalSetLocal checkFunc = it "Fail when giving set_local instr illegal input" $
    checkFunc "$foo" [I32] programIllegalSetLocal `shouldBe` (Left $
        expectType F64 I32)

testIllegalBlockReturn checkFunc = it "Fail when doing an illegal block return" $
    checkFunc "$foo" [] programIllegalBlockReturn `shouldBe` (Left $
        expectTypeList [I32, I64] [I32])

testIllegalPopInBlock checkFunc = it "Fail when popping value from outside block" $
    checkFunc "$foo" [] programIllegalPopInBlock `shouldBe` (Left emptyValStack)

testIllegalLoopReturn checkFunc = it "Fail when doing an illegal loop return" $
    checkFunc "$foo" [] programIllegalLoopReturn `shouldBe` (Left $
        expectTypeList [F64] [I32])

testInfiniteLoop checkFunc = it "Check infinite loop" $
    checkFunc "$foo" [] programInfiniteLoop `shouldBe` (Right [I32])

testOutOfBoundsBranch checkFunc = it "Fail when branching out of bounds with br" $
    checkFunc "$foo" [] programOutOfBoundsBranch `shouldBe` (Left emptyFrStack)

testIllegalBranchReturn checkFunc = it "Fail when doing an illegal branch with br" $
    checkFunc "$foo" [] programIllegalBranchReturn `shouldBe` (Left $
        expectTypeList [I32, I32] [I32])

testOutOfBoundsBranchIf checkFunc = it "Fail when branching out of bounds with brIf" $
    checkFunc "$foo" [] programOutOfBoundsBranchIf `shouldBe` (Left
        emptyFrStack)

testIllegalBranchIfReturn checkFunc = it "Fail when doing an illegal branch with brIf" $
    checkFunc "$foo" [] programIllegalBranchIfReturn `shouldBe` (Left $
        expectTypeList [F32] [I32])

testOutOfScopeFuncCall checkFunc = it "Fail when calling out-of-scope function" $
    checkFunc "$foo" [] programOutOfScopeFuncCall `shouldBe` (Left $
        funcOutOfScope "$bar")

testIllegalFuncCallReturn checkFunc = it "Fail when function produces illegal output" $
    checkFunc "$foo" [] programIllegalFuncCallReturn `shouldBe` (Left $
        expectTypeList [I64] [I32])

testIllegalFuncCallParameter checkFunc = it "Fail when giving function illegal input" $
    checkFunc "$foo" [] programIllegalFuncCallParameter `shouldBe` (Left $
        expectTypeList [I32, I32] [I64, I32])

testInfiniteRecursion checkFunc = it "Check infinite recursion" $
    checkFunc "$foo" [] programInfiniteRecursion `shouldBe` (Right [I32])

testIllegalIfCondition checkFunc = it "Fail when giving if illegal input" $
    checkFunc "$foo" [] programIllegalIfCondition `shouldBe` (Left $
        expectType F64 I32)

testIllegalIfReturn checkFunc = it "Fail when if-block produces illegal output" $
    checkFunc "$foo" [] programIllegalIfReturn `shouldBe` (Left $
        expectTypeList [I64] [I32])

testIllegalElseReturn checkFunc = it "Fail when else-block produces illegal output" $
    checkFunc "$foo" [] programIllegalElseReturn `shouldBe` (Left $
        expectTypeList [I32, I32] [I32])

testNestedIfs checkFunc = it "Check nested ifs" $
    checkFunc "$foo" [I32, I32, I32] programNestedIfs `shouldBe` (Right [I64])

testManyNestedIfs checkFunc = it "Check many nested ifs" $
    checkFunc "$foo" [] programManyNestedIfs `shouldBe` (Right [I64])

testOnePass checkFunc = it "Check if if-statements do not make the type checker\
    \run inefficiently" $ checkFunc "$foo" [] programOnePass `shouldBe`
        (Left $ varOutOfScope "$heya")

testOnePassNested checkFunc = it "Check if nested if-statements do not make the\
    \type checker run inefficiently" $ checkFunc "$foo" [] programOnePassNested
        `shouldBe` (Left $ varOutOfScope "$bonjour")
