module Interp.TestPrograms
where

import Test.QuickCheck
import Test.Hspec

import Parsing.Parser (parseWasm, wasmModule)
import Syntax
import Types
import Interp.Util (ExecType(..))
import Generators

parse :: String -> WasmModule
parse = parseWasm wasmModule

allTests :: [ExecType -> SpecWith ()]
allTests = [testSimpleFunction, testReadLocalVars, testSetLocalVars, testEquals,
            testEqualsFalse, testBlock, testBranch, testBranchIfTrue,
            testBranchIfFalse, testTee, testLoop, testIfElse, testNestedBlocks,
            testFunctionCalls, testEvenOdd]

programSimpleFunction = parse
    "(module\n\
    \(func $add \n\
    \(result i32)\n\
    \i32.const 2\n\
    \i32.const 3\n\
    \i32.add))"

testSimpleFunction execFunc = it "Test simple function" $
    execFunc "$add" [] programSimpleFunction `shouldBe` (Right [I32Val 5])

programReadLocalVars = parse
    "(module\n\
    \(func $add (param $a i32)\n\
    \(result i32)\n\
    \get_local $a\n\
    \i32.const 3\n\
    \i32.add))"

testReadLocalVars execFunc = it "Test using local variables" $
    execFunc "$add" [I32Val 2] programReadLocalVars `shouldBe` (Right
        [I32Val 5])

programSetLocalVars = parse
    "(module\n\
    \(func $add (param $a i32)\n\
    \(result i32)\n\
    \i32.const 1\n\
    \set_local $a\n\
    \get_local $a\n\
    \i32.const 3\n\
    \i32.add))"

testSetLocalVars execFunc = it "Test setting local variables" $
    execFunc "$add" [I32Val 3] programSetLocalVars `shouldBe` (Right
        [I32Val 4])

programEquals = parse
    "(module\n\
    \(func $eq\n\
    \(result i32)\n\
    \i32.const 1\n\
    \i32.const 1\n\
    \i32.eq))"

testEquals execFunc = it "Test equals" $
    execFunc "$eq" [] programEquals `shouldBe` (Right [I32Val 1])

programEqualsFalse = parse
    "(module\n\
    \(func $neq\n\
    \(result i32)\n\
    \i32.const 1\n\
    \i32.const 3\n\
    \i32.eq))"

testEqualsFalse execFunc = it "Test equals false case" $
    execFunc "$neq" [] programEqualsFalse `shouldBe` (Right [I32Val 0])

programBlock = parse
    "(module\n\
    \(func $bl (param $a i32) (result i32)\n\
    \block (result i32)\n\
    \get_local $a\n\
    \end))"

testBlock execFunc = it "Test block" $
    execFunc "$bl" [I32Val 3] programBlock `shouldBe` (Right [I32Val 3])

programBranch = parse
    "(module\n\
    \(func $foo (param $a i32) (result i32)\n\
    \block (result i32)\n\
        \i32.const 2\n\
        \set_local $a\n\
        \br 0\n\
        \i32.const 1\n\
        \set_local $a\n\
    \end\n\
    \get_local $a\n\
    \i32.const 3\n\
    \i32.add))"

testBranch execFunc = it "Test branch" $
    execFunc "$foo" [I32Val 3] programBranch `shouldBe` (Right [I32Val 5])

programBranchIf = parse
    "(module\n\
    \(func $foo (param $a i32) (result i32)\n\
    \block (result i32)\n\
        \i32.const 8\n\
        \get_local $a\n\
        \br_if 1\n\
        \i32.const 7\n\
        \i32.add\n\
    \end))"

testBranchIfTrue execFunc = it "Test branchIf true case" $
    execFunc "$foo" [I32Val 1] programBranchIf `shouldBe` (Right [I32Val 8])

testBranchIfFalse execFunc = it "Test branchIf false case" $
    execFunc "$foo" [I32Val 0] programBranchIf `shouldBe` (Right [I32Val 15])

programTee = parse
    "(module\n\
    \(func $foo (param $x i32) (result i32)\n\
    \get_local $x\n\
    \i32.const 1\n\
    \i32.add\n\
    \tee_local $x))"

testTee execFunc = it "Test setting local variables with tee" $
    execFunc "$foo" [I32Val 0] programTee `shouldBe` (Right [I32Val 1])

programLoop = parse
    "(module\n\
    \(func $foo (param $x i32) (result i32)\n\
    \block\n\
        \loop\n\
            \get_local $x\n\
            \i32.const 1\n\
            \i32.add\n\
            \tee_local $x\n\
            \i32.const 4\n\
            \i32.eq\n\
            \br_if 1\n\
            \br 0\n\
        \end\n\
    \end\n\
    \get_local $x))"

testLoop execFunc = it "Test loop" $
    execFunc "$foo" [I32Val 0] programLoop `shouldBe` (Right [I32Val 4])

programIfElse = parse
    "(module\n\
    \(func $foo (param $x i32) (result i32)\n\
    \i32.const 0\n\
    \if (result i32)\n\
        \i32.const 2\n\
    \else\n\
        \i32.const 3\n\
    \end\n\
    \))"

testIfElse execFunc = it "If else test" $
    execFunc "$foo" [I32Val 0] programIfElse `shouldBe` (Right [I32Val 3])

programNestedBlocks = parse
    "(module\n\
    \(func $foo (param $x i32) (result i32)\n\
    \(block\n\
        \i32.const 0\n\
        \set_local $x\n\
        \(block\n\
            \br 1\n\
            \i32.const 2\n\
            \set_local $x\n\
        \end)\n\
        \i32.const 1\n\
        \set_local $x\n\
    \end)\n\
    \get_local $x\n\
    \))"

testNestedBlocks execFunc = it "Test nested blocks" $
    execFunc "$foo" [I32Val (-1)] programNestedBlocks `shouldBe` (Right [I32Val 0])

programFunctionCalls = parse
    "(module\n\
    \(func $quadruple (param $x i32) (result i32)\n\
    \get_local $x\n\
    \call $double\n\
    \call $double)\n\
    \(func $double (param $x i32) (result i32)\n\
    \get_local $x\n\
    \i32.const 2\n\
    \i32.mul))"

testFunctionCalls execFunc = it "Test function calls" $
    property $ \x -> execFunc "$quadruple" [I32Val (x::Integer)]
        programFunctionCalls `shouldBe` (Right [I32Val (4 * (x::Integer))])

programEvenOdd = parse
    "(module\n\
    \(func $even (param $x i32) (result i32)\n\
    \get_local $x\n\
    \i32.const 0\n\
    \i32.eq\n\
    \if (result i32)\n\
        \i32.const 1\n\
    \else\n\
        \get_local $x\n\
        \i32.const 1\n\
        \i32.sub\n\
        \call $odd\n\
    \end)\n\
    \(func $odd (param $x i32) (result i32)\n\
    \get_local $x\n\
    \i32.const 0\n\
    \i32.eq\n\
    \if (result i32)\n\
        \i32.const 0\n\
    \else\n\
        \get_local $x\n\
        \i32.const 1\n\
        \i32.sub\n\
        \call $even\n\
    \end))"

testEvenOdd execFunc = it "Test complex function calls" $
    forAll genNonNeg $ \x -> execFunc "$even" [I32Val (x::Integer)]
        programEvenOdd `shouldBe` (Right [(boolToWasm (even (x::Integer)))])
