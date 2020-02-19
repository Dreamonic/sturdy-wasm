module TestPrograms.Programs
where

import Syntax (WasmModule)
import Parsing.Parser (parseWasm, wasmModule)

parse :: String -> WasmModule
parse = parseWasm wasmModule

programSimpleFunction = parse
    "(module\n\
    \(func $add \n\
    \(result i32)\n\
    \i32.const 2\n\
    \i32.const 3\n\
    \i32.add))"

programReadLocalVars = parse
    "(module\n\
    \(func $add (param $a i32)\n\
    \(result i32)\n\
    \get_local $a\n\
    \i32.const 3\n\
    \i32.add))"

programSetLocalVars = parse
    "(module\n\
    \(func $add (param $a i32)\n\
    \(result i32)\n\
    \i32.const 1\n\
    \set_local $a\n\
    \get_local $a\n\
    \i32.const 3\n\
    \i32.add))"

programEquals = parse
    "(module\n\
    \(func $eq\n\
    \(result i32)\n\
    \i32.const 1\n\
    \i32.const 1\n\
    \i32.eq))"

programEqualsFalse = parse
    "(module\n\
    \(func $neq\n\
    \(result i32)\n\
    \i32.const 1\n\
    \i32.const 3\n\
    \i32.eq))"

programBlock = parse
    "(module\n\
    \(func $bl (param $a i32) (result i32)\n\
    \block (result i32)\n\
    \get_local $a\n\
    \end))"

programBranch = parse
    "(module\n\
    \(func $foo (param $a i32) (result i32)\n\
    \block (result i32)\n\
        \i32.const 2\n\
        \br 0\n\
        \set_local $a\n\
        \i32.const 1\n\
    \end\n\
    \get_local $a\n\
    \i32.add))"

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

programTee = parse
    "(module\n\
    \(func $foo (param $x i32) (result i32)\n\
    \get_local $x\n\
    \i32.const 1\n\
    \i32.add\n\
    \tee_local $x))"

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

programIfElse = parse
    "(module\n\
    \(func $foo (param $x i32) (result i32) (result i32)\n\
    \i32.const 0\n\
    \if (result i32)\n\
        \i32.const 2\n\
    \else\n\
        \i32.const 3\n\
    \end\n\
    \i32.const 1\n\
    \))"

programNestedBlocks = parse
    "(module\n\
    \(func $foo (result i32) (result i32) (result i32) (result i32) (result i32)\n\
    \block (result i32) (result i32) (result i32) (result i32)\n\
        \i32.const 5\n\
        \block (result i32) (result i32)\n\
            \i32.const 4\n\
            \block (result i32)\n\
                \i32.const 3\n\
            \end\n\
        \end\n\
        \i32.const 2\n\
    \end\n\
    \i32.const 1))"

programNestedBlockBranch = parse
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

programIntsAndFloats = parse
    "(module\n\
    \(func $foo (param $x i32) (param $y f32) (result f32)\n\
    \get_local $x\n\
    \i32.const -5\n\
    \i32.sub\n\
    \set_local $x\n\
    \get_local $y\n\
    \f32.const 4.44\n\
    \f32.mul))"

programPreciseIntsAndFloats = parse
    "(module\n\
    \(func $foo (param $x f64) (param $y i64) (result f64)\n\
    \get_local $y\n\
    \i64.const 2\n\
    \i64.mul\n\
    \set_local $y\n\
    \get_local $x\n\
    \f64.const 11.2\n\
    \f64.sub))"

programIllegalReturn = parse
    "(module\n\
    \(func $foo (result i64)\n\
    \i32.const 1))"

programIllegalAdd = parse
    "(module\n\
    \(func $foo (param $x f64) (result f64)\n\
    \get_local $x\n\
    \f32.const 5.5\n\
    \f64.add))"

programIllegalSub = parse
    "(module\n\
    \(func $foo (param $x f64) (result f64)\n\
    \f32.const 5.5\n\
    \get_local $x\n\
    \f64.sub))"

programIllegalMul = parse
    "(module\n\
    \(func $foo (param $x f64) (result f64)\n\
    \get_local $x\n\
    \f64.mul))"

programIllegalEq = parse
    "(module\n\
    \(func $foo (param $x f64) (result i32)\n\
    \get_local $x\n\
    \f32.const 5.5\n\
    \f64.eq))"

programOutOfScopeVarGet = parse
    "(module\n\
    \(func $foo (param $x i32) (result i32)\n\
    \get_local $somethingelse\n\
    \i32.const 4\n\
    \i32.add))"

programOutOfScopeVarSet = parse
    "(module\n\
    \(func $foo (param $x i32) (result i32)\n\
    \i32.const 4\n\
    \set_local $somethingelse\n\
    \i32.const 0))"

programIllegalSetLocal = parse
    "(module\n\
    \(func $foo (param $x i32) (result i32)\n\
    \f64.const 4.0\n\
    \set_local $x\n\
    \i32.const 1))"

programIllegalBlockReturn = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \block (result i32)\n\
        \i64.const 3\n\
        \i32.const 2\n\
    \end))"

programIllegalPopInBlock = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \i32.const 3\n\
    \block (result i32)\n\
        \i32.const 2\n\
        \i32.add\n\
    \end))"

programIllegalLoopReturn = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \loop (result i32)\n\
        \f64.const 0.0\n\
    \end))"

programInfiniteLoop = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \loop (result i32)\n\
        \br 0\n\
        \i32.const 1\n\
    \end))"

programOutOfBoundsBranch = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \loop (result i32)\n\
        \br 2\n\
    \end))"

programIllegalBranchReturn = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \block (result i32)\n\
        \i32.const 1\n\
        \i32.const 2\n\
        \br 1\n\
    \end))"

programOutOfBoundsBranchIf = parse
    "(module\n\
    \(func $foo (result i64)\n\
    \loop (result i64)\n\
        \i32.const -1\n\
        \br_if 2\n\
    \end))"

programIllegalBranchIfReturn = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \block (result i32)\n\
        \f32.const 1.14\n\
        \i32.const -1\n\
        \br_if 0\n\
    \end))"

programOutOfScopeFuncCall = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \call $bar))"

programIllegalFuncCallReturn = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \call $bar)\n\
    \(func $bar (result i64)\n\
    \i64.const 1))"

programIllegalFuncCallParameter = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \i32.const 1\n\
    \i32.const 2\n\
    \call $bar)\n\
    \(func $bar (param $x i32) (param $y i64) (result i32)\n\
    \get_local $x))"

programInfiniteRecursion = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \call $bar)\n\
    \(func $bar (result i32)\n\
    \call $foo))"

programIllegalIfCondition = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \f64.const 1.0\n\
    \if\n\
        \nop\n\
    \else\n\
        \nop\n\
    \end))"

programIllegalIfReturn = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \i32.const 1\n\
    \if (result i32)\n\
        \i64.const 1\n\
    \else\n\
        \i32.const 2\n\
    \end))"

programIllegalElseReturn = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \i32.const 1\n\
    \if (result i32)\n\
        \i32.const 1\n\
    \else\n\
        \i32.const 2\n\
        \i32.const 3\n\
    \end))"

programNestedIfs = parse
    "(module\n\
    \(func $foo (param $x i32) (param $y i32) (param $z i32) (result i64)\n\
    \get_local $x\n\
    \if (result i64)\n\
        \get_local $y\n\
        \if (result i64)\n\
            \i64.const 1\n\
        \else\n\
            \i64.const 2\n\
        \end\n\
    \else\n\
        \get_local $y\n\
        \if (result i64)\n\
            \get_local $z\n\
            \if (result i64)\n\
                \i64.const 3\n\
            \else\n\
                \i64.const 4\n\
            \end\n\
        \else\n\
            \i64.const 5\n\
        \end\n\
    \end))"

manyIfs :: Int -> String
manyIfs n
    | n == 0    = "i64.const 2\n"
    | even n    = "i32.const 1\n\
                   \if (result i64)\n" ++ manyIfs (n - 1) ++
                       "else\n\
                           \i64.const 2\n\
                       \end\n"
    | otherwise =  "i32.const 1\n\
                   \if (result i64)\n\
                       \i64.const 2\n\
                       \else\n" ++ manyIfs (n - 1) ++ "end\n"

programManyNestedIfs = parse $
    "(module\n\
    \(func $foo (result i64)\n" ++ manyIfs 100 ++ "))"

programOnePass = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \i32.const 1\n\
    \if (result i32)\n\
        \i32.const 1\n\
    \else\n\
        \get_local $heya\n\
    \end\n\
    \get_local $aloha))"

programOnePassNested = parse
    "(module\n\
    \(func $foo (result i32)\n\
    \i32.const 1\n\
    \if (result i32)\n\
        \i32.const 1\n\
        \if (result i32)\n\
            \i32.const 1\n\
            \if (result i32)\n\
                \i32.const 1\n\
            \else\n\
                \get_local $bonjour\n\
            \end\n\
        \else\n\
            \get_local $gutentag\n\
        \end\n\
    \else\n\
        \get_local $hello\n\
    \end\n\
    \get_local $hola))"
