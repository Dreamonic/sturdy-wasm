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
        \set_local $a\n\
        \br 0\n\
        \i32.const 1\n\
        \set_local $a\n\
    \end\n\
    \get_local $a\n\
    \i32.const 3\n\
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
    \(func $foo (param $x i32) (result i32)\n\
    \i32.const 0\n\
    \if (result i32)\n\
        \i32.const 2\n\
    \else\n\
        \i32.const 3\n\
    \end\n\
    \))"

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
