module Interp.Shared.TypeCheckerSpec (spec) where

import Test.Hspec

import Types
import Interp.TestPrograms (parse)
import Interp.Shared.TypeChecker

spec :: Spec
spec = do
    testIf
    testNestedIf
    testIfEmpty
    testIfSingular

simpleIfSpec = parse
    "(module\n\
    \(func $if (result i32)\n\
    \i32.const 1\n\
    \if (result i32)\n\
        \i32.const 1\n\
        \i32.const 2\n\
        \i32.add\n\
    \else\n\
        \i64.const 10\n\
    \end\n\
    \i32.const 3\n\
    \i32.add))"

-- | TODO check why nested ifs are not validated
nestedIfSpec = parse
    "(module\n\
    \(func $if (result i32)\n\
        \i32.const 1\n\
        \if (result i32)\n\
            \i32.const 2\n\
            \if (result i32)\n\
                \i32.const 10\n\
            \else\n\
                \i32.const 20\n\
            \end\n\
        \else\n\
            \i32.const 30\n\
        \end\n\
    \)\n\
    \)"

ifSpec = parse
    "(module\n\
      \(func $dummy)\n\
    \\n\
      \(func $empty (param $a i32)\n\
        \(if (local.get $a) (then))\n\
        \(if (local.get $a) (then) (else))\n\
        \(if $l (local.get $a) (then))\n\
        \(if $l (local.get $a) (then) (else))\n\
      \)\n\
    \\n\
      \(func $singular (param $a i32) (result i32)\n\
        \(if (local.get $a) (then (nop)))\n\
        \(if (local.get $a) (then (nop)) (else (nop)))\n\
        \(if (result i32) (local.get $a) (then (i32.const 7)) (else (i32.const 8)))\n\
      \)\n\
    \)"

testIf = it "If should validate both branches" $
         validateFunc "$if" [] simpleIfSpec `shouldBe` Left "Expected type I32 but got I64."

testNestedIf = it "Correct nested if should validate correctly" $
    validateFunc "$if" [] nestedIfSpec `shouldBe` Right [I32]

testIfEmpty = it "Empty if should validate correctly" $
        validateFunc "$empty" [I32] ifSpec `shouldBe` Right []

testIfSingular = it "Singular if should validate correctly" $
    validateFunc "$singular" [I32] ifSpec `shouldBe` Right [I32]