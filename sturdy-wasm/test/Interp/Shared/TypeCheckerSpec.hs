module Interp.Shared.TypeCheckerSpec (spec) where

import Test.Hspec

import Types
import Interp.TestPrograms (parse)
import Interp.Shared.TypeChecker

spec :: Spec
spec = do
    testIf

ifProgram = parse
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

testIf = it "If should validate both branches" $
         validateFunc "$if" [] ifProgram `shouldBe` Left "Expected type I32 but got I64."