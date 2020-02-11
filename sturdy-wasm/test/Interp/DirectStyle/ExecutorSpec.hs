module Interp.DirectStyle.ExecutorSpec (spec) where

import Test.Hspec

import TestPrograms.Concrete
import Interp.DirectStyle.Executor

spec :: Spec
spec = describe "Run test programs on DirectStyle Executor" $
       sequence_ (fmap (\t -> t execFunc) tests)

-- Test branchIf true is missing
tests = [testSimpleFunction, testReadLocalVars, testSetLocalVars, testEquals,
         testEqualsFalse, testBlock, testBranch, testBranchIfFalse, testTee,
         testLoop, testIfElse, testNestedBlocks, testFunctionCalls, testEvenOdd]
