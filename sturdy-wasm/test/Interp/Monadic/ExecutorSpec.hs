module Interp.Monadic.ExecutorSpec (spec) where

import Test.Hspec

import TestPrograms.Concrete
import Interp.Monadic.Executor

spec :: Spec
spec = describe "Run test programs on Monadic Executor" $
       sequence_ (fmap (\t -> t execFunc) tests)

tests = [testSimpleFunction, testReadLocalVars, testSetLocalVars, testEquals,
         testEqualsFalse, testFunctionCalls]
