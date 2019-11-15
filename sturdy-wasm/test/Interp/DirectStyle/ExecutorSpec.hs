module Interp.DirectStyle.ExecutorSpec (spec) where

import Test.Hspec

import Interp.TestPrograms
import Interp.DirectStyle.Executor

spec :: Spec
spec = describe "Run test programs on DirectStyle Executor" $
       sequence_ (fmap (\t -> t execFunc) tests)

tests = allTests
