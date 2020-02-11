module Interp.Shared.ConcreteInterpreterSpec (spec) where

import Test.Hspec

import TestPrograms.ConcreteTests
import Interp.Shared.ConcreteInterpreter

spec :: Spec
spec = describe "Run ConcreteTests on Shared Concrete Interpreter" $
       sequence_ (fmap (\t -> t execFunc) tests)

tests = allTests
