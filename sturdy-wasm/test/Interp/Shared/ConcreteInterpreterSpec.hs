module Interp.Shared.ConcreteInterpreterSpec (spec) where

import Test.Hspec

import Interp.TestPrograms
import Interp.Shared.ConcreteInterpreter

spec :: Spec
spec = describe "Run test programs on Shared Concrete Interpreter" $
       sequence_ (fmap (\t -> t execFunc) tests)

tests = allTests
