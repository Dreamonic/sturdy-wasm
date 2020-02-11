module Interp.Shared.TypeCheckerSpec (spec) where

import Test.Hspec

import TestPrograms.TypeCheckerTests
import Interp.Shared.TypeChecker

spec :: Spec
spec = describe "Run TypeCheckerTests on Shared TypeChecker" $
       sequence_ (fmap (\t -> t checkFunc) tests)

tests = allTests
