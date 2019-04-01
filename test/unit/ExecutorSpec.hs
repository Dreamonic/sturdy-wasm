module ExecutorSpec (spec) where

import Test.QuickCheck
import Test.Hspec
-- import Generators
-- import Parser
-- import Executor
-- import WasmTypes
-- import qualified Control.Exception as E

-- -- Test suites --

spec :: Spec
spec = return ()

-- -- Linking tests to Test Suites --

-- tsExecFunc :: Spec
-- tsExecFunc = describe "execFunc" $ do
--     testExecFunc
--     testExecAddition
--     testExecSubtraction
--     testExecMultiplication
--     testExecFuncInvalidParams
--     testCatchNoError
--     testCatchWithError
--     testExecFaultyIf
--     testExecLookupErrorCode
--     --testExecDivision
--     --testExecDivByZero

-- -- Exception types --

-- typeError :: Selector ExecutorException
-- typeError (TypeError _) = True
-- typeError _ = False

-- lookupError :: Selector ExecutorException
-- lookupError (LookupError _) = True
-- lookUpError _ = False

-- wasmArithError :: Selector ExecutorException
-- wasmArithError (WasmArithError _) = True
-- wasmArithError _ = False

-- -- Tests --
-- foo = Func "foo" [Param "a" I32, Param "b" I32, Param "c" I32,
--     Param "d" I32] (Block [Result I32] [
--     LocalGet "a",
--     LocalGet "b",
--     Numeric (Div I32 Signed),
--     LocalGet "c",
--     LocalGet "d",
--     Numeric (Mul I32),
--     Numeric (Sub I32)
--     ])

-- testExecFunc = it "Test some function with multiple math operators" $
--     execFunc foo [I32Val 8, I32Val 2, I32Val 5, I32Val 7] `shouldBe` [I32Val (-31)]

-- addition = Func "add" [Param "a" I32, Param "b" I32]
--     (Block [Result I32] [
--         LocalGet "a",
--         LocalGet "b",
--         Numeric (Add I32)
--     ])

-- testExecAddition = it "Test addition with 2 values" $
--     property $ \(x,y) -> execFunc addition [I32Val (x::Integer), I32Val (y::Integer)] `shouldBe` [I32Val (x+y)]

-- subtraction = Func "sub" [Param "a" I32, Param "b" I32]
--     (Block [Result I32] [
--         LocalGet "a",
--         LocalGet "b",
--         Numeric (Sub I32)
--     ])

-- testExecSubtraction = it "Test subtraction with 2 values" $
--     property $ \(x,y) -> execFunc subtraction [I32Val (x::Integer), I32Val (y::Integer)] `shouldBe` [I32Val (x-y)]

-- multiplication = Func "mul" [Param "a" I32, Param "b" I32]
--     (Block [Result I32] [
--         LocalGet "a",
--         LocalGet "b",
--         Numeric (Mul I32)
--     ])

-- testExecMultiplication = it "Test multiplication with 2 values" $
--     property $ \(x,y) -> execFunc multiplication [I32Val (x::Integer), I32Val (y::Integer)] `shouldBe` [I32Val (x*y)]


-- division = Func "div" [Param "a" I32, Param "b" I32]
--     (Block [Result I32] [
--         LocalGet "a",
--         LocalGet "b",
--         Numeric (Div I32 Signed)
--     ])


-- testExecDivision = it "Test division with 2 values" $
--     forAll arbitrary $ \x -> forAll genNonZero $ \y -> execFunc division [I32Val x, I32Val y] `shouldBe` [I32Val (quot x y)]

-- testCatchNoError = it "Test executorCatch on correct wasm code" $
--     forAll arbitrary $ \(x, y) -> executorCatch (\_ -> return [I32Val (-1)])
--         (execFunc addition [I32Val (x::Integer), I32Val (y::Integer)]) `shouldReturn` [I32Val (x + y)]

-- testCatchWithError = it "Test executorCatch on wasm code with insufficient parameters" $
--     forAll arbitrary $ \x -> executorCatch (\_ -> return [I32Val (-1)])
--         (execFunc addition [I32Val (x::Integer)]) `shouldReturn` [I32Val (-1)]

-- testExecFuncInvalidParams = it "Test addition with insufficient parameters" $
--     forAll arbitrary $ \x -> E.evaluate (execFunc addition [I32Val (x::Integer)]) `shouldThrow` typeError

-- faultyIf = Func "if" [Param "a" I64]
--     (Block [Result I64] [
--         LocalGet "a",
--         If (LocalGet "a")
--     ])

-- testExecFaultyIf = it "Expect a TypeError when executing a wasm if-statement with no I32Val on top of the stack" $
--     property $ \x -> E.evaluate (execFunc faultyIf [I64Val (x::Integer)]) `shouldThrow` typeError

-- lookupErrorCode = Func "lookup" []
--     (Block [Result F32] [
--         LocalGet "a"
--     ])

-- testExecLookupErrorCode = it "Test a LookupError when fetching a non-existing local variable" $
--     E.evaluate (execFunc lookupErrorCode []) `shouldThrow` lookupError
-- --testExecDivByZero = it "Test division by 0" $
-- --   property $ \x -> execFunc division [I32Val (x::Integer), I32Val 0] `shouldThrow` DivideByZero
