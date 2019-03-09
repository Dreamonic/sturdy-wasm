module ExecutorSpec (spec) where

import Test.QuickCheck
import Test.Hspec
import Generators
import Parser
import Executor
import Environment
import qualified Control.Exception as E

-- Test suites --

spec :: Spec
spec = do
    tsExecFunc

-- Linking tests to Test Suites --

tsExecFunc :: Spec
tsExecFunc = describe "execFunc" $ do
    testExecFunc
    testExecAddition
    testExecSubtraction
    testExecMultiplication
    testExecFuncInvalidParams
    testCatchNoError
    testCatchWithError
    testExecFaultyIf
    testExecLookupErrorCode
    testCtzInstruction
    testClzInstruction
    testAbsInstruction
    testNegInstruction
    --testExecDivision
    --testExecDivByZero

-- Exception types --

typeError :: Selector ExecutorException
typeError (TypeError _) = True
typeError _ = False

outOfScope :: Selector ExecutorException
outOfScope (OutOfScope _) = True
outOfScope _ = False

wasmArithError :: Selector ExecutorException
wasmArithError (WasmArithError _) = True
wasmArithError _ = False

-- Tests --
foo = Func "foo" [Param "a" I32, Param "b" I32, Param "c" I32,
    Param "d" I32] (Block [Result I32] [
    LocalGet "a",
    LocalGet "b",
    Numeric (Div I32 Signed),
    LocalGet "c",
    LocalGet "d",
    Numeric (Mul I32),
    Numeric (Sub I32)
    ])

testExecFunc = it "Test some function with multiple math operators" $
    execFunc foo (fromStack [I32Val 8, I32Val 2, I32Val 5, I32Val 7]) `shouldBe` (fromStack [I32Val (-31)])

addition = Func "add" [Param "a" I32, Param "b" I32]
    (Block [Result I32] [
        LocalGet "a",
        LocalGet "b",
        Numeric (Add I32)
    ])

testExecAddition = it "Test addition with 2 values" $
    property $ \(x,y) -> execFunc addition (fromStack [I32Val (x::Integer), I32Val (y::Integer)])
        `shouldBe` (fromStack [I32Val (x+y)])

subtraction = Func "sub" [Param "a" I32, Param "b" I32]
    (Block [Result I32] [
        LocalGet "a",
        LocalGet "b",
        Numeric (Sub I32)
    ])

testExecSubtraction = it "Test subtraction with 2 values" $
    property $ \(x,y) -> execFunc subtraction (fromStack [I32Val (x::Integer), I32Val (y::Integer)])
        `shouldBe` (fromStack [I32Val (x-y)])

multiplication = Func "mul" [Param "a" I32, Param "b" I32]
    (Block [Result I32] [
        LocalGet "a",
        LocalGet "b",
        Numeric (Mul I32)
    ])

testExecMultiplication = it "Test multiplication with 2 values" $
    property $ \(x,y) -> execFunc multiplication (fromStack [I32Val (x::Integer), I32Val (y::Integer)])
        `shouldBe` (fromStack [I32Val (x*y)])


division = Func "div" [Param "a" I32, Param "b" I32]
    (Block [Result I32] [
        LocalGet "a",
        LocalGet "b",
        Numeric (Div I32 Signed)
    ])


testExecDivision = it "Test division with 2 values" $
    forAll arbitrary $ \x -> forAll genNonZero $ \y -> execFunc division (fromStack [I32Val x, I32Val y])
        `shouldBe` (fromStack [I32Val (quot x y)])

defEnv :: Environment
defEnv = (fromStack [I32Val (-1)])

testCatchNoError = it "Test executorCatch on correct wasm code" $
    forAll arbitrary $ \(x, y) -> executorCatch (\_ -> return defEnv)
        (execFunc addition (fromStack [I32Val (x::Integer), I32Val (y::Integer)]))
            `shouldReturn` (fromStack [I32Val (x + y)])

testCatchWithError = it "Test executorCatch on wasm code with insufficient parameters" $
    forAll arbitrary $ \x -> executorCatch (\_ -> return defEnv)
        (execFunc addition (fromStack [I32Val (x::Integer)])) `shouldReturn` defEnv

testExecFuncInvalidParams = it "Test addition with insufficient parameters" $
    forAll arbitrary $ \x -> E.evaluate (execFunc addition (fromStack [I32Val (x::Integer)]))
        `shouldThrow` typeError

faultyIf = Func "if" [Param "a" I64]
    (Block [Result I64] [
        LocalGet "a",
        If (LocalGet "a")
    ])

testExecFaultyIf = it "Expect a TypeError when executing a wasm if-statement with no I32Val on top of the stack" $
    property $ \x -> E.evaluate (execFunc faultyIf (fromStack [I64Val (x::Integer)]))
        `shouldThrow` typeError

lookupErrorCode = Func "lookup" []
    (Block [Result F32] [
        LocalGet "a"
    ])

testExecLookupErrorCode = it "Test a OutOfScope when fetching a non-existing local variable" $
    E.evaluate (execFunc lookupErrorCode emptyEnv) `shouldThrow` outOfScope
--testExecDivByZero = it "Test division by 0" $
--   property $ \x -> execFunc division [I32Val (x::Integer), I32Val 0] `shouldThrow` DivideByZero

testCtzInstruction = it "Text correct behaviour of 32 bit ctz instruction" $
    execFunc (Func "$f" [] (Block [Result I32] [Numeric (Const (I32Val 4)), Numeric (Ctz I32)])) emptyEnv
        `shouldBe` (fromStack [I32Val 2])

testClzInstruction = it "Text correct behaviour of 32 bit clz instruction" $
    execFunc (Func "$f" [] (Block [Result I32] [Numeric (Const (I32Val 4)), Numeric (Clz I32)])) emptyEnv
        `shouldBe` (fromStack [I32Val 29])

testAbsInstruction = it "Text correct behaviour of abs instruction" $
    property $ \x -> execFunc (Func "$f" [] (Block [Result F64] [Numeric (Const (F64Val (x::Double))), Numeric (Abs F64)])) emptyEnv
        `shouldBe` (fromStack [F64Val $ abs x])

testNegInstruction = it "Text correct behaviour of neg instruction" $
    property $ \x -> execFunc (Func "$f" [] (Block [Result F64] [Numeric (Const (F64Val (x::Double))), Numeric (Neg F64)])) emptyEnv
        `shouldBe` (fromStack [F64Val (-1 * x)])