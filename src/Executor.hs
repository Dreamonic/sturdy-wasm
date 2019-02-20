module Executor
  ( execFunc
  , ExecutorException(..)
  , executorCatch
  ) where

import Parser
import qualified Data.Map as Map
import qualified Control.Exception as E

-- |All exceptions that can be thrown by the executor.
data ExecutorException
    = TypeError String          -- ^For failed type matching.
    | LookupError String        -- ^For variable lookup errors.
    | NotImplemented Instr      -- ^For unimplemented instruction.
    deriving Show

instance E.Exception ExecutorException

-- |Catches ExecutorExceptions by using the given handler function that are
--  possible thrown by the given expression.
executorCatch :: (ExecutorException -> IO a) -> a -> IO a
executorCatch handler x = E.catch (E.evaluate x) handler

-- |Interprets and executes the given wasm function using the given list of
--  WasmVal parameters.
--  If the parameter list does not match the required parameters of the function
--  definition a TypeError is thrown. ExecutorExceptions are also thrown if
--  instructions in the function are not supported or lead to run-time errors.
execFunc :: Func -> [WasmVal] -> [WasmVal]
execFunc (Func name params block) vals
    | validParams params vals =
        let kvPairs = zipWith (\(Param name _) x -> (name, x)) params vals
        in fst (execBlock block [] (Map.fromList kvPairs))
    | otherwise = E.throw (TypeError $ (show vals) ++ " stack does not match "
        ++ "params " ++ (show params) ++ " of func " ++ name ++ ".")

-- |Returns a modification of the given WasmVal stack and execution
--  environment by executing the instructions in the given block.
--  An ExecutorException might be thrown if instructions in the function are not
--  supported or lead to run-time errors.
execBlock :: Block -> [WasmVal] -> Map.Map String WasmVal ->
    ([WasmVal], Map.Map String WasmVal)
execBlock (Block res []) stack locals
    | validResults res (reverse stack) = (reverse stack, locals)
    | otherwise = E.throw (TypeError $ (show stack) ++ " stack does not match"
        ++ "results " ++ (show res) ++ " of block.")
execBlock (Block res (i:is)) stack locals =
    let update = execInstr i stack locals
    in  execBlock (Block res (is)) (fst update) (snd update)

-- |Determines whether or not the given WasmVal stack matches a given parameter
--  definition. Match checking is done using the valsOfType function.
validParams :: [Param] -> [WasmVal] -> Bool
validParams params vals =
    let types = map (\(Param _ typ) -> typ) params
    in valsOfType vals types

-- |Determines whether or not the given WasmVal stack matches a given list of
--  return values. Match checking is done using the valsOfType function.
validResults :: [Result] -> [WasmVal] -> Bool
validResults results vals =
    let types = map (\(Result typ) -> typ) results
    in valsOfType vals types

-- |Checks if WasmVals in a list correspond with a list of WasmTypes. False is
--  returned if the two lists differ in size.
valsOfType :: [WasmVal] -> [WasmType] -> Bool
valsOfType vals types =
    let zipped = zipWith ofType vals types
    in (length types) == (length vals) && (foldl (&&) True zipped)

-- |Returns a modification of the given WasmVal stack and execution environment
--  by executing the given Instr in the given block as if it were an actual
--  wasm instruction.
--  Throws a NotImplemented if there is no implementation for the given Instr.
--  Throws a LookupError if the Instr cannot be executed within this
--  environment.
execInstr :: Instr -> [WasmVal] -> Map.Map String WasmVal ->
    ([WasmVal], Map.Map String WasmVal)
execInstr (EnterBlock block) stack locals = execBlock block stack locals
execInstr (If instr) (x:stack) locals
    | valToBool x = execInstr instr stack locals
    | otherwise   = (stack, locals)
execInstr (LocalGet tag) stack locals = case Map.lookup tag locals of
    Just val -> (val:stack, locals)
    Nothing -> E.throw (LookupError $ "on local var \"" ++ tag ++ "\".")
execInstr (LocalSet tag) (x:stack) locals = (stack, Map.insert tag x locals)
execInstr (LocalTee tag) stack locals =
    let whenSet = execInstr (LocalSet tag) stack locals
    in  execInstr (LocalGet tag) (fst whenSet) (snd whenSet)
execInstr (Numeric (Const val)) stack locals = (val:stack, locals)
execInstr (Numeric (Add typ)) (x:y:stack) locals =
    ((binOp typ y x (+) (+)):stack, locals)
execInstr (Numeric (Sub typ)) (x:y:stack) locals =
    ((binOp typ y x (-) (-)):stack, locals)
execInstr (Numeric (Mul typ)) (x:y:stack) locals =
    ((binOp typ y x (*) (*)):stack, locals)
execInstr (Numeric (Div typ Signed)) (x:y:stack) locals =
    ((binOp typ y x div (/)):stack, locals)
execInstr Nop stack locals = (stack, locals)
execInstr instr _ _ = E.throw $ NotImplemented instr

-- |Defines wasm I32 values as "falsy" if equal to 0 and "truthy" otherwise.
--  Throws a TypeError if a value of any other type than I32 is given.
valToBool :: WasmVal -> Bool
valToBool val = case val of
    I32Val x -> x /= 0
    val -> E.throw (TypeError $ (show val) ++ " cannot be evaluated to Bool.")

-- |Evaluates an expression of two WasmVals and a numeric binary operator and
--  returns the resulting WasmVal.
--  Throws a TypeError if the operation and of the WasmVals aren't all of the
--  same type.
binOp :: WasmType -> WasmVal -> WasmVal -> (Integer -> Integer -> Integer) ->
    (Double -> Double -> Double) -> WasmVal
binOp typ x y opI opF
    | x `ofType` typ && y `ofType` typ = case (x, y) of
        (I32Val a, I32Val b) -> I32Val (a `opI` b)
        (I64Val a, I64Val b) -> I64Val (a `opI` b)
        (F32Val a, F32Val b) -> F32Val (a `opF` b)
        (F64Val a, F64Val b) -> F64Val (a `opF` b)
        (_, _) -> E.throw (TypeError "binOp on two different types.")
    | otherwise = E.throw (TypeError "binOp type does not match arg type.")
