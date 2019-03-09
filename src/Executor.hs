module Executor
  ( execFunc
  , ExecutorException(..)
  , executorCatch
  ) where

import Parser
import Environment
import qualified Data.Map as M
import qualified Control.Exception as E
import Data.Bits

-- |All exceptions that can be thrown by the executor.
data ExecutorException
    = TypeError String          -- ^For failed type matching.
    | OutOfScope String         -- ^For variable scope errors.
    | WasmArithError String     -- ^For arithmetic errors that would have been
                                --  runtime errors in WASM (like div by zero).
    | NotImplemented Instr      -- ^For unimplemented instruction.
    deriving Show

instance E.Exception ExecutorException

-- |Catches ExecutorExceptions by using the given handler function that are
--  possible thrown by the given expression.
executorCatch :: (ExecutorException  -> IO a) -> a -> IO a
executorCatch handler x = E.catch (E.evaluate x) handler

-- |Interprets and executes the given wasm function using a given Environment.
--  If the list of args does not match the required parameters of the function
--  definition a TypeError is thrown. ExecutorExceptions are also thrown if
--  instructions in the function body lead to run-time errors.
execFunc :: Func -> Environment -> Environment
execFunc (Func name params block) env
    | validParams params (stack env) =
        let kvPairs = zipWith (\(Param name _) x -> (name, x)) params (stack env)
        in  clearLoc (execBlock block (clearStack (setLoc (M.fromList kvPairs) env)))
    | otherwise = E.throw (TypeError $ (show (stack env)) ++ " stack does not match "
        ++ "params " ++ (show params) ++ " of func " ++ name ++ ".")

-- |Returns a modification of the given Environment by executing the
--  instructions in the given block sequentially.
--  An ExecutorException might be thrown if instructions in the function are not
--  supported or lead to run-time errors.
execBlock :: Block -> Environment -> Environment
execBlock (Block res []) env
    | validResults res (stack env) = env
    | otherwise = E.throw (TypeError $ (show (stack env)) ++ " stack does not match"
        ++ "results " ++ (show res) ++ " of block.")
execBlock (Block res (i:is)) env =
    let update = execInstr i env
    in  execBlock (Block res (is)) update

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

-- |Returns a modification of the given Environment by executing the specified
--  instruction as if it were an actual wasm instruction.
--  Throws a NotImplemented if there is no implementation for the given Instr.
--  Throws a OutOfScope if the Instr cannot be executed within this
--  environment.
execInstr :: Instr -> Environment -> Environment
execInstr (EnterBlock block) env = execBlock block env
execInstr (If instr) env
    | valToBool (stackHead env) = execInstr instr (setStack (stackTail env) env)
    | otherwise = env
execInstr (LocalGet tag) env = case M.lookup tag (loc env) of
    Just val -> stackPush val env
    Nothing -> E.throw (OutOfScope $ "on lookup local var \"" ++ tag ++ "\".")
execInstr (LocalSet tag) env
    | M.member tag (loc env) = setStack (stackTail env) (insertLoc tag
        (stackHead env) env)
    | otherwise = E.throw (OutOfScope $ "on setting local var \"" ++ tag
        ++ "\".")
execInstr (LocalTee tag) env =
    let afterSet = execInstr (LocalSet tag) env
    in  execInstr (LocalGet tag) afterSet
execInstr (Numeric (Const val)) env = stackPush val env
execInstr (Numeric (Add typ)) env@(Environment (x:y:s) _ _) =
    setStack ((binOp typ y x (+) (+)):s) env
execInstr (Numeric (Sub typ)) env@(Environment (x:y:s) _ _) =
    setStack ((binOp typ y x (-) (-)):s) env
execInstr (Numeric (Mul typ)) env@(Environment (x:y:s) _ _) =
    setStack ((binOp typ y x (*) (*)):s) env
execInstr (Numeric (Div typ Signed)) env@(Environment (x:y:s) _ _) =
    setStack ((binOp typ y x div (/)):s) env
execInstr (Numeric (Abs typ)) env@(Environment (x:s) _ _) =
    setStack (res:s) env where res = fUnOp typ x abs
execInstr (Numeric (Neg typ)) env@(Environment (x:s) _ _) =
    setStack (res:s) env where res = fUnOp typ x negate
execInstr (Numeric (Sqrt typ)) env@(Environment (x:s) _ _) =
    setStack (res:s) env where res = fUnOp typ x sqrt
execInstr (Numeric (Clz typ)) env@(Environment (x:s) _ _) =
    setStack (res:s) env where res = iUnOp typ x $ toInteger . (clz typ)
execInstr (Numeric (Ctz typ)) env@(Environment (x:s) _ _) =
    setStack (res:s) env where res = iUnOp typ x $ toInteger . (ctz typ)
execInstr (Numeric (Popcnt typ)) env@(Environment (x:s) _ _) =
    setStack (res:s) env where res = iUnOp typ x $ toInteger . popCount
execInstr Nop env = env
execInstr instr _ = E.throw $ NotImplemented instr

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

-- | Evaluates an integer unary operator of a single WasmVal and
-- returns the resulting WasmVal.
iUnOp :: WasmType -> WasmVal -> (Integer -> Integer) -> WasmVal
iUnOp opType val op
  | val `ofType` opType = case val of
    (I32Val a) -> I32Val (op a)
    (I64Val a) -> I64Val (op a)
    _ -> E.throw $ TypeError "Integer unary operator called with float"
  | otherwise = E.throw $ TypeError ("operation type does not match, Expected: " ++
    show opType ++ " but got: " ++ show valueType)
  where valueType = getType val

-- | Evaluates an floating point unary operator of a single WasmVal and
-- returns the resulting WasmVal.
fUnOp :: WasmType -> WasmVal -> (Double -> Double) -> WasmVal
fUnOp opType val op
  | val `ofType` opType = case val of
    (F32Val a) -> F32Val (op a)
    (F64Val a) -> F64Val (op a)
    _ -> E.throw $ TypeError "Float unary operator called with integer"
  | otherwise = E.throw $ TypeError ("operation type does not match, Expected: " ++
    show opType ++ " but got: " ++ show valueType)
  where valueType = getType val

-- | Return the count of trailing zero bits in a
-- all bits are considered trailing zeros if a is 0
ctz :: WasmType -> Integer -> Int
ctz typ a = case typ of
  I32 -> countWhile condition [0..31]
  I64 -> countWhile condition [0..63]
  _ -> E.throw $ TypeError "ctz called with float"
  where condition = (not . testBit a)

-- | Return the count of leading zero bits in a
-- all bits are considered leading zeros if a is 0
clz :: WasmType -> Integer -> Int
clz typ a = case typ of
  I32 -> countWhile condition [31,30..0]
  I64 -> countWhile condition [63,62..0]
  _ -> E.throw $ TypeError "clz called with float"
  where condition = (not . testBit a)

countWhile :: Num a => (a -> Bool) -> [a] -> Int
countWhile _ [] = 0
countWhile c (x:xs) = if c x then 1 + countWhile c xs else 0