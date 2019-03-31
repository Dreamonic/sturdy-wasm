module Executor
  ( execFunc
  , ExecutorException(..)
  , executorCatch
  ) where

import Parser
import qualified Data.Map as Map
import qualified Control.Exception as E

import Data.Int

-- |All exceptions that can be thrown by the executor.
{-# DEPRECATED  WasmArithError "These cases should reduce to traps" #-}
data ExecutorException
    = TypeError String          -- ^For failed type matching.
    | LookupError String        -- ^For variable lookup errors.
    | WasmArithError String     -- ^For arithmetic errors that would have been
                                --  runtime errors in WASM (like div by zero).
    | NotImplemented Instr      -- ^For unimplemented instruction.
    deriving Show

instance E.Exception ExecutorException

-- |Catches ExecutorExceptions by using the given handler function that are
--  possible thrown by the given expression.
executorCatch :: (ExecutorException  -> IO a) -> a -> IO a
executorCatch handler x = E.catch (E.evaluate x) handler





{- Types -}

-- data Code = [value]

data ModInst =
  EmptyInst
  deriving (Show, Eq)

data Locals =
  EmptyBindings
  deriving (Show, Eq)

data Frame =
  FrameT ModInst Locals
  deriving (Show, Eq)

type Stack a = [a]

data Code =
  Code { codeStack :: (Stack WasmVal), codeEs :: [AdminInstr]}
  deriving (Show, Eq)

data Closure =
  Closure ModInst Func
  deriving (Show, Eq)

data AdminInstr =
    Plain Instr
  | Invoke Closure
  | Trapping String {- Trap with error message -}
  | Returning (Stack WasmVal)
  | Breaking Int32 (Stack WasmVal)
  | Label Int32 [Instr] Code
  | Frame Frame Code
  deriving (Show, Eq)


data Config =
  Config { confFrame :: Frame, confCode :: Code }
  deriving (Show, Eq)

{- reduction -}

step :: Config -> Config
step c@(Config frame (Code vs es)) = do
  let (vs', es') = step' (head es) vs c
  Config frame (Code vs' (es' ++ tail es))

-- |Intermediate step function
-- takes admin_instr and a value stack
-- returns the modified value stack
step' :: AdminInstr -> Stack WasmVal -> Config -> (Stack WasmVal, [AdminInstr])

-- Definitions for plain instructions
step' (Plain e) vs _ = case (e, vs) of
  (Nop, vs) ->
    (vs, [])

  (Unreachable, vs) ->
    (vs, [Trapping "unreachable code executed"])

  {- TODO: factor Numeric (binary, unary etc. out into its own function/file-}
  (Numeric e', v2 : v1 : vs') ->
    case e' of
      (Add typ) -> (binOp typ v1 v2 (+) (+) : vs', [])
      _     -> error "unimplemented numeric"

  ((Bl inputTypes instructions), vs) -> do
    let inputLen = fromIntegral $ length inputTypes :: Int32
    let code = Code [] (map Plain instructions)   -- ^map instructions in block to admin_instr
    (vs, [Label inputLen [] code])

  ((Loop inputTypes instructions), vs) -> do
    let code = Code [] (map Plain instructions)
    (vs, [Label 0 [e] code])                      -- ^add loop instr e inside inner label

  ((If inputTypes trueBl elseBl), (I32Val 0):vs') ->
    (vs', [Plain (Bl inputTypes elseBl)])

  ((If inputTypes trueBl elseBl), (I32Val _):vs') ->
    (vs', [Plain (Bl inputTypes trueBl)])

  ((Br x), vs) ->
    ([], [Breaking x vs])

  ((BrIf _), (I32Val 0):vs') ->
    (vs', [])

  ((BrIf x), (I32Val _):vs') ->
    (vs', [Plain (Br x)])

-- Breaking with no encapsulating label
-- should throw an error
step' (Breaking _ _) _  _ = error "undefined label"

-- End of label
-- should continue evalution
step' (Label _ _ (Code vs' [])) vs _ =
  (vs' ++ vs, [])

-- Trap inside label
-- current stack and msg is returned
step' (Label _ _ (Code _ ((Trapping msg):_))) vs _ =
  (vs, [Trapping msg])

-- Return inside label
-- current stack and return stack are returned
step' (Label _ _ (Code _ ((Returning vs'):_))) vs _ =
  (vs, [Returning vs'])

-- 'Break' to end of this label
-- the required amount of values are popped from the inner stack
-- and pushed on the outer stack
-- if the label has inside instructions (Loop does this)
-- the inside instructions are returned
step' (Label n inner (Code _ ((Breaking 0 retStack):_))) vs _ =
  ((take (fromIntegral n :: Int) retStack) ++ vs, map Plain inner)

-- 'Break' k label layers up
-- returns new Breaking instr with k-1
step' (Label _ _ (Code _ ((Breaking k retStack):_))) vs _ =
  (vs, [Breaking (k - 1) retStack])

-- Continue evaluation of instructions inside label
-- step code inside label in context of current config
-- return Label with resulting code
step' (Label n innerInstr code') vs c =
  let c' = step $ c { confCode = code' }        -- ^Step under c with the code inside label
  in (vs, [Label n innerInstr $ confCode c'])

step' _ _ _ = error "Not implemented"

-- |Evaluates code under given context
-- based on proposition 4.2 evalution should either
--    * return value stack
--    * trap
--    * modify context
eval :: Config -> Stack WasmVal
eval c@(Config _ (Code vs es)) = case es of
  [] -> vs
  (Trapping msg):_ -> error msg -- ^ TODO: change to other type of error handling
  _ -> eval $ step c

-- |Interprets and executes the given wasm function using the given list of
--  WasmVal parameters.
--  If the parameter list does not match the required parameters of the function
--  definition a TypeError is thrown. ExecutorExceptions are also thrown if
--  instructions in the function are not supported or lead to run-time errors.
{-# DEPRECATED  execFunc "Use step instead" #-}
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
{-# DEPRECATED  execBlock "Use step instead" #-}
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
{-# DEPRECATED  execInstr "Use step instead" #-}
execInstr :: Instr -> [WasmVal] -> Map.Map String WasmVal ->
    ([WasmVal], Map.Map String WasmVal)
--execInstr (EnterBlock block) stack locals = execBlock block stack locals
--execInstr (If instr) (x:stack) locals
--    | valToBool x = execInstr instr stack locals
--    | otherwise   = (stack, locals)
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
