module Executor
  ( execFunc
  ) where

import Parser
import qualified Data.Map as Map

execFunc :: Func -> [WasmVal] -> [WasmVal]
execFunc (Func name params block) vals =
  if (validParams params vals) then
    let kvPairs = zipWith (\(Param name _) x -> (name, x)) params vals
    in (execBlock block [] (Map.fromList kvPairs))
    else error ("Type error: given stack " ++ (show vals) ++ " does not match "
      ++ "the parameter types " ++ (show params) ++ " of function " ++ name
      ++ ".")

execBlock :: Block -> [WasmVal] -> Map.Map String WasmVal -> [WasmVal]
execBlock (Block res []) stack _ =
  if validResults res (reverse stack) then reverse stack
    else error ("Type error: resulting stack " ++ (show stack) ++ " does not "
      ++ "match result types " ++ (show res) ++ " of the block.")
execBlock (Block res (i:is)) stack locals =
  let newStack = execInstr i stack locals
  in  execBlock (Block res (is)) newStack locals

validParams :: [Param] -> [WasmVal] -> Bool
validParams params vals =
  let types = map (\(Param _ typ) -> typ) params
  in valsOfType vals types

validResults :: [Result] -> [WasmVal] -> Bool
validResults results vals =
  let types = map (\(Result typ) -> typ) results
  in valsOfType vals types

valsOfType :: [WasmVal] -> [WasmType] -> Bool
valsOfType vals types =
  let zipped = zipWith ofType vals types
  in (length types) == (length vals) && (foldl (&&) True zipped)

execInstr :: Instr -> [WasmVal] -> Map.Map String WasmVal -> [WasmVal]
execInstr instr stack locals = case (instr, stack, locals) of
  ((LocalGet tag), stack, locals) -> case Map.lookup tag locals of
    Just val -> val:stack
    Nothing -> error ("No local variable in scope with tag \"" ++ tag ++ "\".")
  ((Numeric (Add typ)), (x:y:stack), _) -> (binOp typ y x (+) (+)):stack
  ((Numeric (Sub typ)), (x:y:stack), _) -> (binOp typ y x (-) (-)):stack
  ((Numeric (Mul typ)), (x:y:stack), _) -> (binOp typ y x (*) (*)):stack
  ((Numeric (Div typ Signed)), (x:y:stack), _) -> (binOp typ y x div (/)):stack
  (instr, _, _) -> error ("Couldn't evaluate " ++ (show instr) ++ ".")

binOp :: WasmType -> WasmVal -> WasmVal -> (Integer -> Integer -> Integer) ->
  (Double -> Double -> Double) -> WasmVal
binOp typ x y opI opF = if not (x `ofType` typ && y `ofType` typ)
  then error ("Type error: binary operation type does not match the types of"
    ++ " its arguments.")
  else case (x, y) of
    (I32Val a, I32Val b) -> I32Val (a `opI` b)
    (I64Val a, I64Val b) -> I64Val (a `opI` b)
    (F32Val a, F32Val b) -> F32Val (a `opF` b)
    (F64Val a, F64Val b) -> F64Val (a `opF` b)
    (_, _) -> error "Type error: binary operation on two different types."

--iUnOp :: WasmType -> WasmVal -> WasmVal -> (Integer -> Integer) -> WasmVal
--iUnOp
