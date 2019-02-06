module Executor
  ( executeFunc
  ) where

import Parser
import qualified Data.Map as Map

executeFunc :: Func -> [WasmVal] -> Maybe [WasmVal]
executeFunc (Func name params block) vals =
  if not (validParams params vals) then Nothing else
    let kvPairs = zipWith (\(Param name _) x -> (name, x)) params vals
    in (executeBlock block [] (Map.fromList kvPairs))

executeBlock :: Block -> [WasmVal] -> Map.Map String WasmVal -> Maybe [WasmVal]
executeBlock (Block res []) stack _ =
  if validResults res (reverse stack) then Just (reverse stack) else Nothing
executeBlock (Block res (i:is)) stack locals =
  case (executeInstr i stack locals) of
      Nothing -> Nothing
      Just newStack -> executeBlock (Block res (is)) newStack locals

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

executeInstr :: Instr -> [WasmVal] -> Map.Map String WasmVal -> Maybe [WasmVal]
executeInstr (LocalGet tag) stack locals = case Map.lookup tag locals of
  Just val -> Just (val:stack)
  Nothing -> Nothing
executeInstr (Numeric (Add typ)) (x:y:stack) _ = case add typ x y of
  Just val -> Just (val:stack)
  Nothing -> Nothing
executeInstr _ _ _ = Nothing

add :: WasmType -> WasmVal -> WasmVal -> Maybe WasmVal
add typ x y = if not (x `ofType` typ) then Nothing else
  case (x, y) of (I32Val a, I32Val b) -> Just (I32Val (a + b))
                 (I64Val a, I64Val b) -> Just (I64Val (a + b))
                 (F32Val a, F32Val b) -> Just (F32Val (a + b))
                 (F64Val a, F64Val b) -> Just (F64Val (a + b))
                 (_, _) -> Nothing
