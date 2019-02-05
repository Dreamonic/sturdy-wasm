module Executor
  ( executeFunc
  , wasmAdd
  ) where

import Parser
import Data.Set

wasmAdd = Func "add" [Param "a" I32, Param "b" I32] (Block [Result I32] [
  LocalGet "a",
  LocalGet "b",
  Numeric (Add I32)
  ])

executeFunc :: Func -> [WasmVal] -> Maybe [WasmVal]
executeFunc (Func name params block) vals =
  if not (validParams (Func name params block) vals) then Nothing else
    let kvPairs = zipWith (\(Param name _) x -> (name, x)) params vals
    in (executeBlock block [] (Map.fromList kvPairs))
    -- TODO return type checking

validParams :: Func -> [WasmVal] -> Bool
validParams (Func name params block) vals =
  let types = map (\(Param _ x) -> x) params
      zipped = zipWith sameType vals types
  in (length params) == (length vals) && (foldl (&&) True zipped)

executeBlock :: Block -> [WasmVal] -> Map.Map String WasmValue -> Maybe [WasmVal]
executeBlock = Nothing
