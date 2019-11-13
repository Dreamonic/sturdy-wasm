module Interp.Util
    ( funcMapFromModule
    , ExecType(..)
    ) where

import qualified Data.Map as M

import Syntax
import Types

type ExecType = String -> [WasmVal] -> WasmModule -> Either String [WasmVal]

-- |    Retrieve a Map from a WasmModule from which Funcs can be fetched by
--      using the String of their name as a key.
funcMapFromModule :: WasmModule -> M.Map String Func
funcMapFromModule m = let fs = modFuncs m
                      in  M.fromList (zip (map fName fs) fs)
