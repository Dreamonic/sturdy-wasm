module Embedder
  ( runWasmRepl
  , cleanInput
) where

import qualified Data.Map as Map
import qualified Data.Text as T (strip, pack, unpack)
import Data.List
import Parser
import Eval
import System.IO
import MonadicExecutor

runWasmRepl :: IO ()
runWasmRepl = wasmRepl (WasmModule [])

wasmRepl :: WasmModule -> IO ()
wasmRepl module' = do
    putStr $ "$> "
    hFlush stdout
    input <- getLine
    argv <- return $ words input
    putStrLn ("Input: " ++ show argv)
    processRepl module' argv

processRepl :: WasmModule -> [String] -> IO ()
processRepl module' argv = case argv of
    "exit":_ -> putStrLn "Goodbye!"
    ":l":xs ->  loadFile $ head xs
    vs ->       callFunc module' vs

cleanInput :: String -> String
cleanInput str = intercalate " " (filter (/="") (map (T.unpack . T.strip . T.pack) (lines str)))

loadFile :: String -> IO ()
loadFile filename = do
    contents <- readFile filename
    module' <- return $ parseWasm wasmModule $ cleanInput contents
    putStrLn ("Module loaded: " ++ (show module'))
    wasmRepl module'

callFunc :: WasmModule -> [String] -> IO ()
callFunc module' (f:xs) = do
    vs <- return $ execFunc ('$':f) (toWasmVals xs) module'
    putStrLn ("Result: " ++ show vs)
    wasmRepl module'

toWasmVals :: [String] -> [WasmVal]
toWasmVals xs = case xs of
    [] -> []
    h:t -> (I32Val (read h :: Integer)) : (toWasmVals t)
