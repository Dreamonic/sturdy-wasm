module Embedding.Embedder
  ( runWasmRepl
  , cleanInput
) where

import qualified Data.Map as Map
import qualified Data.Text as T (strip, pack, unpack)
import Data.List
import System.IO

import Syntax
import Types
import Parsing.Parser
import Embedding.Eval
import Execution.MonadicExecutor

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
    res <- return $ execFunc ('$':f) (toWasmVals xs) module'
    case res of
        Right vs -> putStrLn ("Result: " ++ show vs)
        Left msg -> putStrLn ("Error: " ++ msg)
    wasmRepl module'

toWasmVals :: [String] -> [WasmVal]
toWasmVals xs = case xs of
    [] -> []
    h:t -> (I32Val (read h :: Integer)) : (toWasmVals t)
