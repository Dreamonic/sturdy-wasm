module Embedder
  ( runWasmRepl
) where

import qualified Data.Map as Map
import qualified Data.Text as T (strip, pack, unpack)
import Data.List
import System.IO

import Parsing.Parser
import Syntax
import Types
import Interp.Monadic.Executor

runWasmRepl :: IO ()
runWasmRepl = wasmRepl (WasmModule [])

wasmRepl :: WasmModule -> IO ()
wasmRepl module' = do
    putStr $ "$> "
    hFlush stdout
    input <- getLine
    argv <- return $ words input
    processRepl module' argv

processRepl :: WasmModule -> [String] -> IO ()
processRepl module' argv = case argv of
    "exit":_ -> goodbye
    ":q":_   -> goodbye
    ":l":xs  -> loadFile $ head xs
    vs ->       callFunc module' vs
    where goodbye = putStrLn "Goodbye!"

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
