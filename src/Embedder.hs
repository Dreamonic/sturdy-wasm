module Embedder (
    runWasmRepl
) where

import qualified Data.Map as Map
import qualified Data.Text as T (strip, pack, unpack)
import Data.List
import Parser
import Executor
import System.IO

runWasmRepl :: IO ()
runWasmRepl = wasmRepl Map.empty

wasmRepl :: Map.Map String Func -> IO ()
wasmRepl map = do
    putStr $ "$> "
    hFlush stdout
    input <- getLine
    argv <- return $ words input
    putStrLn ("Input: " ++ show argv)
    processRepl map argv

processRepl :: Map.Map String Func -> [String] -> IO ()
processRepl map argv = case argv of
    "exit":_ -> putStrLn "Goodbye!"
    ":l":xs -> loadFile map $ head xs
    vs -> callFunc map vs

cleanInput :: String -> String
cleanInput str = intercalate " " (filter (/="") (map (T.unpack . T.strip . T.pack) (lines str)))

loadFile :: Map.Map String Func -> String -> IO ()
loadFile map filename = do
    contents <- readFile filename
    func <- return $ parseFunc Parser.function $ cleanInput contents
    map' <- return $ Map.insert (getFuncName func) func map
    putStrLn ("Function loaded: " ++ (tail (getFuncName func)))
    wasmRepl map'

callFunc :: Map.Map String Func -> [String] -> IO ()
callFunc map (f:xs) = do
    func <- return $ case Map.lookup ('$':f) map of
        Just x -> x
        Nothing -> error "Function not loaded"
    vs <- return $ execFunc (toWasmVals xs) func
    putStrLn ("Result: " ++ show vs)
    wasmRepl map

getFuncName :: Func -> String
getFuncName (Func name _ _) = name

toWasmVals :: [String] -> [WasmVal]
toWasmVals xs = case xs of
    [] -> []
    h:t -> (I32Val (read h :: Integer)) : (toWasmVals t)