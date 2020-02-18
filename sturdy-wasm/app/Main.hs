module Main where

import Embedder

import Interp.Shared.TypeChecker
import Parsing.Parser

main :: IO ()
main = do
    let f = validateFunc "$if" [] nestedIfSpec
    putStrLn (show f)

nestedIfSpec = parseWasm wasmModule
    "(module\n\
    \(func $if (result i32)\n\
        \i32.const 1\n\
        \if (result i32)\n\
            \i32.const 1\n\
            \if (result i32)\n\
                \i32.const 10\n\
            \else\n\
                \i32.const 10\n\
            \end\n\
        \else\n\
            \i32.const 30\n\
        \end\n\
    \)\n\
    \)"