module Eval (
    execFunc
) where

import MonadicExecutor
import Parser
import WasmTypes

execFunc :: [WasmVal] -> Func -> Stack WasmVal
execFunc vs func = case snd $ (unEnv eval) $ initConfig vs (Invoke (Closure EmptyInst func)) of
    Config _ (Code vs _) -> vs


eval :: MExecutor ()
eval = do
    cond <- hasInstr ;
    if cond
        then do {
            instr <- getInstr ;
            step instr ;
            eval }
        else
            return ()


step :: AdminInstr -> MExecutor ()

step (Plain e) = case e of
    {- -}
    Nop -> return ()
    Unreachable -> putInstr (Trapping "unreachable code executed")
    Const v ->      push v
    Binary _ e' ->  case e' of
        Add ->   do {    a <- pop ;
                            b <- pop ;
                            push (a + b) }
        Sub ->   do {    a <- pop ;
                            b <- pop ;
                            push (a - b) }
        Mul ->   do {    a <- pop ;
                            b <- pop ;
                            push (a * b) }
        Div _ -> do {    a <- pop ;
                            b <- pop ;
                            push (a / b) }
        err ->   error ("Not implemented Binary: " ++ show err)

    Compare _ e' -> case e' of
        Eql -> do {    a <- pop ;
                            b <- pop ;
                            push $ boolToWasm (a == b) }
        err -> error ("Not implemented Compare: " ++ show err)

    Block tps es -> do {    let len = length tps in
                            let c = code es in
                            putInstr (label len c) }

    Loop tps es ->  do {    len <- return $ length tps ;
                            c <- return $ code es ;
                            putInstr (Label len [e] c) }

    If tps tb fb -> do {    cond <- pop ;
                            if wasmToBool cond
                            then putInstr (Plain (Block tps tb))
                            else putInstr (Plain (Block tps fb)) }

    Br v ->         do {    vs <- retrieveStack ;
                            putInstr (Breaking v vs) }

    BrIf v ->       do {    cond <- pop ;
                            if wasmToBool cond
                            then putInstr (Plain (Br v))
                            else return () }

    LocalGet v ->   do {    var <- getVar v ;
                            push var }

    LocalSet v ->   do {    val <- pop ;
                            setVar v val }

    err ->          error ("Not implemented Plain: " ++ show err)

step (Invoke (Closure _ (Func name params _ instr))) = do {
    parseBinds params ;
    putInstrList (fmap Plain instr) }

step err = error ("Not implemented instruction: " ++ show err)

parseBinds :: [Param] -> MExecutor ()
parseBinds ps = case ps of
    [] -> return ()
    (Param p _):ps -> do {
        x <- pop ;
        setVar p x ;
        parseBinds ps }
