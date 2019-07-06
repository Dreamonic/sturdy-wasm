module Eval (
    execFunc
) where

import MonadicExecutor
import Parser
import WasmTypes
import Debug.Trace

execFunc :: [WasmVal] -> Func -> Either String (Stack WasmVal)
execFunc vs func = 
    let res = (unEnv eval) $ initConfig vs (Invoke (Closure EmptyInst func))
    in case fst res of
        Left msg -> Left msg
        _ -> Right $ getStack $ snd res


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
    Numeric e' -> case e' of
        Add _ ->    do {    a <- pop ;
                            b <- pop ;
                            push (a + b) }
        Sub _ ->    do {    a <- pop ;
                            b <- pop ;
                            push (a - b) }
        Mul _ ->    do {    a <- pop ;
                            b <- pop ;
                            push (a * b) }
        Div _ _ ->  do {    a <- pop ;
                            b <- pop ;
                            push (a / b) }
        Eql _ ->    do {    a <- pop;
                            b <- pop ;
                            push $ boolToWasm (a == b) }
        Const v ->          push v
        err ->              throwError ("Not implemented Numeric: " ++ show err)

    Bl tps es ->    do {    let len = length tps in
                            let c = code es in
                            putInstr (label len c) }

    Loop tps es ->  do {    len <- return $ length tps ;
                            c <- return $ code es ;
                            putInstr (Label len [e] c) }

    If tps tb fb -> do {    cond <- pop ;
                            if wasmToBool cond
                            then putInstr (Plain (Bl tps tb))
                            else putInstr (Plain (Bl tps fb)) }

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

    err ->                  throwError ("Not implemented Plain: " ++ show err)

step (Invoke (Closure _ (Func name params block))) = do {
    parseBinds params ;
    putBlock block }

step (Breaking levels vs) = case levels of
    0 ->        endLabel
    n -> do {   branchUp;
                putInstr (Breaking (n-1) vs) }

step err = throwError ("Not implemented instruction: " ++ show err)

parseBinds :: [Param] -> MExecutor ()
parseBinds ps = case ps of
    [] -> return ()
    (Param p _):ps -> do {
        x <- pop ;
        setVar p x ;
        parseBinds ps }