module Validator where

import Parser
import WasmTypes
import SimpleValidator
import Control.Monad

-- State and Output function
-- An Either monad could have been used
-- but a similar monad to the executor is used
-- The resulting value is ignored
newtype M a = Env (Context -> Either String (a, Context))
unpack (Env f) = f

instance Functor M where 
    fmap f e = Env $ \ctx -> do
        (a, ctx') <- (unpack e) ctx
        return (f a, ctx')

instance Applicative M where
    pure = return
    f <*> e = Env $ \ctx -> do
        (f', ctx1) <- (unpack f) ctx
        (a, ctx2) <- (unpack e) ctx1
        return (f' a, ctx2)

instance Monad M where
    return x = Env $ \ctx -> Right (x, ctx)
    e >>= f = Env $ \ctx -> do
        (a, ctx') <- (unpack e) ctx
        unpack (f a) ctx'

-- | Pushes single value on op stack
pushM :: InferType -> M ()
pushM op = Env $ \ctx -> Right ((), pushOp op ctx)

-- | Pops single value off op stack
-- 'Fails' if
--      The op stack underflows below the height of the ctrl frame height
--      A known value that differs from expected is popped off 
popM :: InferType -> M InferType
popM expected = Env $ \ctx -> do
    (r, ctx') <- popOp expected ctx
    return (r, ctx')

-- | Push control frame with given type signature
pushCtrlM :: [ValType] -> [ValType] -> M ()
pushCtrlM labels results = Env $ \ctx -> Right ((), pushCtrl labels results ctx)

-- | Pop control frame from control stack
-- 'Fails' if:
--      The return count is higher than the types on the op stack at the end of evaluation
--      The types of returns and current differ
--      The ctrl stack is empty
popCtrlM :: M ()
popCtrlM = Env $ \ctx -> do
    ctx' <- popCtrl ctx
    return ((), ctx')

-- | Push single value on locals stack
pushLocal :: ValType -> M ()
pushLocal val = Env $ \ctx -> do
    let (Context _ _ locals) = ctx
    return ((), ctx { locals = val:locals })

-- | Get value at local[i], fails if locals[i] does not exist
getLocal :: Int -> M ValType
getLocal i = Env $ \ctx -> do
    let (Context _ _ locals) = ctx
    when (i >= length locals) $ Left $ "Index " ++ show i ++ " of local stack is out of range"
    return (locals !! i, ctx)

-- | Set local[i] to given value, fails if locals[i] does not exist
setLocal :: Int -> ValType -> M ()
setLocal i value = Env $ \ctx -> do
    let (Context _ _ locals) = ctx
    when (i >= length locals) $ Left $ "Index " ++ show i ++ " of local stack is out of range"
    return ((), ctx { locals = update locals i value} )     -- ^ maybe change locals to Seq instead of List
    where update xs n val = take n xs ++ [val] ++ drop (n + 1) xs

-- | Sets the unreachable flag of current ctrl frame to True
setUnreachableM :: M ()
setUnreachableM = Env $ \ctx -> Right ((), setUnreachable ctx)

-- | Typecheck single instruction under context
checkM :: Instr -> M ()
checkM e = case e of
    Unreachable -> do
        setUnreachableM

    Const t -> do
        pushM $ Actual t

    Binary t -> do
        popM $ Actual t
        popM $ Actual t
        pushM $ Actual t

    If t trueBr falseBr -> do
        popM $ Actual I32
        checkLabel t t trueBr
        checkLabel t t falseBr
        forM_ (known t) pushM
    
    Bl t ops' -> do
        checkLabel t t ops'
        forM_ (known t) pushM

checkSeqM :: [Instr] -> M ()
checkSeqM ops' = mapM_ checkM ops'
    
checkLabel :: [ValType] -> [ValType] -> [Instr] -> M()
checkLabel labels results ops' = do
    pushCtrlM labels results
    checkSeqM ops'
    popCtrlM

checkFunc :: Func -> M ()
checkFunc (Func _ params (Block results instr)) = do
    let params' = map getValue params
    let results' = [t | (Result t) <- results]
    forM_ params' pushLocal
    pushCtrlM results' results'
    checkSeqM instr
    popCtrlM

check m = case unpack m emptyCtx of
    Left err -> err
    Right r  -> "Valid"

printRes :: M t -> Context -> IO ()
printRes m ctx = case unpack m $ ctx of
    Left err -> putStrLn err
    Right (_, ctx') -> do
        putStrLn "Valid"
        putStrLn $ "ops:\t" ++ show (ops ctx')
        putStrLn $ "frames: " ++ show (frames ctx')
        putStrLn $ "locals: " ++ show (locals ctx')