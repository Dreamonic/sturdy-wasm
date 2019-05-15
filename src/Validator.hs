module Validator where

import Parser(Instr(..), TypedInstr(..))
import WasmTypes
import SimpleValidator
import Control.Monad

-- State and Output function
-- An Either monad could have been used
-- but a similar monad to the executor is used
-- The resulting value is ignored
newtype M a = St (Context -> Either String (a, Context))
unSt (St f) = f

instance Functor M where 
    fmap f e = St $ \ctx -> do
        (a, ctx') <- (unSt e) ctx
        return (f a, ctx')

instance Applicative M where
    pure = return
    f <*> e = St $ \ctx -> do
        (f', ctx1) <- (unSt f) ctx
        (a, ctx2) <- (unSt e) ctx1
        return (f' a, ctx2)

instance Monad M where
    return x = St $ \ctx -> Right (x, ctx)
    e >>= f = St $ \ctx -> do
        (a, ctx') <- (unSt e) ctx
        unSt (f a) ctx'

-- | Pushes single value on op stack
pushM :: InferType -> M ()
pushM op = St $ \ctx -> Right ((), pushOp op ctx)

-- | Pops single value off op stack
-- 'Fails' if
--      The op stack underflows below the height of the ctrl frame height
--      A known value that differs from expected is popped off 
popM :: InferType -> M ()
popM expected = St $ \ctx -> do
    ctx' <- popOp expected ctx
    return ((), ctx')

-- | Push control frame with given type signature
pushCtrlM :: [ValType] -> [ValType] -> M ()
pushCtrlM ins outs = St $ \ctx -> Right ((), pushCtrl ins outs ctx)

-- | Pop control frame from control stack
-- 'Fails' if:
--      The return count is higher than the types on the op stack at the end of evaluation
--      The types of returns and current differ
--      The ctrl stack is empty
popCtrlM :: M ()
popCtrlM = St $ \ctx -> do
    ctx' <- popCtrl ctx
    return ((), ctx')

-- | Sets the unreachable flag of current ctrl frame to True
setUnreachableM :: M ()
setUnreachableM = St $ \ctx -> Right ((), setUnreachable ctx)

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

    If t true false -> do
        popM $ Actual I32
        checkBl [] t true
        checkBl [] t false

checkSeqM :: [Instr] -> M [()]
checkSeqM ops' = mapM checkM ops'

checkBl :: [ValType] -> [ValType] -> [Instr] -> M ()
checkBl ins outs ops = do
    forM_ (known ins) popM  -- ^ pushCtrl does not pop from outer frame 
    pushCtrlM ins outs      -- ^ 
    forM_ (known ins) pushM -- ^ and push into inner frame
    checkSeqM ops           -- ^
    popCtrlM                -- ^ popCtrl pops return values of inner frame

printRes :: M t -> Context -> IO ()
printRes m ctx = case (unSt) m $ ctx of
    Left err -> putStrLn err
    Right (_, ctx') -> do
        putStrLn "Valid"
        putStrLn $ "ops:\t" ++ show (ops ctx')
        putStrLn $ "frames: " ++ show (frames ctx')