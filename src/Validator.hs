module Validator where

import Parser(Instr(..), TypedInstr(..))
import WasmTypes
import SimpleValidator

-- State and Output function
newtype M a = St (Context -> Either String (a, Context))
unSt (St f) = f

instance Functor M where 
    fmap f e = St $ \ctx -> case (unSt e) ctx of
        Right (a, ctx') -> Right (f a, ctx')
        Left err -> Left err

instance Applicative M where
    pure = return
    f <*> e = St $ \ctx -> let res = (unSt f) ctx in case res of
        Right (a, ctx1) -> case (unSt e) ctx1 of
            Right (b, ctx2) -> Right (a b, ctx2)
            Left err -> Left err
        Left err -> Left err

instance Monad M where
    return x = St $ \ctx -> Right (x, ctx)
    e >>= f = St $ \ctx -> do
        (a, ctx') <- (unSt e) ctx
        unSt (f a) ctx'

pushM :: [InferType] -> M ()
pushM ops' = St $ \ctx -> Right ((), push ops' ctx)

popM :: [InferType] -> M ()
popM expected = St $ \ctx -> do
    ctx' <- pop expected ctx
    Right ((), ctx')

checkM :: Instr -> [InferType] -> M ()
checkM e s = case e of
    Const t -> do
        pushM $ known [t]

    Binary t -> do
        popM $ known [t, t]
        pushM $ known [t]
