module Validator where

import Parser
import WasmTypes
import Control.Monad

type ValType = WasmType
data InferType = Actual ValType | Unknown deriving (Show, Eq)

type StackType = [ValType]

type FuncType = (StackType, StackType)

data Frame = Frame {
    labels :: [ValType],
    results :: [ValType],
    height :: Int,
    unreachable :: Bool
} deriving (Show, Eq)

data Context = Context {
    ops :: [InferType],
    frames :: [Frame],
    locals :: [ValType]
} deriving (Show, Eq)

emptyCtx = Context [] [] []

known :: [ValType] -> [InferType]
known xs = map Actual xs

pushOp :: InferType -> Context -> Context
pushOp op ctx = ctx { ops = op:(ops ctx) }

push :: [InferType] -> Context -> Context
push ops' ctx = ctx { ops = (reverse ops') ++ (ops ctx) }

equalType :: InferType -> InferType -> Bool
equalType t1 t2 = t1 == t2 || t1 == Unknown || t2 == Unknown

checkStack :: [InferType] -> [InferType] -> Bool
checkStack ts1 ts2 = (length ts1) == (length ts2) && (and $ zipWith equalType ts1 ts2)

currentFrame :: Context -> Frame
currentFrame ctx = head $ frames ctx

popOp :: InferType -> Context -> Either String (InferType, Context)
popOp expect ctx = do
    let ops' = ops ctx
    let currentFrame' = currentFrame ctx
    actual <- do
        if length ops' == height currentFrame' then
            if unreachable currentFrame' then Right Unknown else Left "Underflow occured"
        else
            Right $ head ops'
    case (actual, expect) of
        (Actual a, Actual b) -> 
            if a == b then 
                return $ (actual, ctx { ops = drop 1 ops' })
            else Left $ "Expected: " ++ show expect ++ " but got: " ++ show actual
        otherwise -> return (actual, ctx { ops = drop 1 ops' })

pop :: [InferType] -> Context -> Either String Context
pop expected ctx = do
    let cFrame = currentFrame ctx
    let stack = ops ctx
    let ex = length expected
    let st = length stack - height cFrame
    let min' = min ex st
    let padding = if unreachable cFrame then (ex - min') else 0
    if checkStack expected ((take min' stack) ++ (replicate padding Unknown)) then
        Right $ ctx { ops = drop min' stack }
    else
        Left $ "Expected: " ++ show expected ++ " but got: " ++ show (take min' stack)

pushCtrl :: [ValType] -> [ValType] -> Context -> Context
pushCtrl labels results ctx = do
    let frame = Frame labels results (length $ ops ctx) False
    ctx { frames = frame:(frames ctx) }

popCtrl :: Context -> Either String Context
popCtrl (Context _ [] _) = Left "Can't pop frame from empty stack"
popCtrl ctx @ (Context _ (f:fs) _) = do
    ctx' <- pop (known $ results f) ctx
    when ((length $ ops ctx') /= height f) (Left $ "Type mismatch in block")
    return $ ctx' { frames = fs }

setUnreachable :: Context -> Context
setUnreachable ctx @ (Context _ [] _) = ctx
setUnreachable ctx @ (Context ops' (f:fs) _) = ctx { 
    frames = (f { unreachable = True } ):fs, 
    ops = drop ((length ops') - (height f)) ops'
    }

-- State and Output function
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

failIf :: (Context -> Bool) -> String -> M ()
failIf guard err = Env $ \ctx -> if guard ctx then Left err else Right ((), ctx)

-- | Fails with err if (getter Context) does not satisfy guard
failM :: (Context -> a) -> (a -> Bool) -> String -> M ()
failM getter guard err = failIf (guard . getter) err

-- | Pushes single value on op stack
pushM :: InferType -> M ()
pushM op = Env $ \ctx -> Right ((), pushOp op ctx)

-- | Pops single value off op stack
-- 'Fails' if
--      The op stack height decreases below that of the current control frame
--      A known value that differs from 'expected' is popped off
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

-- | Get frame at i
-- 'Fails' if:
--      If i >= length of control frame stack
peekCtrlM :: Int -> M Frame
peekCtrlM i = Env $ \ctx -> do
    let frames' = frames ctx
    when (i >= length frames') $ Left $ "Index " ++ show i ++ " of frame stack is out of range"
    return (frames' !! i, ctx)

-- | Push single value on locals stack
pushLocal :: ValType -> M ()
pushLocal val = Env $ \ctx -> do
    let locals' = locals ctx
    return ((), ctx { locals = val:locals' })

-- | Pop single value from locals stack, fails if local stack is empty
popLocal :: M ValType
popLocal = Env $ \ctx -> do
    let locals' = locals ctx
    when (length locals' == 0) $ Left "Cannot pop local from empty stack"
    return (head locals', ctx { locals = tail locals' })

-- | Get value at local[i], fails if locals[i] does not exist
getLocal :: Int -> M ValType
getLocal i = Env $ \ctx -> do
    let locals' = locals ctx
    when (i >= length locals') $ Left $ "Index " ++ show i ++ " of local stack is out of range"
    return (locals' !! i, ctx)

-- | Set local[i] to given value, fails if locals[i] does not exist
setLocal :: Int -> ValType -> M ()
setLocal i value = Env $ \ctx -> do
    let (Context _ _ locals) = ctx
    when (i >= length locals) $ Left $ "Index " ++ show i ++ " of local stack is out of range"
    return ((), ctx { locals = update locals i value} )     -- ^ maybe change locals to Seq instead of List
    where update xs n val = take n xs ++ [val] ++ drop (n + 1) xs

-- | Sets the unreachable flag of current control frame to True
setUnreachableM :: M ()
setUnreachableM = Env $ \ctx -> Right ((), setUnreachable ctx)

-- | Validate single instruction under context
checkM :: Instr -> M ()
checkM e = case e of
    Const val -> do
        let t = getType val
        pushM $ Actual t
        
    Block t ops' -> do
        checkLabel t t ops'
        forM_ (known t) pushM

    Br levels -> do
        let n = fromInteger levels
        failM frames (((>=) n) . length) $ "Cannot branch up by " ++ show n
        frame <- peekCtrlM n
        forM_ (known $ labels frame) popM
        setUnreachableM

    BrIf levels -> do
        let n = fromInteger levels
        failM frames (((>=) n) . length) $ "Cannot branch up by " ++ show n
        popM $ Actual I32
        frame <- peekCtrlM n
        forM_ (known $ labels frame) popM
        forM_ (known $ labels frame) pushM
        
    If t trueBr falseBr -> do
        popM $ Actual I32
        checkLabel t t trueBr
        checkLabel t t falseBr
        forM_ (known t) pushM

    Loop t ops' -> do
        checkLabel [] t ops'
        forM_ (known t) pushM

    Binary t _ -> do
        popM $ Actual t
        popM $ Actual t
        pushM $ Actual t

    Unary t _ -> do
        popM $ Actual t
        pushM $ Actual t

    Nop -> do
        return ()

    Unreachable -> do
        setUnreachableM

-- | Applies checkM on a list of instructions
checkSeqM :: [Instr] -> M ()
checkSeqM ops' = mapM_ checkM ops'
    
-- | Validate label under context
checkLabel :: [ValType] -> [ValType] -> [Instr] -> M()
checkLabel labels results ops' = do
    pushCtrlM labels results
    checkSeqM ops'
    popCtrlM

-- | Validates a single function
checkFunc :: Func -> M ()
checkFunc (Func _ params results instr) = do
    let params' = map getValue params
    let results' = [t | (Result t) <- results]
    forM_ params' pushLocal
    pushCtrlM results' results'
    checkSeqM instr
    popCtrlM
    replicateM_ (length params) popLocal

-- | Applies given context on state output function and prints the result
printRes :: M t -> Context -> IO ()
printRes m ctx = case unpack m $ ctx of
    Left err -> putStrLn err
    Right (_, ctx') -> do
        putStrLn "Valid"
        putStrLn $ "ops:\t" ++ show (ops ctx')
        putStrLn $ "frames: " ++ show (frames ctx')
        putStrLn $ "locals: " ++ show (locals ctx')