module SimpleValidator where

import Parser(Instr(..))
import WasmTypes
import Control.Monad

type ValType = WasmType
data InferType = Actual ValType | Unknown deriving (Show, Eq)

type StackType = [ValType]

data FuncType = FuncType StackType StackType deriving (Show)

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
    -- funcs :: [FuncType]
    -- globals
    -- memories
    -- tables
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
            else Left$ "Expected: " ++ show expect ++ " but got: " ++ show actual
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
