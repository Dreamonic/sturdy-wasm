module SimpleValidator where

import Parser(Instr(..), TypedInstr(..))
import WasmTypes

type ValType = WasmType
data InferType = Actual ValType | Unknown deriving (Show, Eq)

type StackType = [ValType]

data FuncType = FuncType StackType StackType deriving (Show)

data Frame = Frame {
    labelTypes :: [ValType],
    results :: [ValType],
    height :: Int,
    unreachable :: Bool
} deriving (Show, Eq)

data Context = Context {
    ops :: [InferType],
    frames :: [Frame]
    -- funcs :: [FuncType]
    -- locals :: [ValType]
} deriving (Show, Eq)

emptyContext = Context [] [] 

known :: [ValType] -> [InferType]
known xs = map Actual xs

push :: [InferType] -> Context -> Context
push ops' ctx = ctx { ops = (reverse ops') ++ (ops ctx) }

equalType :: InferType -> InferType -> Bool
equalType t1 t2 = t1 == t2 || t1 == Unknown || t2 == Unknown

checkStack :: [InferType] -> [InferType] -> Bool
checkStack ts1 ts2 = (length ts1) == (length ts2) && (and $ zipWith equalType ts1 ts2)

currentFrame :: Context -> Frame
currentFrame ctx = head $ frames ctx

popOp :: InferType -> Context -> InferType
popOp expect ctx = do
    let ops' = ops ctx
    let currentFrame' = currentFrame ctx
    let actual = do
        if length ops' == height currentFrame' then
            if unreachable currentFrame' then Unknown else error "Underflow occured"
        else
            head ops'
    case (actual, expect) of
        (Unknown, _) -> expect
        (_, Unknown) -> actual
        (a, b) -> if a == b then actual else error "Type equality error"

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
pushCtrl labelIn labelOut ctx = do
    let frame = Frame labelIn labelOut (length $ ops ctx) False
    ctx { frames = frame:(frames ctx) }

popCtrl :: Context -> Either String Context
popCtrl (Context _ []) = Left "Can't pop frame from empty stack"
popCtrl ctx @ (Context _ (f:fs)) = case pop (known $ results f) ctx of
    Right ctx' -> do
        if (length $ ops ctx') /= height f then
            Left $ "Incorrect return count"
        else
            Right $ ctx' { frames = fs }
    r -> r

setUnreachable :: Context -> Context
setUnreachable ctx @ (Context _ []) = ctx
setUnreachable ctx @ (Context ops' (f:fs)) = ctx { 
    frames = (f { unreachable = True } ):fs, 
    ops = drop ((length ops') - (height f)) ops'
    }

type Arrow = ([InferType], [InferType])

(-->) :: [ValType] -> [ValType] -> Arrow
(-->) ts1 ts2 = (known ts1, known ts2)

checkInstr :: Context -> Instr -> [InferType] -> Arrow
checkInstr ctx e s = case e of
    Const t -> 
        [] --> [t]

    Binary t ->
        [t, t] --> [t]

checkSeq :: Context -> [Instr] -> Either String Context
checkSeq ctx es = case es of
    [] -> 
        Right $ ctx { ops = [] }

    _ -> do
        let e = last es
        let es' = init es 
        ctx1 <- checkSeq ctx es'
        let (pops, pushes) = checkInstr ctx e (ops ctx1)
        ctx2 <- pop pops ctx1 
        Right $ push pushes ctx2

emptyCtx = pushCtrl [] [] (Context [] [])
validSeq ctx ops = case checkSeq ctx ops of
    Left err -> err
    Right (Context ops _) -> "Valid, end=" ++ show ops








