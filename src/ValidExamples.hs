import Parser
import Validator

-- run examples with
-- > check $ checkFunc fn#

-- simple binary, valid
--
-- i32.const 
-- i32.const 
-- i32.add
fn1 = Func "" [] (Block [Result I32] [Const I32, Const I32, Binary I32])

-- invalid binary
--
-- i32.const
-- i64.const
-- i32.add
fn2 = Func "" [] (Block [Result I32] [Const I32, Const I64, Binary I32])

-- valid block
--
-- i32.const
-- block (result i32)
--     i32.const
-- end
-- i32.add
fn3 = Func "" [] (Block [Result I32] code)
    where code = [Const I32, Bl [I32] [Const I32], Binary I32]

-- invalid block result type
--
-- i32.const
-- block (result i64)
--     i64.const
-- end
-- i32.add
fn4 = Func "" [] (Block [Result I32] code)
    where code = [Const I32, Bl [I64] [Const I64], Binary I32]

-- invalid block result stack
--
-- i32.const
-- block (result i32)
--     i32.const
--     i32.const
-- end
-- i32.add
fn5 = Func "" [] (Block [Result I32] code)
    where code = [Const I32, Bl [I32] [Const I32, Const I32], Binary I32]

-- valid branch
--
-- i32.const
-- block (result i32)
--     i32.const
--     br 0
-- end
-- i32.add
fn6 = Func "" [] (Block [Result I32] code)
    where code = [Const I32, Bl [I32] [Const I32, Br 0], Binary I32]

-- valid branch to outer function
--
-- i32.const
-- block (result i32)
--     i32.const
--     br 1
-- end
-- i32.add
fn7 = Func "" [] (Block [Result I32] code)
    where code = [Const I32, Bl [I32] [Const I32, Br 1], Binary I32]

-- invalid branch
--
-- i32.const
-- block (result i32)
--     i32.const
--     br 2
-- end
-- i32.add
fn8 = Func "" [] (Block [Result I32] code)
    where code = [Const I32, Bl [I32] [Const I32, Br 2], Binary I32]

-- valid if
--
-- i32.const
-- if (result i32)
--     i32.const
-- else
--     i32.const
-- end
fn9 = Func "" [] (Block [Result I32] code)
    where code = [Const I32, If [I32] [Const I32] [Const I32]]

-- invalid if branch
--
-- i32.const
-- if (result i32)
--     i64.const
-- else
--     i32.const
-- end
fn10 = Func "" [] (Block [Result I32] code)
    where code = [Const I32, If [I32] [Const I64] [Const I32]]