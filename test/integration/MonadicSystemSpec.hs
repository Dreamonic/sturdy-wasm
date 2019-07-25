module MonadicSystemSpec (spec) where

    import Test.QuickCheck
    import Test.Hspec
    import Lexer
    import Parser
    import WasmTypes
    import Eval
    import qualified Data.Map as Map

    -- Test suites --
    spec :: Spec
    spec = do
        tsFunctions


    -- Linking tests to Test Suites --

    tsFunctions = describe "functions" $ do
        testSimpleFunction
        testReadLocalVars
        testSetLocalVars
        testEquals
        testEqualsFalse
        testFunctionCalls
        -- testEvenOdd
        -- testLoop
        -- testBlock
        -- testIfElse
        -- testNestedBlocks


    -- Tests --

    programSimpleFunction = "(module\n\
                \(func $add \n\
                \(result i32)\n\
                \i32.const 2\n\
                \i32.const 3\n\
                \i32.add))"

    moduleSimpleFunction = parseWasm Parser.wasmModule programSimpleFunction

    testSimpleFunction = it "Test simple function" $
        execFunc "$add" [] moduleSimpleFunction `shouldBe` [I32Val 5]

    programReadLocalVars = "(module\n\
            \(func $add (param $a i32)\n\
            \(result i32)\n\
            \get_local $a\n\
            \i32.const 3\n\
            \i32.add))"

    moduleReadLocalVars = parseWasm Parser.wasmModule programReadLocalVars

    testReadLocalVars = it "Test using local variables" $
        execFunc "$add" [I32Val 2] moduleReadLocalVars `shouldBe` [I32Val 5]

    programSetLocalVars = "(module\n\
        \(func $add (param $a i32)\n\
        \(result i32)\n\
        \i32.const 1\n\
        \set_local $a\n\
        \get_local $a\n\
        \i32.const 3\n\
        \i32.add))"

    moduleSetLocalVars = parseWasm Parser.wasmModule programSetLocalVars

    testSetLocalVars = it "Test setting local variables" $
        execFunc "$add" [I32Val 3] moduleSetLocalVars `shouldBe` [I32Val 4]

    programEquals = "(module\n\
        \(func $eq\n\
        \(result i32)\n\
        \i32.const 1\n\
        \i32.const 1\n\
        \i32.eq))"

    moduleEquals = parseWasm Parser.wasmModule programEquals

    testEquals = it "Test equals" $
        execFunc "$eq" [] moduleEquals `shouldBe` [I32Val 1]

    programEqualsFalse = "(module\n\
        \(func $neq\n\
        \(result i32)\n\
        \i32.const 1\n\
        \i32.const 3\n\
        \i32.eq))"

    moduleEqualsFalse = parseWasm Parser.wasmModule programEqualsFalse

    testEqualsFalse = it "Test equals" $
        execFunc "$neq" [] moduleEqualsFalse `shouldBe` [I32Val 0]


    programBlock = "(module\n\
        \(func $foo (param $a i32)\n\
        \(result i32)\n\
        \(block\n\
        \i32.const 2\n\
        \set_local $a\n\
        \br 0\n\
        \i32.const 1\n\
        \set_local $a\n\
        \end)\n\
        \get_local $a\n\
        \i32.const 3\n\
        \i32.add))"

    moduleBlock = parseWasm Parser.wasmModule programBlock

    testBlock = it "Test block" $
        execFunc "$foo" [I32Val 3] moduleBlock `shouldBe` [I32Val 5]


    programLoop = "(module\n\
        \(func $foo (param $x i32) (result i32)\n\
        \(block\n\
        \(loop\n\
            \get_local $x\n\
            \i32.const 1\n\
            \i32.add\n\
            \set_local $x\n\
            \get_local $x\n\
            \i32.const 4\n\
            \i32.eq\n\
            \br_if 1\n\
            \br 0\n\
        \end)\n\
        \end)\n\
        \get_local $x\n\
        \))"

    moduleLoop = do
        parseWasm Parser.wasmModule programLoop

    testLoop = it "Loop test" $
        execFunc "$foo" [I32Val 0] moduleLoop `shouldBe` [I32Val 4]

    programIfElse = "(module\n\
        \(func $foo (param $x i32) (result i32)\n\
        \i32.const 0\n\
        \if (result i32)\n\
            \i32.const 2\n\
        \else\n\
            \i32.const 3\n\
        \end\n\
        \))"

    moduleIfElse = do
        parseWasm Parser.wasmModule programIfElse

    testIfElse = it "If else test" $
        execFunc "$foo" [I32Val 0] moduleIfElse `shouldBe` [I32Val 3]

    programNestedBlocks = "(module\n\
        \(func $foo (param $x i32) (result i32)\n\
        \(block\n\
            \i32.const 0\n\
            \set_local $x\n\
            \(block\n\
                \br 1\n\
                \i32.const 2\n\
                \set_local $x\n\
            \end)\n\
            \i32.const 1\n\
            \set_local $x\n\
        \end)\n\
        \get_local $x\n\
        \))"

    moduleNestedBlocks =
        parseWasm Parser.wasmModule programNestedBlocks

    testNestedBlocks = it "Test nested blocks" $
        execFunc "$foo" [I32Val (-1)] moduleNestedBlocks `shouldBe` [I32Val 0]

    programFunctionCalls = "(module\n\
        \(func $quadruple (param $x i32) (result i32)\n\
        \get_local $x\n\
        \call $double\n\
        \call $double)\n\
        \(func $double (param $x i32) (result i32)\n\
        \get_local $x\n\
        \i32.const 2\n\
        \i32.mul))"

    moduleFunctionCalls =
        parseWasm Parser.wasmModule programFunctionCalls

    testFunctionCalls = it "Test function calls" $
        property $ \x -> execFunc "$quadruple" [I32Val (x::Integer)]
            moduleFunctionCalls `shouldBe` [(I32Val (4 * (x::Integer)))]

    programEvenOdd = "(module\n\
        \(func $even (param $x i32) (result i32)\n\
        \get_local $x\n\
        \i32.const 0\n\
        \i32.eq\n\
        \if (result i32)\n\
            \i32.const 1\n\
        \else\n\
            \get_local $x\n\
            \i32.const 1\n\
            \i32.sub\n\
            \call $odd\n\
        \end)\n\
        \(func $odd (param $x i32) (result i32)\n\
        \get_local $x\n\
        \i32.const 0\n\
        \i32.eq\n\
        \if (result i32)\n\
            \i32.const 0\n\
        \else\n\
            \get_local $x\n\
            \i32.const 1\n\
            \i32.sub\n\
            \call $even\n\
        \end))"

    moduleEvenOdd =
        parseWasm Parser.wasmModule programEvenOdd

    testEvenOdd = it "Test complex function calls" $
        property $ \x -> execFunc "$even" [I32Val (x::Integer)] moduleEvenOdd
            `shouldBe` [(boolToWasm (even (x::Integer)))]
