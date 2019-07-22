module MonadicSystemSpec (spec) where

    import Test.QuickCheck
    import Test.Hspec
    import Lexer
    import Parser
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
        -- testLoop
        -- testBlock
        -- testIfElse
        -- testNestedBlocks


    -- Tests --

    programSimpleFunction = "(func $add \n\
                \(result i32)\n\
                \i32.const 2\n\
                \i32.const 3\n\
                \i32.add)"

    functionSimpleFunction = parseFunc Parser.function programSimpleFunction

    testSimpleFunction = it "Test simple function" $
        execFunc [] functionSimpleFunction `shouldBe` [I32Val 5]

    programReadLocalVars = "(func $add (param $a i32)\n\
            \(result i32)\n\
            \get_local $a\n\
            \i32.const 3\n\
            \i32.add)"

    functionReadLocalVars = parseFunc Parser.function programReadLocalVars

    testReadLocalVars = it "Test using local variables" $
        execFunc [I32Val 2] functionReadLocalVars `shouldBe` [I32Val 5]

    programSetLocalVars = "(func $add (param $a i32)\n\
        \(result i32)\n\
        \i32.const 1\n\
        \set_local $a\n\
        \get_local $a\n\
        \i32.const 3\n\
        \i32.add)"

    functionSetLocalVars = parseFunc Parser.function programSetLocalVars

    testSetLocalVars = it "Test setting local variables" $
        execFunc [I32Val 3] functionSetLocalVars `shouldBe` [I32Val 4]

    programEquals = "(func $add\n\
        \(result i32)\n\
        \i32.const 1\n\
        \i32.const 1\n\
        \i32.eq)"

    functionEquals = parseFunc Parser.function programEquals

    testEquals = it "Test equals" $
        execFunc [] functionEquals `shouldBe` [I32Val 1]

    programEqualsFalse = "(func $add\n\
        \(result i32)\n\
        \i32.const 1\n\
        \i32.const 3\n\
        \i32.eq)"

    functionEqualsFalse = parseFunc Parser.function programEqualsFalse

    testEqualsFalse = it "Test equals" $
        execFunc [] functionEqualsFalse `shouldBe` [I32Val 0]


    programBlock = "(func $add (param $a i32)\n\
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
        \i32.add)"

    functionBlock = parseFunc Parser.function programBlock

    testBlock = it "Test block" $
        execFunc [I32Val 3] functionBlock `shouldBe` [I32Val 5]


    programLoop = "(func $add (param $x i32) (result i32)\n\
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
        \)"

    functionLoop = do
        parseFunc Parser.function programLoop

    testLoop = it "Loop test" $
        execFunc [I32Val 0] functionLoop `shouldBe` [I32Val 4]

    programIfElse = "(func $add (param $x i32) (result i32)\n\
        \i32.const 0\n\
        \if (result i32)\n\
            \i32.const 2\n\
        \else\n\
            \i32.const 3\n\
        \end\n\
        \)"

    functionIfElse = do
        parseFunc Parser.function programIfElse

    testIfElse = it "If else test" $
        execFunc [I32Val 0] functionIfElse `shouldBe` [I32Val 3]

    programNestedBlocks = "(func $add (param $x i32) (result i32)\n\
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
        \)"

    functionNestedBlocks =
        parseFunc Parser.function programNestedBlocks

    testNestedBlocks = it "Test nested blocks" $
        execFunc [I32Val (-1)] functionNestedBlocks `shouldBe` [I32Val 0]
