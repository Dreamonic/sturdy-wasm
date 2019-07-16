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
        -- testT
        -- testT2
        -- testT3
        -- testT4
        -- testT5
        testT6
        -- testT7
        -- testT8
        -- testNestedBlocks


    -- Tests --

    programT = "(func $add \n\
                \(result i32)\n\
                \i32.const 2\n\
                \i32.const 3\n\
                \i32.add)"

    functionT = parseWasm Parser.function programT

    testT = it "Test simple function" $
        execFunc [] functionT `shouldBe` [I32Val 5]

    programT2 = "(func $add (param $a i32)\n\
            \(result i32)\n\
            \get_local $a\n\
            \i32.const 3\n\
            \i32.add)"

    functionT2 = parseWasm Parser.function programT2

    testT2 = it "Test using local variables" $
        execFunc [I32Val 2] functionT2 `shouldBe` [I32Val 5]

    programT3 = "(func $add (param $a i32)\n\
        \(result i32)\n\
        \i32.const 1\n\
        \set_local $a\n\
        \get_local $a\n\
        \i32.const 3\n\
        \i32.add)"

    functionT3 = parseWasm Parser.function programT3

    testT3 = it "Test setting local variables" $
        execFunc [I32Val 3] functionT3 `shouldBe` [I32Val 4]

    programT4 = "(func $add\n\
        \(result i32)\n\
        \i32.const 1\n\
        \i32.const 1\n\
        \i32.eq)"

    functionT4 = parseWasm Parser.function programT4

    testT4 = it "Test equals" $
        execFunc [] functionT4 `shouldBe` [I32Val 1]

    programT5 = "(func $add\n\
        \(result i32)\n\
        \i32.const 1\n\
        \i32.const 3\n\
        \i32.eq)"

    functionT5 = parseWasm Parser.function programT5

    testT5 = it "Test equals" $
        execFunc [] functionT5 `shouldBe` [I32Val 0]


    programT7 = "(func $add (param $a i32)\n\
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

    functionT7 = parseWasm Parser.function programT7

    testT7 = it "Test block" $
        execFunc [I32Val 3] functionT7 `shouldBe` [I32Val 5]


    programT6 = "(func $add (param $x i32) (result i32)\n\
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

    functionT6 = do
        parseWasm Parser.function programT6

    testT6 = it "Loop test" $
        execFunc [I32Val 0] functionT6 `shouldBe` [I32Val 4]

    programT8 = "(func $add (param $x i32) (result i32)\n\
        \i32.const 0\n\
        \if (result i32)\n\
            \i32.const 2\n\
        \else\n\
            \i32.const 3\n\
        \end\n\
        \)"

    functionT8 = do
        parseWasm Parser.function programT8

    testT8 = it "If else test" $
        execFunc [I32Val 0] functionT8 `shouldBe` [I32Val 3]

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
        parseWasm Parser.function programNestedBlocks

    testNestedBlocks = it "Test nested blocks" $
        execFunc [I32Val (-1)] functionNestedBlocks `shouldBe` [I32Val 0]
