(module
    (func $add (param $a i32) (param $b i32) (result i32)
        get_local $a
        get_local $b
        i32.add
    )

    (func $id (param $x i32) (result i32)
        get_local $x
    )

    (func $sub (param $a i32) (param $b i32) (result i32)
        get_local $b
        i32.const -1
        i32.mul
        get_local $a
        call $add
    )

    (func $faulty (result i32)
        get_local $x
    )

    (func $loop4 (param $x i32) (result i32)
        block
            loop
                get_local $x
                i32.const 1
                i32.add
                set_local $x
                get_local $x
                i32.const 4
                i32.eq
                br_if 1
                br 0
            end
        end
        get_local $x
    )
)
