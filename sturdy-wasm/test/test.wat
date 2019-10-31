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
)
