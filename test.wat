(func $add (param $a i32) (param $b i32) (result i32)
    block
        i32.const 3
        set_local $a
    end
    get_local $a
    get_local $b
    i32.add
)