(func $add (param $a i32) (param $b i32) (result i32)
    get_local $a
    f32.abs
    get_local $b
    i32.add
)