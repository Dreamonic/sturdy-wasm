(func $add (param $x i32) (result i32)
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