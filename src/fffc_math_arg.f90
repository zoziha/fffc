submodule(fffc_math) fffc_math_arg
contains

    module procedure arg
    complex(kind=fffc_complex_kind), parameter :: zero = (0.0_fffc_real_kind, 0.0_fffc_real_kind)

    if (z == zero) then
        arg = 0.0_fffc_real_kind
    else
        arg = atan2(z%im, z%re)
    end if

    end procedure arg

end submodule fffc_math_arg
