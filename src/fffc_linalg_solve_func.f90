submodule(fffc_linalg) fffc_linalg_solve_func
contains

    module procedure solve_func_real_kind
    real(kind=fffc_real_kind) :: a_local(size(a, 1), size(a, 2))

    x = b
    a_local = a
    call solve(a_local, x)

    end procedure solve_func_real_kind

    module procedure solve_func_complex_kind
    complex(kind=fffc_complex_kind) :: a_local(size(a, 1), size(a, 2))

    x = b
    a_local = a
    call solve(a_local, x)

    end procedure solve_func_complex_kind

end submodule fffc_linalg_solve_func
