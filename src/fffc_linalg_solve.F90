#ifdef REAL64
#define sgesv dgesv
#define cgesv zgesv
#endif
submodule(fffc_linalg) fffc_linalg_solve
contains
    module procedure solve_real_kind
        integer :: ipiv(size(a, 1)), info
        call sgesv(size(a, 1), size(b, 2), a, size(a, 1), ipiv, b, size(b, 1), info)
    end procedure solve_real_kind
    module procedure solve_complex_kind
        integer :: ipiv(size(a, 1)), info
        call cgesv(size(a, 1), size(b, 2), a, size(a, 1), ipiv, b, size(b, 1), info)
    end procedure solve_complex_kind
end submodule fffc_linalg_solve