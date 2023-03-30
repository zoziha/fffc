submodule(fffc_linalg) fffc_linalg_inv
contains
    module procedure inv_real_kind
        integer :: ipiv(size(a, 1)), info
        real(kind=real_kind) :: work(size(a, 2))
        call dgetrf(size(a, 1), size(a, 2), a, size(a, 1), ipiv, info)
        call dgetri(size(a, 2), a, size(a, 1), ipiv, work, size(work), info)
    end procedure inv_real_kind
    module procedure inv_complex_kind
        integer :: ipiv(size(a, 1)), info
        complex(kind=complex_kind) :: work(size(a, 2))
        call zgetrf(size(a, 1), size(a, 2), a, size(a, 1), ipiv, info)
        call zgetri(size(a, 2), a, size(a, 1), ipiv, work, size(work), info)
    end procedure inv_complex_kind
end submodule fffc_linalg_inv