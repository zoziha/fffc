submodule (fffc_linalg) fffc_linalg_linv
contains
    module procedure linv_real_kind
        real(kind=fffc_real_kind) :: Ainv(size(x, 1), size(x, 1))
        y = transpose(x)
        Ainv = gemm(y, x)
        call inv(Ainv)
        y = gemm(Ainv, y)
    end procedure linv_real_kind
    module procedure linv_complex_kind
        complex(kind=fffc_complex_kind) :: Ainv(size(x, 1), size(x, 1))
        y = transpose(x)
        Ainv = gemm(y, x)
        call inv(Ainv)
        y = gemm(Ainv, y)
    end procedure linv_complex_kind
end submodule fffc_linalg_linv
