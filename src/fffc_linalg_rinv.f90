submodule(fffc_linalg) fffc_linalg_rinv
contains

    module procedure rinv_real_kind
    real(kind=fffc_real_kind) :: Ainv(size(x, 1), size(x, 1))

    y = transpose(x)
    Ainv = gemm(x, y)
    call inv(Ainv)
    y = gemm(y, Ainv)

    end procedure rinv_real_kind

    module procedure rinv_complex_kind
    complex(kind=fffc_complex_kind) :: Ainv(size(x, 1), size(x, 1))

    y = transpose(x)
    Ainv = gemm(x, y)
    call inv(Ainv)
    y = gemm(y, Ainv)

    end procedure rinv_complex_kind

end submodule fffc_linalg_rinv
