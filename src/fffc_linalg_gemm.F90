#ifdef REAL64
#define sgemm dgemm
#define cgemm zgemm
#endif
submodule(fffc_linalg) fffc_linalg_gemm
contains

    module procedure rrgemm
    integer :: m, n, k

    m = size(a, 1)
    n = size(b, 2)
    k = size(a, 2)
    call sgemm('N', 'N', m, n, k, 1.0_fffc_real_kind, a, m, b, k, 0.0_fffc_real_kind, c, m)

    end procedure rrgemm

    module procedure ccgemm
    integer :: m, n, k

    m = size(a, 1)
    n = size(b, 2)
    k = size(a, 2)
    call cgemm('N', 'N', m, n, k, (1.0_fffc_complex_kind, 0.0_fffc_complex_kind), a, m, b, k, &
               (0.0_fffc_complex_kind, 0.0_fffc_complex_kind), c, m)

    end procedure ccgemm

    module procedure crgemm
    integer :: m, n, k

    m = size(a, 1)
    n = size(b, 2)
    k = size(a, 2)
    call cgemm('N', 'N', m, n, k, (1.0_fffc_complex_kind, 0.0_fffc_complex_kind), a, m, &
               cmplx(b, kind=fffc_complex_kind), k, &
               (0.0_fffc_complex_kind, 0.0_fffc_complex_kind), c, m)

    end procedure crgemm

    module procedure rcgemm
    integer :: m, n, k

    m = size(a, 1)
    n = size(b, 2)
    k = size(a, 2)
    call cgemm('N', 'N', m, n, k, (1.0_fffc_complex_kind, 0.0_fffc_complex_kind), &
               cmplx(a, kind=fffc_complex_kind), m, b, k, &
               (0.0_fffc_complex_kind, 0.0_fffc_complex_kind), c, m)

    end procedure rcgemm

end submodule fffc_linalg_gemm
