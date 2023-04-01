#ifdef REAL64
#define sgetrf dgetrf
#endif
submodule(fffc_linalg) fffc_linalg_det
contains
    module procedure det
        integer :: ipiv(size(a, 1)), info, i, count
        call sgetrf(size(a, 1), size(a, 2), a, size(a, 1), ipiv, info)
        ans = 1.0_fffc_real_kind
        do i = 1, size(a, 1)
            ans = ans * a(i, i)
        end do
        count = 0
        do i = 1, size(a, 1)
            if (ipiv(i) /= i) then
                count = count + 1
            end if
        end do
        if (mod(count, 2) == 1) ans = -ans
    end procedure det
end submodule fffc_linalg_det