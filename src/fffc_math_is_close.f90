submodule(fffc_math) fffc_math_is_close
    use ieee_arithmetic, only: ieee_is_nan
contains
    module procedure is_close
        real(kind=fffc_real_kind) :: abs_tol_, rel_tol_
        real(kind=fffc_real_kind), parameter :: sqrt_eps = sqrt(epsilon(1.0_fffc_real_kind))
        logical :: equal_nan_
        if (present(equal_nan)) then
            equal_nan_ = equal_nan
        else
            equal_nan_ = .false.
        end if
        if (ieee_is_nan(a) .or. ieee_is_nan(b)) then
            close = merge(.true., .false., equal_nan_ .and. ieee_is_nan(a) .and. ieee_is_nan(b))
        else
            if (present(rel_tol)) then
                rel_tol_ = rel_tol
            else
                rel_tol_ = sqrt_eps
            end if

            if (present(abs_tol)) then
                abs_tol_ = abs_tol
            else
                abs_tol_ = 0.0_fffc_real_kind
            end if
            close = abs(a - b) <= max(abs(rel_tol_*max(abs(a), abs(b))), &
                                      abs(abs_tol_))
        end if
    end procedure is_close
end submodule fffc_math_is_close