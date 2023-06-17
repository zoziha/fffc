!> 数学
module fffc_math

    use fffc_kinds
    implicit none

    private
    public :: cross_product, arange, diff, linspace, arg, is_close

    interface cross_product
        pure module function cross_product_real_kind(v1, v2) result(ans)
            real(kind=fffc_real_kind), intent(in) :: v1(3)
            real(kind=fffc_real_kind), intent(in) :: v2(3)
            real(kind=fffc_real_kind) :: ans(3)
        end function cross_product_real_kind
        pure module function cross_product_int_kind(v1, v2) result(ans)
            integer(kind=fffc_int_kind), intent(in) :: v1(3)
            integer(kind=fffc_int_kind), intent(in) :: v2(3)
            integer(kind=fffc_int_kind) :: ans(3)
        end function cross_product_int_kind
    end interface cross_product

    interface arange
        pure module function arange_real_kind(start, stop, step) result(ans)
            real(kind=fffc_real_kind), intent(in) :: start
            real(kind=fffc_real_kind), intent(in) :: stop
            real(kind=fffc_real_kind), intent(in), optional :: step
            real(kind=fffc_real_kind), allocatable :: ans(:)
        end function arange_real_kind
        pure module function arange_int_kind(start, stop, step) result(ans)
            integer(kind=fffc_int_kind), intent(in) :: start
            integer(kind=fffc_int_kind), intent(in) :: stop
            integer(kind=fffc_int_kind), intent(in), optional :: step
            integer(kind=fffc_int_kind), allocatable :: ans(:)
        end function arange_int_kind
    end interface arange

    interface diff
        pure module function diff_real_kind(x, n, prepend, append) result(y)
            real(kind=fffc_real_kind), intent(in) :: x(:)
            integer, intent(in), optional :: n
            real(kind=fffc_real_kind), intent(in), optional :: prepend(:), append(:)
            real(kind=fffc_real_kind), allocatable :: y(:)
        end function diff_real_kind
        pure module function diff_int_kind(x, n, prepend, append) result(y)
            integer(kind=fffc_int_kind), intent(in) :: x(:)
            integer, intent(in), optional :: n
            integer(kind=fffc_int_kind), intent(in), optional :: prepend(:), append(:)
            integer(kind=fffc_int_kind), allocatable :: y(:)
        end function diff_int_kind
    end interface diff

    interface linspace
        pure module function linspace_real_kind(start, stop, n) result(ans)
            real(kind=fffc_real_kind), intent(in) :: start
            real(kind=fffc_real_kind), intent(in) :: stop
            integer, intent(in) :: n
            real(kind=fffc_real_kind) :: ans(max(0, n))
        end function linspace_real_kind
        pure module function linspace_int_kind(start, stop, n) result(ans)
            integer(kind=fffc_int_kind), intent(in) :: start
            integer(kind=fffc_int_kind), intent(in) :: stop
            integer, intent(in) :: n
            integer(kind=fffc_int_kind) :: ans(max(0, n))
        end function linspace_int_kind
    end interface linspace

    interface
        elemental module function arg(z)
            complex(kind=fffc_complex_kind), intent(in) :: z
            real(kind=fffc_real_kind) :: arg
        end function arg
        elemental module function is_close(a, b, rel_tol, abs_tol, equal_nan) result(close)
            real(kind=fffc_real_kind), intent(in) :: a, b
            real(kind=fffc_real_kind), intent(in), optional :: rel_tol, abs_tol
            logical, intent(in), optional :: equal_nan
            logical :: close
        end function is_close
    end interface

end module fffc_math
