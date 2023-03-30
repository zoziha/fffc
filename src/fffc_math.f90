module fffc_math
    use fffc_kinds
    interface cross_product
        pure module function cross_product_real_kind(v1, v2)
            real(kind=real_kind), intent(in) :: v1(3)
            real(kind=real_kind), intent(in) :: v2(3)
            real(kind=real_kind) :: cross_product_real_kind(3)
        end function cross_product_real_kind
        pure module function cross_product_int_kind(v1, v2)
            integer(kind=int_kind), intent(in) :: v1(3)
            integer(kind=int_kind), intent(in) :: v2(3)
            integer(kind=int_kind) :: cross_product_int_kind(3)
        end function cross_product_int_kind
    end interface cross_product
    interface arange
        pure module function arange_real_kind(start, stop, step) result(ans)
            real(kind=real_kind), intent(in) :: start
            real(kind=real_kind), intent(in) :: stop
            real(kind=real_kind), intent(in), optional :: step
            real(kind=real_kind), allocatable :: ans(:)
        end function arange_real_kind
        pure module function arange_int_kind(start, stop, step) result(ans)
            integer(kind=int_kind), intent(in) :: start
            integer(kind=int_kind), intent(in) :: stop
            integer(kind=int_kind), intent(in), optional :: step
            integer(kind=int_kind), allocatable :: ans(:)
        end function arange_int_kind
    end interface arange
    interface
        elemental module function arg(z)
            complex(kind=complex_kind), intent(in) :: z
            real(kind=real_kind) :: arg
        end function arg
    end interface
end module fffc_math
