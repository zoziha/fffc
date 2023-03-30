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
end module fffc_math
