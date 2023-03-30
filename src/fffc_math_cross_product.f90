submodule(fffc_math) fffc_math_cross_product
contains
    module procedure cross_product_real_kind
        cross_product_real_kind(1) = v1(2)*v2(3) - v1(3)*v2(2)
        cross_product_real_kind(2) = v1(3)*v2(1) - v1(1)*v2(3)
        cross_product_real_kind(3) = v1(1)*v2(2) - v1(2)*v2(1)
    end procedure cross_product_real_kind
    module procedure cross_product_int_kind
        cross_product_int_kind(1) = v1(2)*v2(3) - v1(3)*v2(2)
        cross_product_int_kind(2) = v1(3)*v2(1) - v1(1)*v2(3)
        cross_product_int_kind(3) = v1(1)*v2(2) - v1(2)*v2(1)
    end procedure cross_product_int_kind
end submodule fffc_math_cross_product
