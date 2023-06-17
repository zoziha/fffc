submodule(fffc_utils) fffc_utils_swap
contains

    module procedure swap_int_kind
    integer(kind=fffc_int_kind) :: tmp

    tmp = a
    a = b
    b = tmp

    end procedure swap_int_kind
    module procedure swap_real_kind
    real(kind=fffc_real_kind) :: tmp

    tmp = a
    a = b
    b = tmp

    end procedure swap_real_kind

end submodule fffc_utils_swap
