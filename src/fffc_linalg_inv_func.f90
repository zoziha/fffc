submodule(fffc_linalg) fffc_linalg_inv_func
contains
    module procedure inv_func_real_kind
        b = a
        call inv(b)
    end procedure inv_func_real_kind
    module procedure inv_func_complex_kind
        b = a
        call inv(b)
    end procedure inv_func_complex_kind
end submodule fffc_linalg_inv_func
