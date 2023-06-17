!> 精度
module fffc_kinds

#ifdef REAL64
    integer, parameter :: fffc_real_kind = kind(0.0d0)
#else
    integer, parameter :: fffc_real_kind = kind(0.0)
#endif
    integer, parameter :: fffc_complex_kind = fffc_real_kind
    integer, parameter :: fffc_int_kind = kind(0)

end module fffc_kinds
