module fffc_kinds
#ifdef REAL64
    integer, parameter :: real_kind = kind(0.0d0)
#else
    integer, parameter :: real_kind = kind(0.0)
#endif
    integer, parameter :: int_kind = kind(0)
end module fffc_kinds
