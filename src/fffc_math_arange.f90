submodule(fffc_math) fffc_math_arange
contains

    module procedure arange_real_kind
    real(kind=fffc_real_kind) :: step_
    integer :: i

    if (present(step)) then
        step_ = step
    else
        step_ = 1.0_fffc_real_kind
    end if
    allocate (ans, source=[(start + (i - 1)*step_, i=1, floor((stop - start)/step_) + 1)])

    end procedure arange_real_kind

    module procedure arange_int_kind
    integer(kind=fffc_int_kind) :: step_
    integer :: i

    if (present(step)) then
        step_ = step
    else
        step_ = 1_fffc_int_kind
    end if
    allocate (ans, source=[(i, i=start, stop, step_)])

    end procedure arange_int_kind

end submodule fffc_math_arange
