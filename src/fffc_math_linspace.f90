submodule(fffc_math) fffc_math_linspace
contains

    module procedure linspace_real_kind
    real(kind=fffc_real_kind) :: step
    integer :: i

    if (n <= 0) then
        return
    elseif (n == 1) then
        ans(1) = start
        return
    end if
    step = (stop - start)/real(n - 1, kind=fffc_real_kind)
    do i = 1, n
        ans(i) = start + real(i - 1, kind=fffc_real_kind)*step
    end do

    end procedure linspace_real_kind

    module procedure linspace_int_kind
    integer(kind=fffc_int_kind) :: step
    integer :: i

    if (n <= 0) then
        return
    elseif (n == 1) then
        ans(1) = start
        return
    end if
    step = (stop - start)/(n - 1)
    do i = 1, n
        ans(i) = start + (i - 1)*step
    end do

    end procedure linspace_int_kind

end submodule fffc_math_linspace
