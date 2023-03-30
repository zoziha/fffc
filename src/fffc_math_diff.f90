submodule(fffc_math) fffc_math_diff
contains
    module procedure diff_real_kind
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, i
        if (present(n)) then
            n_ = n
        else
            n_ = 1
        end if
        if (n_ <= 0) then
            y = x
            return
        end if
        size_prepend = 0
        size_append = 0
        if (present(prepend)) size_prepend = size(prepend)
        if (present(append)) size_append = size(append)
        size_x = size(x)
        size_work = size_x + size_prepend + size_append
        if (size_work <= n_) then
            allocate (y(0))
            return
        end if
        !> Use a quick exit for the common case, to avoid memory allocation.
        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            y = x(2:) - x(1:size_x - 1)
            return
        end if
        block
            real(kind=real_kind) :: work(size_work)
            if (size_prepend > 0) work(:size_prepend) = prepend
            work(size_prepend + 1:size_prepend + size_x) = x
            if (size_append > 0) work(size_prepend + size_x + 1:) = append
            do i = 1, n_
                work(1:size_work - i) = work(2:size_work - i + 1) - work(1:size_work - i)
            end do
            y = work(1:size_work - n_)
        end block
    end procedure diff_real_kind
    module procedure diff_int_kind
        integer :: size_prepend, size_append, size_x, size_work
        integer :: n_, i
        if (present(n)) then
            n_ = n
        else
            n_ = 1
        end if
        if (n_ <= 0) then
            y = x
            return
        end if
        size_prepend = 0
        size_append = 0
        if (present(prepend)) size_prepend = size(prepend)
        if (present(append)) size_append = size(append)
        size_x = size(x)
        size_work = size_x + size_prepend + size_append
        if (size_work <= n_) then
            allocate (y(0))
            return
        end if
        !> Use a quick exit for the common case, to avoid memory allocation.
        if (size_prepend == 0 .and. size_append == 0 .and. n_ == 1) then
            y = x(2:) - x(1:size_x - 1)
            return
        end if
        block
            integer(kind=int_kind) :: work(size_work)
            if (size_prepend > 0) work(:size_prepend) = prepend
            work(size_prepend + 1:size_prepend + size_x) = x
            if (size_append > 0) work(size_prepend + size_x + 1:) = append
            do i = 1, n_
                work(1:size_work - i) = work(2:size_work - i + 1) - work(1:size_work - i)
            end do
            y = work(1:size_work - n_)
        end block
    end procedure diff_int_kind
end submodule fffc_math_diff