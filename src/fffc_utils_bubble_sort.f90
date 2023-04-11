submodule(fffc_utils) fffc_utils_bubble_sort
contains
    module procedure bubble_sort_real_kind
        integer :: i, j, n
        n = size(v)
        do i = 1, n
            do j = 1, n
                if (v(i) > v(j)) then
                    call swap(v(i), v(j))
                end if
            end do
        end do
    end procedure bubble_sort_real_kind
    module procedure bubble_sort_int_kind
        integer :: i, j, n
        n = size(v)
        do i = 1, n
            do j = 1, n
                if (v(i) > v(j)) then
                    call swap(v(i), v(j))
                end if
            end do
        end do
    end procedure bubble_sort_int_kind
end submodule fffc_utils_bubble_sort
