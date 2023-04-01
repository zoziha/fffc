submodule(fffc_linalg) fffc_linalg_diag
contains
    module procedure diag_rank1
        integer :: i, j
        do j = 1, size(v)
            do i = 1, size(v)
                if (i == j) then
                    a(i, j) = v(i)
                else
                    a(i, j) = 0.0_fffc_real_kind
                end if
            end do
        end do
    end procedure diag_rank1
    module procedure diag_rank2
        integer :: i
        do i = 1, size(v)
            v(i) = a(i, i)
        end do
    end procedure diag_rank2
end submodule fffc_linalg_diag