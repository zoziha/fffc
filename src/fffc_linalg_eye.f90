submodule(fffc_linalg) fffc_linalg_eye
contains
    module procedure eye
        integer :: i, j
        do j = 1, n
            do i = 1, m
                if (i==j) then
                    a(i,j) = 1_int8
                else
                    a(i,j) = 0_int8
                endif
            enddo
        enddo
    end procedure eye
end submodule fffc_linalg_eye