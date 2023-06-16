submodule(fffc_string) fffc_string_to_upper
contains
    module procedure to_upper
        integer :: i
        do i = 1, len(string)
            select case (string(i:i))
            case ('a':'z')
                upper(i:i) = char(iachar(string(i:i)) - 32)
            case default
                upper(i:i) = string(i:i)
            end select
        end do
    end procedure to_upper
end submodule fffc_string_to_upper
