submodule(fffc_string) fffc_string_to_lower
contains

    module procedure to_lower
    integer :: i

    do i = 1, len(string)
        select case (string(i:i))
        case ('A':'Z')
            lower(i:i) = char(iachar(string(i:i)) + 32)
        case default
            lower(i:i) = string(i:i)
        end select
    end do

    end procedure to_lower

end submodule fffc_string_to_lower
