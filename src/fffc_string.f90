module fffc_string
    use fffc_kinds
    interface to_string
        module function to_string_real_kind(real, fmt) result(string)
            real(kind=real_kind), intent(in) :: real
            character(len=*), intent(in) :: fmt
            character(len=:), allocatable :: string
        end function to_string_real_kind
        module function to_string_int_kind(int, fmt) result(string)
            integer(kind=int_kind), intent(in) :: int
            character(len=*), intent(in) :: fmt
            character(len=:), allocatable :: string
        end function to_string_int_kind
        module function to_string_logical_kind(logical, fmt) result(string)
            logical, intent(in) :: logical
            character(len=*), intent(in) :: fmt
            character(len=:), allocatable :: string
        end function to_string_logical_kind
    end interface to_string
end module fffc_string
