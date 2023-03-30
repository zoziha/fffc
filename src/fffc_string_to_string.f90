submodule(fffc_string) fffc_string_to_string
contains
    module procedure to_string_real_kind
        character(len=128) :: s
        write (s, "("//fmt//")") real
        string = trim(s)
    end procedure to_string_real_kind
    module procedure to_string_int_kind
        character(len=128) :: s
        write (s, "("//fmt//")") int
        string = trim(s)
    end procedure to_string_int_kind
    module procedure to_string_logical_kind
        character(len=128) :: s
        write (s, "("//fmt//")") logical
        string = trim(s)
    end procedure to_string_logical_kind
end submodule fffc_string_to_string