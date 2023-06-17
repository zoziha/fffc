submodule(fffc_utils) fffc_utils_is_env
contains

    module procedure is_env
    integer :: stat
    character(len=64) :: value

    call get_environment_variable(key, value, status=stat)
    if (stat == 0) then
        is_env = .true.
    else
        is_env = .false.
    end if

    end procedure is_env

end submodule fffc_utils_is_env
