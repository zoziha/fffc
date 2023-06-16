submodule(fffc_filesystem) fffc_filesystem_is_windows
contains
    module procedure is_windows
        character(16) :: os_name
        logical, save :: is_windows_ = .false.
        logical, save :: is_first_run = .true.
        if (is_first_run) then
            call get_environment_variable("OS", os_name)
            is_windows_ = trim(os_name) == "Windows_NT"
            is_first_run = .false.
            is_windows = is_windows_
        else
            is_windows = is_windows_
        end if
    end procedure is_windows
end submodule fffc_filesystem_is_windows