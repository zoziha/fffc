submodule(fffc_filesystem) fffc_filesystem_is_exist
contains
    module procedure is_exist
#if defined __INTEL_COMPILER
        if (present(is_directory)) then
            if (is_directory) then
                inquire (directory=file, exist=is_exist)
                return
            end if
        end if
#endif
        inquire (file=file, exist=is_exist)
    end procedure is_exist
end submodule fffc_filesystem_is_exist
