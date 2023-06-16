submodule(fffc_filesystem) fffc_filesystem_exists
contains
    module procedure exists
#if defined __INTEL_COMPILER
        if (present(is_directory)) then
            if (is_directory) then
                inquire (directory=file, exist=exists)
                return
            end if
        end if
#endif
        inquire (file=file, exist=exists)
    end procedure exists
end submodule fffc_filesystem_exists
