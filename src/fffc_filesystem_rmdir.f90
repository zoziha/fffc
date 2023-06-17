submodule(fffc_filesystem) fffc_filesystem_rmdir
contains

    module procedure rmdir

    if (is_windows()) then
        call execute_command_line("rd /s /q "//path)
    else
        call execute_command_line("rm -rf "//path)
    end if

    end procedure rmdir

end submodule fffc_filesystem_rmdir
