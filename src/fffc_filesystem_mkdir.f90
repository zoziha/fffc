submodule(fffc_filesystem) fffc_filesystem_mkdir
contains

    module procedure mkdir

    if (is_windows()) then
        call execute_command_line("md "//path)
    else
        call execute_command_line("mkdir -p "//path)
    end if

    end procedure mkdir

end submodule fffc_filesystem_mkdir
