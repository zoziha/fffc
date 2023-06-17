submodule(fffc_filesystem) fffc_filesystem_dirname
contains

    module procedure dirname
    integer :: ik, len_path

    len_path = len_trim(path)
    if (len_path == 0) then
        dir = '.'
    else
        ik = index(unix_path(path), '/', back=.true.)
        if (ik == 0) then
            dir = '.'
        else
            dir = path(1:ik - 1)
        end if
    end if

    end procedure dirname

end submodule fffc_filesystem_dirname
