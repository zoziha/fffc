submodule (fffc_filesystem) fffc_filesystem_basename
contains
    module procedure basename
        integer :: ik, len_path
        len_path = len_trim(path)
        if (len_path == 0) then
            base = '.'
        else
            ik = index(unix_path(path), '/', back=.true.)
            if (ik == 0) then
                base = path
            else
                base = path(ik+1:len_path)
            end if
        end if
    end procedure basename
end submodule fffc_filesystem_basename
