submodule (fffc_filesystem) fffc_filesystem_unix_path
contains
    module procedure unix_path
        integer :: i
        do i = 1, len(path)
            if (path(i:i) == "\") then
                unix_path(i:i) = "/"
            else
                unix_path(i:i) = path(i:i)
            end if
        end do
    end procedure unix_path
end submodule fffc_filesystem_unix_path
