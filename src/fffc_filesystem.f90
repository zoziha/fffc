module fffc_filesystem
    interface
        module function is_windows()
            logical :: is_windows
        end function is_windows
        module subroutine mkdir(path)
            character(len=*), intent(in) :: path
        end subroutine mkdir
        module subroutine rmdir(path)
            character(len=*), intent(in) :: path
        end subroutine rmdir
        module function is_exist(file, is_directory)
            character(len=*), intent(in) :: file
            logical, intent(in), optional :: is_directory
            logical :: is_exist
        end function is_exist
    end interface
end module fffc_filesystem
