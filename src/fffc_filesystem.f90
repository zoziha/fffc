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
    end interface
end module fffc_filesystem
