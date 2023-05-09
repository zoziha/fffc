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
        integer module function countlines(file)
            character(len=*), intent(in) :: file
        end function countlines
    end interface
    interface operator(.join.)
        module function join(path, name)
            character(len=*), intent(in) :: path
            character(len=*), intent(in) :: name
            character(len=len(path)+len(name)+1) :: join
        end function join
    end interface
end module fffc_filesystem
