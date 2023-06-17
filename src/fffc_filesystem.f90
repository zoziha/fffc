!> 文件系统
module fffc_filesystem

    implicit none

    private
    public :: is_windows, mkdir, rmdir, exists, countlines, unix_path, &
              dirname, basename, operator(.join.)

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
        module function exists(file, is_directory)
            character(len=*), intent(in) :: file
            logical, intent(in), optional :: is_directory
            logical :: exists
        end function exists
        integer module function countlines(file)
            character(len=*), intent(in) :: file
        end function countlines
        pure module function unix_path(path)
            character(len=*), intent(in) :: path
            character(len=len(path)) :: unix_path
        end function unix_path
        pure module function dirname(path) result(dir)
            character(len=*), intent(in) :: path
            character(len=:), allocatable :: dir
        end function dirname
        pure module function basename(path) result(base)
            character(len=*), intent(in) :: path
            character(len=:), allocatable :: base
        end function basename
    end interface

    interface operator(.join.)
        module function join(path, name)
            character(len=*), intent(in) :: path
            character(len=*), intent(in) :: name
            character(len=len(path) + len(name) + 1) :: join
        end function join
    end interface

end module fffc_filesystem
