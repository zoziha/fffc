!> I/O
module fffc_io

    use fffc_kinds, only: rk => fffc_real_kind
    use fffc_string, only: to_string
    use, intrinsic :: iso_c_binding, only: nl => c_new_line
    implicit none

    private
    public :: display

    !> 在屏幕输出浮点型数组, 支持标量, 向量, 矩阵
    interface display
        module procedure :: display_0d
        module procedure :: display_1d
        module procedure :: display_2d
    end interface display

contains

    subroutine display_0d(re, header, brief, format)
        real(rk), intent(in) :: re
        character(*), intent(in), optional :: header
        logical, intent(in), optional :: brief
        character(*), intent(in), optional :: format
        logical :: brief_
        character(:), allocatable :: str
        character(:), allocatable :: header_
        character(:), allocatable :: format_

        if (present(format)) then
            format_ = format
        else
            format_ = 'es10.3'
        end if

        if (present(brief)) then
            brief_ = brief
        else
            brief_ = .true.
        end if

        if (present(header)) then
            header_ = header
        else
            header_ = ''
        end if

        str = '[scalar] '//header_//nl
        str = str//to_string(re, format_)

        write (*, '(a)') str

    end subroutine display_0d

    subroutine display_1d(re, header, brief, format)
        real(rk), intent(in) :: re(:)
        character(*), intent(in), optional :: header
        logical, intent(in), optional :: brief
        character(*), intent(in), optional :: format
        logical :: brief_
        character(:), allocatable :: str
        character(:), allocatable :: header_
        character(:), allocatable :: format_

        if (present(format)) then
            format_ = format
        else
            format_ = 'es10.3'
        end if

        if (present(brief)) then
            brief_ = brief
        else
            brief_ = .true.
        end if

        if (present(header)) then
            header_ = header
        else
            header_ = ''
        end if

        str = '[vector: '//to_string(size(re), 'i0')//'] '//header_//nl
        call vector_string(re, brief_, format_, str)

        write (*, '(a)') str

    end subroutine display_1d

    subroutine display_2d(re, header, brief, format)
        real(rk), intent(in) :: re(:, :)
        character(*), intent(in), optional :: header
        logical, intent(in), optional :: brief
        character(*), intent(in), optional :: format
        logical :: brief_
        character(:), allocatable :: str
        character(:), allocatable :: header_
        character(:), allocatable :: format_
        integer :: i

        if (present(format)) then
            format_ = format
        else
            format_ = 'es10.3'
        end if

        if (present(brief)) then
            brief_ = brief
        else
            brief_ = .true.
        end if

        if (present(header)) then
            header_ = header
        else
            header_ = ''
        end if

        str = '[matrix: '//to_string(size(re, 1), 'i0')//'*'//to_string(size(re, 2), 'i0')//'] '//header_//nl
        if (brief_ .and. size(re, 1) > 5) then
            do i = 1, 3
                call vector_string(re(i, :), brief_, format_, str)
                str = str//";"//nl
            end do
            str = str//' : '//nl
            call vector_string(re(size(re, 1), :), brief_, format_, str)
        else
            do i = 1, size(re, 1) - 1
                call vector_string(re(i, :), brief_, format_, str)
                str = str//";"//nl
            end do
            call vector_string(re(size(re, 1), :), brief_, format_, str)
        end if

        write (*, '(a)') str

    end subroutine display_2d

    subroutine vector_string(x, brief, format, string)
        real(rk), intent(in) :: x(:)
        logical, intent(in) :: brief
        character(*), intent(in) :: format
        character(:), allocatable, intent(inout) :: string
        integer :: i

        if (brief) then
            string = string//to_string(x(1), format)//', '//to_string(x(2), format)//', '// &
                     to_string(x(3), format)//', ... '//to_string(x(size(x)), format)
        else
            string = string//to_string(x(1), format)
            do i = 2, size(x)
                string = string//', '//to_string(x(i), format)
            end do
        end if

    end subroutine vector_string

end module fffc_io
