module fffc_terminal
    use fffc_utils
    use, intrinsic :: iso_c_binding, only: ccr => c_carriage_return
    private
    character(*), parameter :: colors(*) = [achar(27)//'[31m', &
                                            achar(27)//'[32m', &
                                            achar(27)//'[33m', &
                                            achar(27)//'[34m', &
                                            achar(27)//'[00m']
    type terminal
        logical, private :: use_color
    contains
        procedure :: setup => set_terminal
        procedure :: progress_bar => terminal_progress_bar
        procedure :: info => terminal_info
        procedure :: warning => terminal_warning
        procedure :: error => terminal_error
        procedure :: success => terminal_success
        procedure :: blue, green, yellow, red
    end type terminal
    type(terminal), public :: terminal_obj
contains
    !> 设置终端输出
    subroutine set_terminal(self)
        class(terminal), intent(out) :: self
        if (.not. is_env('NO_COLOR') .and. &
#ifdef NO_COLOR
            .false. &
#else
            .true. &
#endif
            ) then
            self%use_color = .true.
        else
            self%use_color = .false.
        end if
    end subroutine set_terminal
    !> 信息
    subroutine terminal_info(self, msg)
        class(terminal), intent(in) :: self
        character(*), intent(in) :: msg
        write (*, '(a)') self%blue(msg)
    end subroutine terminal_info
    !> 警告
    subroutine terminal_warning(self, msg)
        class(terminal), intent(in) :: self
        character(*), intent(in) :: msg
        print *, self%yellow('[WARN]  '//msg)
    end subroutine terminal_warning
    !> 错误
    subroutine terminal_error(self, msg)
        class(terminal), intent(in) :: self
        character(*), intent(in) :: msg
        print *, self%red('<ERROR> '//msg)
        stop 1
    end subroutine terminal_error
    !> 成功
    subroutine terminal_success(self, msg)
        class(terminal), intent(in) :: self
        character(*), intent(in) :: msg
        write (*, '(a)') self%green(msg)
    end subroutine terminal_success
    !> 蓝色
    pure function blue(self, msg)
        class(terminal), intent(in) :: self
        character(*), intent(in) :: msg
        character(:), allocatable :: blue
        if (self%use_color) then
            blue = colors(4)//msg//colors(5)
        else
            blue = msg
        end if
    end function blue
    !> 绿色
    pure function green(self, msg)
        class(terminal), intent(in) :: self
        character(*), intent(in) :: msg
        character(:), allocatable :: green
        if (self%use_color) then
            green = colors(2)//msg//colors(5)
        else
            green = msg
        end if
    end function green
    !> 黄色
    pure function yellow(self, msg)
        class(terminal), intent(in) :: self
        character(*), intent(in) :: msg
        character(:), allocatable :: yellow
        if (self%use_color) then
            yellow = colors(3)//msg//colors(5)
        else
            yellow = msg
        end if
    end function yellow
    !> 红色
    pure function red(self, msg)
        class(terminal), intent(in) :: self
        character(*), intent(in) :: msg
        character(:), allocatable :: red
        if (self%use_color) then
            red = colors(1)//msg//colors(5)
        else
            red = msg
        end if
    end function red
    !> 进度条
    subroutine terminal_progress_bar(self, msg, p)
        class(terminal), intent(in) :: self
        character(*), intent(in) :: msg
        real, intent(in) :: p
        character(:), allocatable :: bar
        integer :: length_, l
        parameter(l=35)
        length_ = len(msg) + l + 12
        allocate (character(length_) :: bar)
        write (bar, "(2A,A2,A35,A2,F5.1,A2)") ccr, msg, &
            " [", repeat("*", nint(p*l))//repeat("-", l - nint(p*l)), "] ", 100*p, " %"
        write (*, '(a)', advance='no') bar
    end subroutine terminal_progress_bar
end module fffc_terminal
