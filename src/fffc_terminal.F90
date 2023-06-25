!> 终端
module fffc_terminal

    use fffc_utils
    use, intrinsic :: iso_c_binding, only: CR => c_carriage_return
    use fffc_time, only: timer, sec2hms
    use fffc_kinds, only: rk => fffc_real_kind
    implicit none

    private
    public :: terminal, terminal_obj, CR

    character(*), parameter :: colors(*) = [achar(27)//'[31m', &
                                            achar(27)//'[32m', &
                                            achar(27)//'[33m', &
                                            achar(27)//'[34m', &
                                            achar(27)//'[00m']
    !> 终端
    type terminal
        private
        logical :: use_color = .false.
        integer :: len_bar = 23  !! 进度长度
        character(1) :: marker(2) = ["*", "-"]  !! 标志
        integer :: count = 1
    contains
        procedure :: init => set_terminal
        procedure :: progress_bar, alive_bar, bar
        procedure :: info => terminal_info
        procedure :: warning => terminal_warning
        procedure :: error => terminal_error
        procedure :: success => terminal_success
        procedure :: blue, green, yellow, red
    end type terminal

    type(terminal) :: terminal_obj

contains

    !> 设置终端输出
    pure subroutine set_terminal(self, use_color, len_bar, marker)
        class(terminal), intent(inout) :: self
        logical, intent(in), optional :: use_color
        integer, intent(in), optional :: len_bar
        character(1), intent(in), optional :: marker(2)

        if (present(use_color)) self%use_color = use_color
        if (present(len_bar)) self%len_bar = len_bar - 2
        if (present(marker)) self%marker = marker

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
    pure function progress_bar(self, progress) result(bar)
        class(terminal), intent(in) :: self
        real, intent(in) :: progress
        character(:), allocatable :: bar
        real :: progress_

        progress_ = min(1.0, max(0.0, progress))
        associate (pad => nint(progress_*self%len_bar))
            allocate (bar, source="["//repeat(self%marker(1), pad)// &
                      repeat(self%marker(2), self%len_bar - pad)//"]")
        end associate

    end function progress_bar

    !> 动态进度条
    function alive_bar(self) result(bar)
        class(terminal), intent(inout) :: self
        character(1) :: bar
        character(*), parameter :: marker = "|/-\"

        if (self%count == 5) self%count = 1
        bar(1:1) = marker(self%count:self%count)
        self%count = self%count + 1

    end function alive_bar

    !> 进度条
    subroutine bar(self, value, max)
        class(terminal), intent(inout) :: self
        integer, intent(in) :: value, max
        type(timer), save :: tmr  !! 计时器
        integer, save :: value_  !! 上一次的值
        real(rk) :: dt, v

        dt = tmr%toc()
        v = (value - value_)/dt

        associate (eta => (max - value)/v, &
                   progress => real(value)/max)
            value_ = value

#ifdef __INTEL_COMPILER
            write (*, '(2a,1x,a,1x,i0,a,i0,1x,a,i0,a,i0,3a\)') CR, self%progress_bar(progress), &
#else
            write (*, '(2a,1x,a,1x,i0,a,i0,1x,a,i0,a,i0,3a)', advance='no') CR, self%progress_bar(progress), &
#endif
                self%alive_bar(), value, '/', max, &
                '[', nint(progress*100), '%] (', nint(v), '/s, eta: ', sec2hms(eta), ')'

        end associate

        call tmr%tic()

    end subroutine bar

end module fffc_terminal
