!> 时间模块
module fffc_time

    use fffc_kinds
    implicit none

    private
    public :: timer

    !> 计时器类型
    type timer
        integer, private :: seed
    contains
        procedure :: tic, toc, nowtime
    end type timer

contains

    !> 开始计时
    subroutine tic(self)
        class(timer), intent(out) :: self

        call system_clock(self%seed)

    end subroutine tic

    !> 结束计时
    function toc(self)
        class(timer), intent(in) :: self
        real(kind=fffc_real_kind) :: toc
        integer :: time_now, time_rate

        call system_clock(time_now, time_rate)
        toc = real(time_now - self%seed, fffc_real_kind)/time_rate

    end function toc

    !> 获取当前时间
    character(23) function nowtime(self) result(t)
        class(timer), intent(in) :: self
        character(len=8) :: datstr
        character(len=10) :: timstr

        call date_and_time(datstr, timstr)
        t = datstr(1:4)//"-"//datstr(5:6)//"-"//datstr(7:8)//" "// &
            timstr(1:2)//":"//timstr(3:4)//":"//timstr(5:10)

    end function nowtime

end module fffc_time
