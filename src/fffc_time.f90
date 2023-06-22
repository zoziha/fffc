!> 时间模块
module fffc_time

    use fffc_kinds, only: rk => fffc_real_kind
    implicit none

    private
    public :: timer, sec2hms, nowtime

    !> 计时器类型
    type timer
        integer, private :: seed
    contains
        procedure :: tic, toc
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
        real(kind=rk) :: toc
        integer :: time_now, time_rate

        call system_clock(time_now, time_rate)
        toc = real(time_now - self%seed, rk)/time_rate

    end function toc

    !> 将秒数转换为时分秒
    pure function sec2hms(sec) result(hms)
        real(kind=rk), intent(in) :: sec
        character(8) :: hms
        integer :: h, m, s

        h = int(sec/3600.0_rk)
        m = int((sec - h*3600.0_rk)/60.0_rk)
        s = int(sec - h*3600.0_rk - m*60.0_rk)
        write(hms, '(i2.2, ":", i2.2, ":", i2.2)') h, m, s

    end function sec2hms

    !> 获取当前时间
    character(23) function nowtime() result(t)
        character(len=8) :: datstr
        character(len=10) :: timstr

        call date_and_time(datstr, timstr)
        t = datstr(1:4)//"-"//datstr(5:6)//"-"//datstr(7:8)//" "// &
            timstr(1:2)//":"//timstr(3:4)//":"//timstr(5:10)

    end function nowtime

end module fffc_time
