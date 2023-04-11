module fffc_time
    use fffc_kinds
    type timer
        integer, private :: seed
    contains
        procedure :: tic, toc, nowtime
    end type timer
contains
    subroutine tic(self)
        class(timer), intent(out) :: self
        call system_clock(self%seed)
    end subroutine tic
    function toc(self)
        class(timer), intent(in) :: self
        real(kind=fffc_real_kind) :: toc
        integer :: time_now, time_rate
        call system_clock(time_now, time_rate)
        toc = real(time_now - self%seed, fffc_real_kind)/time_rate
    end function toc
    character(23) function nowtime(self) result(t)
        class(timer), intent(in) :: self
        character(len=8) :: datstr
        character(len=10) :: timstr
        call date_and_time(datstr, timstr)
        t = datstr(1:4)//"-"//datstr(5:6)//"-"//datstr(7:8)//" "// &
            timstr(1:2)//":"//timstr(3:4)//":"//timstr(5:10)
    end function nowtime
end module fffc_time
