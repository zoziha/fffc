!> 时间函数单元测试
module test_fffc_time

    use testdrive, only: new_unittest, unittest_type, error_type, check
    use fffc_time, only: timer, sec2hms
    use fffc_kinds, only: rk => fffc_real_kind
    implicit none

    private
    public :: collect_time

contains

    subroutine collect_time(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        allocate (testsuite, source=[ &
                  new_unittest('timer', test_timer), &
                  new_unittest('sec2hms', test_sec2hms) &
                  ])

    end subroutine collect_time

    subroutine test_timer(error)
        type(error_type), allocatable, intent(out) :: error
        type(timer) :: t

        call t%tic()
        call check(error, t%toc() < 10.0_rk)

    end subroutine test_timer

    subroutine test_sec2hms(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, sec2hms(8278.0_rk), '02:17:58')

    end subroutine test_sec2hms

end module test_fffc_time
