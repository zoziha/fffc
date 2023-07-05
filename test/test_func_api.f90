!> author: 左志华
!> date: 2022-07-16
!>
!> 单元测试
program test_func_api

    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_fffc_filesystem, only: collect_filesystem
    use test_fffc_time, only: collect_time
    use test_fffc_io, only: collect_io
    use test_fffc_terminal, only: collect_terminal
    implicit none

    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    allocate (testsuites, source=[ &
              new_testsuite("filesystem", collect_filesystem), &
              new_testsuite("io", collect_io), &
              new_testsuite("terminal", collect_terminal), &
              new_testsuite("time", collect_time) &
              ])

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Running testsuite:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    else
        write (error_unit, '(a)') "All tests passed!"
    end if

end program test_func_api
