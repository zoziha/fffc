!> 时间函数单元测试
module test_fffc_io

    use testdrive, only: new_unittest, unittest_type, error_type
    use fffc_io, only: display
    use fffc_kinds, only: rk => fffc_real_kind
    implicit none

    private
    public :: collect_io

contains

    subroutine collect_io(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        allocate (testsuite, source=[ &
                  new_unittest('display', test_display) &
                  ])

    end subroutine collect_io

    subroutine test_display(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: i

        call display(1.0_rk, '1.0:')
        call display(1.0_rk, '1.0:', format='sp,f10.3', unit=6) ! default format is 'es10.3'
        call display([real(rk) ::(i, i=1, 10)], '1:10:')
        call display([real(rk) ::(i, i=1, 10)], header='1:10:', brief=.false.)
        call display(reshape([real(rk) ::(i, i=1, 25)], [5, 5]), '5x5:')
        call display(reshape([real(rk) ::(i, i=1, 36)], [6, 6]), '6x6:', .true.)
        call display(reshape([real(rk) ::(i, i=1, -35, -1)], [6, 6]), '6x6:', .false.)

    end subroutine test_display

end module test_fffc_io
