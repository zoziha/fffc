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
        call display([real(rk) ::(i, i=1, 10)], '1:10:')
        call display(reshape([real(rk) ::(i, i=1, 36)], [6, 6]), '6x6:', .true.)
        call display(reshape([real(rk) ::(i, i=1, 36)], [6, 5]), '6x5:', .false.)
        call display(reshape([real(rk) ::(i, i=1, 25)], [5, 5]), '5x5:')

    end subroutine test_display

end module test_fffc_io
