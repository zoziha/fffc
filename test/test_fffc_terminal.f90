!> 终端单元测试
module test_fffc_terminal

    use testdrive, only: new_unittest, unittest_type, error_type, check
    use fffc_terminal, only: terminal_obj
    implicit none

    private
    public :: collect_terminal

contains

    subroutine collect_terminal(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        allocate (testsuite, source=[ &
                  new_unittest('warning', test_warning), &
                  new_unittest('progress_bar', test_progress_bar) &
                  ])

    end subroutine collect_terminal

    subroutine test_warning(error)
        type(error_type), allocatable, intent(out) :: error

        call terminal_obj%init(use_color=.true.)
        call terminal_obj%warning('This is a warning')

    end subroutine test_warning

    subroutine test_progress_bar(error)
        type(error_type), allocatable, intent(out) :: error

        call terminal_obj%bar(11, 10, advance=.false.)

    end subroutine test_progress_bar

end module test_fffc_terminal
