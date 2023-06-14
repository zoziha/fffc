!> 文件系统单元测试
module test_fffc_filesystem

    use testdrive, only: new_unittest, unittest_type, error_type, check
    use fffc_filesystem, only: operator(.join.), countlines
    implicit none

    private
    public :: collect_filesystem

contains

    subroutine collect_filesystem(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        allocate (testsuite, source=[ &
                  new_unittest('join', test_join), &
                  new_unittest('countlins', test_countlines) &
                  ])

    end subroutine collect_filesystem

    subroutine test_join(error)
        type(error_type), allocatable, intent(out) :: error

        call check(error, 'a '.join.'b', 'a/b')

    end subroutine test_join

    subroutine test_countlines(error)
        type(error_type), allocatable, intent(out) :: error

#ifdef FPM
        call check(error, countlines('src/fffc_filesystem_countlines.f90'), 18)
#else
        call check(error, countlines('../src/fffc_filesystem_countlines.f90'), 18)
#endif

    end subroutine test_countlines

end module test_fffc_filesystem
