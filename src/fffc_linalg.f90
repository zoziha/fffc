!> 线性代数
module fffc_linalg

    use iso_fortran_env, only: int8
    use fffc_kinds
    implicit none

    private
    public :: inv, inv_func, linv, rinv, solve, solve_func, diag, gemm

    interface inv
        module subroutine inv_real_kind(a)
            real(kind=fffc_real_kind), intent(inout) :: a(:, :)
        end subroutine inv_real_kind
        module subroutine inv_complex_kind(a)
            complex(kind=fffc_complex_kind), intent(inout) :: a(:, :)
        end subroutine inv_complex_kind
    end interface inv

    interface inv_func
        module function inv_func_real_kind(a) result(b)
            real(kind=fffc_real_kind), intent(in) :: a(:, :)
            real(kind=fffc_real_kind) :: b(size(a, 1), size(a, 2))
        end function inv_func_real_kind
        module function inv_func_complex_kind(a) result(b)
            complex(kind=fffc_complex_kind), intent(in) :: a(:, :)
            complex(kind=fffc_complex_kind) :: b(size(a, 1), size(a, 2))
        end function inv_func_complex_kind
    end interface inv_func

    interface linv
        !! m > n, 列满秩，使用 linv
        module function linv_real_kind(x) result(y)
            real(kind=fffc_real_kind), intent(in) :: x(:, :)
            real(kind=fffc_real_kind) :: y(size(x, 2), size(x, 1))
        end function linv_real_kind
        module function linv_complex_kind(x) result(y)
            complex(kind=fffc_complex_kind), intent(in) :: x(:, :)
            complex(kind=fffc_complex_kind) :: y(size(x, 2), size(x, 1))
        end function linv_complex_kind
    end interface linv

    interface rinv
        !! m < n, 行满秩，使用 rinv
        module function rinv_real_kind(x) result(y)
            real(kind=fffc_real_kind), intent(in) :: x(:, :)
            real(kind=fffc_real_kind) :: y(size(x, 2), size(x, 1))
        end function rinv_real_kind
        module function rinv_complex_kind(x) result(y)
            complex(kind=fffc_complex_kind), intent(in) :: x(:, :)
            complex(kind=fffc_complex_kind) :: y(size(x, 2), size(x, 1))
        end function rinv_complex_kind
    end interface rinv

    interface solve
        module subroutine solve_real_kind(a, b)
            real(kind=fffc_real_kind), intent(inout) :: a(:, :)
            real(kind=fffc_real_kind), intent(inout) :: b(:, :)
        end subroutine solve_real_kind
        module subroutine solve_complex_kind(a, b)
            complex(kind=fffc_complex_kind), intent(inout) :: a(:, :)
            complex(kind=fffc_complex_kind), intent(inout) :: b(:, :)
        end subroutine solve_complex_kind
    end interface solve

    interface solve_func
        module function solve_func_real_kind(a, b) result(x)
            real(kind=fffc_real_kind), intent(in) :: a(:, :)
            real(kind=fffc_real_kind), intent(in) :: b(:, :)
            real(kind=fffc_real_kind) :: x(size(b, 1), size(b, 2))
        end function solve_func_real_kind
        module function solve_func_complex_kind(a, b) result(x)
            complex(kind=fffc_complex_kind), intent(in) :: a(:, :)
            complex(kind=fffc_complex_kind), intent(in) :: b(:, :)
            complex(kind=fffc_complex_kind) :: x(size(b, 1), size(b, 2))
        end function solve_func_complex_kind
    end interface solve_func

    interface diag
        pure module function diag_rank1(v) result(a)
            real(kind=fffc_real_kind), intent(in) :: v(:)
            real(kind=fffc_real_kind) :: a(size(v), size(v))
        end function diag_rank1
        pure module function diag_rank2(a) result(v)
            real(kind=fffc_real_kind), intent(in) :: a(:, :)
            real(kind=fffc_real_kind) :: v(min(size(a, 1), size(a, 2)))
        end function diag_rank2
    end interface diag

    interface gemm
        module function rrgemm(a, b) result(c)
            real(kind=fffc_real_kind), intent(in) :: a(:, :), b(:, :)
            real(kind=fffc_real_kind) :: c(size(a, 1), size(b, 2))
        end function rrgemm
        module function ccgemm(a, b) result(c)
            complex(kind=fffc_complex_kind), intent(in) :: a(:, :), b(:, :)
            complex(kind=fffc_complex_kind) :: c(size(a, 1), size(b, 2))
        end function ccgemm
        module function crgemm(a, b) result(c)
            complex(kind=fffc_complex_kind), intent(in) :: a(:, :)
            real(kind=fffc_real_kind), intent(in) :: b(:, :)
            complex(kind=fffc_complex_kind) :: c(size(a, 1), size(b, 2))
        end function crgemm
        module function rcgemm(a, b) result(c)
            real(kind=fffc_real_kind), intent(in) :: a(:, :)
            complex(kind=fffc_complex_kind), intent(in) :: b(:, :)
            complex(kind=fffc_complex_kind) :: c(size(a, 1), size(b, 2))
        end function rcgemm
    end interface gemm

    interface
        module function det(a) result(ans)
            real(kind=fffc_real_kind), intent(inout) :: a(:, :)
            real(kind=fffc_real_kind) :: ans
        end function det
        pure module function eye(m, n) result(a)
            integer, intent(in) :: m, n
            integer(kind=int8) :: a(m, n)
        end function eye
    end interface

end module fffc_linalg
