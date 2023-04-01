module fffc_linalg
    use iso_fortran_env, only: int8
    use fffc_kinds
    interface inv
        module subroutine inv_real_kind(a)
            real(kind=fffc_real_kind), intent(inout) :: a(:,:)
        end subroutine inv_real_kind
        module subroutine inv_complex_kind(a)
            complex(kind=fffc_complex_kind), intent(inout) :: a(:,:)
        end subroutine inv_complex_kind
    end interface inv
    interface solve
        module subroutine solve_real_kind(a, b)
            real(kind=fffc_real_kind), intent(inout) :: a(:,:)
            real(kind=fffc_real_kind), intent(inout) :: b(:,:)
        end subroutine solve_real_kind
        module subroutine solve_complex_kind(a, b)
            complex(kind=fffc_complex_kind), intent(inout) :: a(:,:)
            complex(kind=fffc_complex_kind), intent(inout) :: b(:,:)
        end subroutine solve_complex_kind
    end interface solve
    interface diag
        pure module function diag_rank1(v) result(a)
            real(kind=fffc_real_kind), intent(in) :: v(:)
            real(kind=fffc_real_kind) :: a(size(v),size(v))
        end function diag_rank1
        pure module function diag_rank2(a) result(v)
            real(kind=fffc_real_kind), intent(in) :: a(:,:)
            real(kind=fffc_real_kind) :: v(min(size(a,1),size(a,2)))
        end function diag_rank2
    end interface diag
    interface
        module function det(a) result(ans)
            real(kind=fffc_real_kind), intent(inout) :: a(:,:)
            real(kind=fffc_real_kind) :: ans
        end function det
        pure module function eye(m, n) result(a)
            integer, intent(in) :: m, n
            integer(kind=int8) :: a(m,n)
        end function eye
    end interface
end module fffc_linalg
