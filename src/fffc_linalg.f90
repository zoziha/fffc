module fffc_linalg
    use fffc_kinds
    interface inv
        module subroutine inv_real_kind(a)
            real(kind=real_kind), intent(inout) :: a(:,:)
        end subroutine inv_real_kind
        module subroutine inv_complex_kind(a)
            complex(kind=complex_kind), intent(inout) :: a(:,:)
        end subroutine inv_complex_kind
    end interface inv
    interface solve
        module subroutine solve_real_kind(a, b)
            real(kind=real_kind), intent(inout) :: a(:,:)
            real(kind=real_kind), intent(inout) :: b(:,:)
        end subroutine solve_real_kind
        module subroutine solve_complex_kind(a, b)
            complex(kind=complex_kind), intent(inout) :: a(:,:)
            complex(kind=complex_kind), intent(inout) :: b(:,:)
        end subroutine solve_complex_kind
    end interface solve
    interface
        module function det(a) result(ans)
            real(kind=real_kind), intent(inout) :: a(:,:)
            real(kind=real_kind) :: ans
        end function det
    end interface
end module fffc_linalg
