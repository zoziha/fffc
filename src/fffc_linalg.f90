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
end module fffc_linalg
