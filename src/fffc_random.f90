module fffc_random
    use fffc_kinds
    use random
    private
    public :: randn, randu
    interface
        impure elemental module subroutine randn(x, mean, std)
            real(kind=fffc_real_kind), intent(out) :: x
            real(kind=fffc_real_kind), intent(in) :: mean, std
        end subroutine randn
        impure elemental module subroutine randu(x, min, max)
            real(kind=fffc_real_kind), intent(out) :: x
            real(kind=fffc_real_kind), intent(in) :: min, max
        end subroutine randu
    end interface
end module fffc_random
