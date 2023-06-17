!> 统计学
module fffc_stats

    use fffc_kinds
    implicit none

    private
    public :: mean

    interface mean
        pure module function mean_real_kind(x)
            real(kind=fffc_real_kind), intent(in) :: x(:)
            real(kind=fffc_real_kind) :: mean_real_kind
        end function mean_real_kind
        pure module function mean_int_kind(x)
            integer(kind=fffc_int_kind), intent(in) :: x(:)
            integer(kind=fffc_int_kind) :: mean_int_kind
        end function mean_int_kind
    end interface mean

end module fffc_stats
