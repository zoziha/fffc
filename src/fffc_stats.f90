module fffc_stats
    use fffc_kinds
    interface mean
        pure module function mean_real_kind(x)
            real(kind=real_kind), intent(in) :: x(:)
            real(kind=real_kind) :: mean_real_kind
        end function mean_real_kind
        pure module function mean_int_kind(x)
            integer(kind=int_kind), intent(in) :: x(:)
            integer(kind=int_kind) :: mean_int_kind
        end function mean_int_kind
    end interface mean
end module fffc_stats