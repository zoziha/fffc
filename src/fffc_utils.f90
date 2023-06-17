!> 额外的工具函数
module fffc_utils
    use fffc_kinds
    implicit none

    private
    public :: swap, bubble_sort, is_env

    interface swap
        elemental module subroutine swap_int_kind(a, b)
            integer(kind=fffc_int_kind), intent(inout) :: a
            integer(kind=fffc_int_kind), intent(inout) :: b
        end subroutine swap_int_kind
        elemental module subroutine swap_real_kind(a, b)
            real(kind=fffc_real_kind), intent(inout) :: a
            real(kind=fffc_real_kind), intent(inout) :: b
        end subroutine swap_real_kind
    end interface

    interface bubble_sort
        pure module subroutine bubble_sort_real_kind(v)
            real(kind=fffc_real_kind), intent(inout) :: v(:)
        end subroutine bubble_sort_real_kind
        pure module subroutine bubble_sort_int_kind(v)
            integer(kind=fffc_int_kind), intent(inout) :: v(:)
        end subroutine bubble_sort_int_kind
    end interface

    interface
        module function is_env(key)
            character(len=*), intent(in) :: key
            logical :: is_env
        end function is_env
    end interface

end module fffc_utils
