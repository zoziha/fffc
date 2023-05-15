program example_linv

    use fffc_module, only: linv, fffc_real_kind
    implicit none

    real(fffc_real_kind) :: x(3, 2) = reshape([1, 2, 3, 4, 5, 6], [3, 2])

    print *, linv(x)

end program example_linv
