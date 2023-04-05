submodule(fffc_random) fffc_random_randn
contains
    module procedure randn
        real(kind=fffc_real_kind) :: u, v
        do
            call random_number(u)
            call random_number(v)
            u = 2.0_fffc_real_kind * u - 1.0_fffc_real_kind
            v = 2.0_fffc_real_kind * v - 1.0_fffc_real_kind
            x = u * u + v * v
            if (x <= 1.0_fffc_real_kind) exit
        end do
        x = mean + u*sqrt(-2.0_fffc_real_kind*log(x)/x)*std
    end procedure randn
end submodule fffc_random_randn
