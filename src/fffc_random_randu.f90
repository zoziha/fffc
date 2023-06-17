submodule(fffc_random) fffc_random_randu
contains

    module procedure randu

    call random_number(x)
    x = min + x*(max - min)

    end procedure randu

end submodule fffc_random_randu
