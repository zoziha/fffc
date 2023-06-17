submodule(fffc_random) fffc_random_randn
contains

    module procedure randn

    x = mean + std*random_normal()

    end procedure randn

end submodule fffc_random_randn
