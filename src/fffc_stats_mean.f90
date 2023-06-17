submodule(fffc_stats) fffc_stats_mean
contains

    module procedure mean_real_kind

    mean_real_kind = sum(x)/size(x)

    end procedure mean_real_kind

    module procedure mean_int_kind

    mean_int_kind = sum(x)/size(x)

    end procedure mean_int_kind

end submodule fffc_stats_mean
