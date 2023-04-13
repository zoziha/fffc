submodule(fffc_filesystem) fffc_filesystem_join
contains
    module procedure join
        join = path//"/"//name
    end procedure join
end submodule fffc_filesystem_join
