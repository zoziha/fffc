submodule(fffc_filesystem) fffc_filesystem_join
contains
    module procedure join
        join = trim(path)//"/"//trim(name)
    end procedure join
end submodule fffc_filesystem_join
