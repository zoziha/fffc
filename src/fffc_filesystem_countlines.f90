submodule(fffc_filesystem) fffc_filesystem_countlines
contains

    module procedure countlines
    integer :: istat, iunit

    open (newunit=iunit, file=file, status='old')
    countlines = 0
    do
        read (iunit, *, iostat=istat)
        if (is_iostat_end(istat)) exit
        countlines = countlines + 1
    end do
    close (iunit)

    end procedure countlines

end submodule fffc_filesystem_countlines
