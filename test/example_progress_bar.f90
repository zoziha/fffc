program example_progress_bar

    use fffc_module, only: terminal_obj, CR
    implicit none
    integer :: i

    do i = 1, 10
        write (*, '(2a)', advance='no') CR, terminal_obj%progress_bar(real(i)/10)
        write (*, '(", ", a)', advance='no') terminal_obj%alive_bar()
    end do
    print *, ''

    do i = 1, 10
        write (*, '(2a)', advance='no') CR, terminal_obj%progress_bar(real(i)/10)
        write (*, '(", ", a)', advance='no') terminal_obj%alive_bar()
        print *, ''
    end do

    do i = 1, 10
        call terminal_obj%bar(i, 10)
        print *, ''
    end do

end program example_progress_bar
