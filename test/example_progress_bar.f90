program example_progress_bar

    use fffc_module, only: terminal_obj, stdout_flush, stdout_newline
    implicit none
    integer :: i

    do i = 1, 10
        call stdout_flush()
        write (*, '(a)', advance='no') terminal_obj%progress_bar(real(i)/10)
        write (*, '(", ", a)', advance='no') terminal_obj%alive_bar()
    end do
    call stdout_newline()

    do i = 1, 10
        call stdout_flush()
        write (*, '(a)', advance='no') terminal_obj%progress_bar(real(i)/10)
        write (*, '(", ", a)', advance='no') terminal_obj%alive_bar()
        call stdout_newline()
    end do

end program example_progress_bar
