program example_progress_bar

    use fffc_module, only: terminal_obj
    implicit none

    call terminal_obj%progress_bar('Progress bar', 0.0)
    print *, "..."
    call terminal_obj%progress_bar('Progress bar', -0.5)
    print *, "..."
    call terminal_obj%progress_bar('Progress bar', 0.5)
    print *, "..."
    call terminal_obj%progress_bar('Progress bar', 1.5)
    print *, "..."

end program example_progress_bar
