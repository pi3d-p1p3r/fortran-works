program recurrence
    implicit none
    integer :: n
    real :: exact, approx

    ! Calculate and compare the solutions for n = 0 to 10
    do n = 0, 10
        if (n == 0) then
            exact = 1.0
            approx = 1.0
        else if (n == 1) then
            exact = 5.0
            approx = 5.0
        else
            exact = (3.0/2.0) * n**2 + (5.0/2.0) * n + 1.0
            approx = 2.0 * approx - exact
        end if
        write(*, '(A, I2, A, F10.2, A, F10.2)') "n = ", n, ", Exact Solution = ", exact, ", Approximate Solution = ", approx
    end do

end program recurrence