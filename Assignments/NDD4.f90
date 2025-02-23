!Gamemaker: Suave
!Hunter: Tanvir

program NDD_table
    implicit none
    integer :: n, i, j,k
    real :: x(6), y(6), f(100,100),t,p,M

    n = 6
    x = (/ 1.0, 2.0, 3.0, 4.0, 6.0, 10.0 /)
    y = (/ 121.0, 155.0, 161.5, 183.0, 264.1, 319.0 /)
    p = 7
    t = y(1)

    do i = 1, n
        f(i, 1) = y(i)
    end do

    do j = 2, n
        do i = 1, n-j+1
            f(i, j) = (f(i+1, j-1) - f(i, j-1)) / (x(i+j-1) - x(i))
        end do
    end do

    print *, "Difference table:"
    do i = 1, n
        write(*, '(6F10.4)') (f(i,j), j=1,n-i+1)
    end do

    do j = 2,n
        M=1
        do k = 2,n
            M = M*(p-x(k-1))
            if(k==j) exit
        end do
        t = t + (f(1,j))*M
    end do

print*, "The polynomial evaluated at x = ", p, " is ", t
end program NDD_table
