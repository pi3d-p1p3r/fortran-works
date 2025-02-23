program newton_divided_difference

    implicit none

    integer :: n, i, j,k
    real :: x(6), y(6), f(100,100),d(100),t,x0

    n = 6
    x = (/ 1.0, 2.0, 3.0, 4.0, 6.0, 10.0 /)
    y = (/ 181.0, 155.0, 161.5, 183.0, 214.1, 319.0 /)

    do i = 1, n
        f(i, 1) = y(i)
    end do

    do j = 2, n
        do i = 1, n-j+1
            f(i, j) = (f(i+1, j-1) - f(i, j-1)) / (x(i+j-1) - x(i))
        end do
    end do

    x0=7
    t = f(i,j)
    print *, "Difference table:"
    do i = 1, n
        write(*, '(6F10.4)') (f(i,j), j=1,n-i+1)
        if(i<n) t = f(1,n-i) + (x0-x(n-i))*t
    end do

print*, "The polynomial evaluated at x = ", x0, " is ", t

    do i = 1, n
    d(i) = y(i)
end do

do j = 1, n-1
    do i = n, j+1, -1
        d(i) = (d(i)-d(i-1))/(x(i)-x(i-j))
    end do
end do

x0=7
t = d(n)
do k = 1, n-1
    t = d(n-k) + (x0-x(n-k))*t
end do

print*, "The polynomial evaluated at x = ", x0, " is ", t
end program newton_divided_difference
