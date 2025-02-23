program newton_divided_difference
implicit none
integer :: n, i, j, k
real :: x(6), y(6), x0, t, d(100)
n=6
x = (/ 1.0, 2.0, 3.0, 4.0, 6.0, 10.0 /)
y = (/ 181.0, 155.0, 161.5, 183.0, 214.1, 319.0 /)

do i = 1, n
    d(i) = y(i)
end do

    write(*,*)"        x              f(x)             del"

do j = 1, n-1
    do i = n, j+1, -1
        d(i) = (d(i)-d(i-1))/(x(i)-x(i-j))
        do k = 1,n-1,-1
        
        write(*,*)x(i),y(i),d(k),d(k+6),d(k+11),d(k+15),d(k+18)
        end do
    end do
end do

x0=7
t = d(n)
do k = 1, n-1
    t = d(n-k) + (x0-x(n-k))*t
end do


print*, "The polynomial evaluated at x = ", x0, " is ", t

end program newton_divided_difference
