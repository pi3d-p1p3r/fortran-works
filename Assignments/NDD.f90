program newton_divided_difference
implicit none

integer :: n, i, j, k
real :: x(100), y(100), p, t, d(100)
n=6
x = (/ 1.0, 2.0, 3.0, 4.0, 6.0, 10.0 /)
y = (/ 181.0, 155.0, 161.5, 183.0, 214.1, 319.0 /)
!print*, "Enter the number of data points:"
!read*, n

!print*, "Enter the x values:"
!read*, (x(i),i=1,n)

!print*, "Enter the y values:"
!read*, (y(i),i=1,n)

do i = 1, n
    d(i) = y(i)
end do

do j = 1, n-1
    do i = n, j+1, -1
        d(i) = (d(i)-d(i-1))/(x(i)-x(i-j))
    end do
end do

!print*, "Enter the point at which to evaluate the polynomial:"
!read*, p
p=7
t = d(n)
do k = 1, n-1
    t = d(n-k) + (p-x(n-k))*t
end do

print*, "The polynomial evaluated at x = ", p, " is ", t

end program newton_divided_difference
