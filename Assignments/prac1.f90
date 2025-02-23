program NDD_table
    implicit none
    integer :: n, i, j,k
    real :: x(6), y(6),t,p,M
    real,DIMENSION(6,7)::f

    n = 6
    x = (/ 1.0, 2.0, 3.0, 4.0, 6.0, 10.0 /)
    y = (/ 181.0, 155.0, 161.5, 183.0, 214.1, 319.0 /)
    p = 7
    t = y(1)

do i = 1,n
    f(i,1)=x(i)
    f(i,2)=y(i)
end do

do j = 3,7
    do i = 1,5
        f(i,j) = (f(i,j-1)-f(i+1,j-1))/(x(i)-x(i+j-2))
    end do
end do

do i = 1,6
    write(*,"(f6.1,6(4x,f6.1))")(f(i,j),j=1,8-i)
end do

do i = 1,5
    M = 1.0
    do j = 1,i
        M = M*(p-x(j))
    end do 
    t = t + M*f(1,i+2)
end do
write(*,*)"The value of the polynomial at the point is ",t
end program