!Hunter: Wasiful Haque

program laggy
implicit none
    integer :: i, j, n
    real :: x(4), y(4), xi, yi, L

    x = (/ 5.0, 6.0, 9.0, 11.0/)
    y = (/ 12.0, 13.0, 14.0, 16.0/)

    n = 4
    xi = 10.0
    yi = 0.0

    do i = 1,n
        L = 1.0
        do j = 1,n
            if(i/=j) then
            L = L*((xi-x(j))/(x(i)-x(j)))
            end if
        end do
        yi = yi + L*y(i)
    end do
    print*,"The value of y(10) is",yi

end program