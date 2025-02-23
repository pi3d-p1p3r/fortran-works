program lagrange_interpolation
    implicit none
    integer :: i, j, n
    real :: x(6), y(6), xi, yi, L

    x = (/ 1.0, 2.0, 3.0, 4.0, 6.0, 10.0 /)
    y = (/ 181.0, 155.0, 161.5, 183.0, 214.1, 319.0 /)

    n = 6
    xi = 7.0
    yi = 0.0
    do i = 1,n
        L = 1.0
        do j=1,n
            if (j /= i) then
                L = L * (xi - x(j)) / (x(i) - x(j))
            end if
        end do
        yi = yi + L * y(i)
    end do

    write(*,*) 'Interpolated value at xi=',xi,' is yi=',yi

end program lagrange_interpolation
