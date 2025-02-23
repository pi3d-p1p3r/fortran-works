program lagrange_interpolation
    implicit none
    integer :: i, j, n
    real :: x(4), y(4), xi, yi, L

    x = (/ 150.0, 152.0, 154.0, 156.0 /)
    y = (/ 12.247, 12.329, 12.410, 12.490 /)

    n = 4
    xi = 155.0
    yi = 0.0

    do i=1,n
        L = 1.0
        do j=1,n
            if (j /= i) then
                L = L * (xi - x(j)) / (x(i) - x(j))
            end if
        end do
        yi = yi + L * y(i)
    end do

    write(*,*) 'Interpolated value at x=',xi,' is yi=',yi

end program lagrange_interpolation
