!Gamemaker: Irteza Asif '06
!Hunter: Tanvir Kabir '07

program newt_fwd_diff
    implicit none
    integer :: n, i, j, k
    real :: x(6), y(6), f(100,100),v,p,M,h,r

    n = 6
    x = (/ 0.0, 10.0, 20.0, 30.0, 40.0, 50.0 /)
    y = (/ 0.0, 227.04, 360.0, 518.0, 602.97, 900.0 /)
    p = 5.0
    v = y(1)
    r = (p-x(1))/(x(2)-x(1))

    do i = 1, n
        f(i, 1) = y(i)
    end do

    do j = 2, n
        do i = 1, n-(j-1)
            f(i, j) = f(i+1, j-1) - f(i, j-1)
        end do
    end do

    print *, "Difference table:"
    write(*,*)"  x       f(x)      del      del2      del3      del4      del5"
    write(*,*)"=================================================================="
    do i = 1, n
        write(*, '(F5.2,6F10.4)')x(i),(f(i,j), j=1,n-i+1)
    end do

    do j = 2,n
        M = 1
        do k = 2,n
            M = M*((r-(k-2))/(k-1))
            if(k==j) exit
        end do
        v = v + M*(f(1,j))
    end do

write(*,*) "The polynomial evaluated at x = ", p, " is ", v
write(*,*)size(x)
end program 
