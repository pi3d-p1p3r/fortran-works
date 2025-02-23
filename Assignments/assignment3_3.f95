program rafsan
implicit none
    real :: x0,x1,f,df,tol=10E-5
    integer :: i
    write(*,*)"Enter the initial guess :"
    read(*,*)x0
    write(*,*)"        NO.      Pn-1         f'(Pn-1)         Pn                f(Pn)"
    do i=1,50
        x1=x0-(f(x0)/df(x0))
       
        write(*,*)i,x0,df(x0),x1,f(x1)

        if(abs(x1-x0)<tol)then
            print*,"The root is =",x1
            stop
        end if

        x0=x1

    end do

end program

real function f(x)
implicit none
    real,intent(in) :: x 

    f=16*x**4 + 88*x**3 + 159*x**2 + 76*x-240

end function

real function df(x)
implicit none
real,intent(in) :: x 

    df=64*x**3+264*x**2+318*x+76

end function