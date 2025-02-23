program fixed_point
    implicit none
    real :: f,dg,g,tol= 0.001,p0,p1,a,b,a2,b2,t1,t0
    integer :: i,max_iter=1000

    print*,"Enter the interval(a and b) :"
    read(*,*)a,b
    a2=a
    b2=b

    if(f(a)*f(b)>0)then
        print*,"The exists no root in the given interval"
        stop
    else
        print*,"There is a root in the given interval"
    end if

    Print*,"Enter your first initial guess :"
    Read(*,*)p0

    Print*,"Enter your second initial guess :"
    Read(*,*)t0

    if(abs(dg(p0))<1)then
        print*,"There exits a Fixed point"

        print*,"Iteration no.     Pn-1          Pn              f(Pn)             rel. err"
        do i=1,max_iter
            p1=g(p0)
            write(*,*)i,p0,p1,f(p1),abs(p1-p0)/abs(p1)

            if((abs(p1-p0))<tol)then
                print*,"The root is =",p1
                exit
            end if

            p0=p1
        
        end do

        if(abs(dg(t0))<1)then
        print*,"There exits a Fixed point"
        else
        print*,"There doesn't exist a Fixed point at",t0
        stop
        end if
        do i=1,max_iter
            t1=g(t0)
            write(*,*)i,t0,t1,f(t1),abs(t1-t0)/abs(t1)

            if((abs(t1-t0))<tol)then
                print*,"The root is =",t1
                stop
            end if

            t0=t1
        
        end do

        print*,"It doesn't converge within the maximum iteration"
    else
        Print*,"No fixed point"
        Stop
    end if



end program


real function g(x)
    implicit none
    real,intent(in) :: x
    g=2**(-x)-x**3+(0.5*x**2)

end function

real function dg(x)
    implicit none
    real,intent(in) :: x
    dg=-2**(-x)*log(2.0) - 3*x**2 + x
end function

real function f(x)
    implicit none
    real,intent(in) :: x
    f=-2**(-x) + x**3 - (0.5*x**2) + x
end function

