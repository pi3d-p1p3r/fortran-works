program bisection
    implicit none
    real :: a,b,tol= 0.000001,p,f,p0,a2,b2,p2
    integer:: i,max_iter=1000
    write(*,*)"Enter the interval :"
    read(*,*)a,b
    a2=a
    b2=b

    if(f(a)*f(b)<0)then
        write(*,*)"There exists a root in the given interval"

        write(*,*)"Table:1"
        write(*,*)"Iteration No.    a                b                  Pn           abs((pn-pn-1)/pn)"
        write(*,*)"=================================================================================="

        do i=1,max_iter

            p=(a+b)/2

            if((abs(p-p0)/abs(p))<tol)then
                write(*,*)"The root is =",p
                exit
            end if

        write(*,*)i,a,b,p,(abs(p-p0)/abs(p))

        if(f(a)*f(p)>0)then
            a=p
        else
            b=p 
        end if
        p0=p
        
        end do

        write(*,*)"Table:2"
        write(*,*)"Iteration No.    a                b                  Pn           abs. err"
        write(*,*)"=================================================================================="
        
        do i=1,max_iter

            p2=(a2+b2)/2

            if(abs(f(p2))<tol)then
                write(*,*)"The root is =",p2
                stop
            end if

        write(*,*)i,a2,b2,p2,abs(f(p2))

        if(f(a2)*f(p2)>0)then
            a2=p2
        else
            b2=p2 
        end if
        
        end do
    else
        write(*,*)"There exists no root in the interval"
        stop
    end if
end program bisection

real function f(x)
    implicit none
    real,intent(in) :: x
    f=(x**3)-x-1
end function