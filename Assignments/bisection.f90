program bisection
    implicit none
    real :: a,b,tol= 0.001,p,f,p0
    integer:: i,max_iter=1000
    write(*,*)"Enter the interval :"
    read(*,*)a,b

    if(f(a)*f(b)<0)then
        write(*,*)"There exists a root in the given interval"

        write(*,*)"Table:1"
        write(*,*)"Iteration No.    a                b                  Pn            f(Pn)               Error"
        write(*,*)"======================================================================================================"

        do i=1,max_iter

            p=(a+b)/2

            write(*,*)i,a,b,p,f(p),abs(p-p0)/abs(p)

            if(abs(f(p))<tol)then
                write(*,*)"The root is =",p
                exit
            end if

        if(f(a)*f(p)>0)then
            a=p
        else
            b=p 
        end if
        p0=p
        end do

    else
        write(*,*)"There exists no root in the interval"
        stop
    end if
end program bisection

real function f(x)
    implicit none
    real,intent(in) :: x
    f=x**3 - x - 1
end function