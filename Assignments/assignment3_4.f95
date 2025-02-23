program false_position
    implicit none
    
    integer :: max_iter, iter
    real(kind=16) :: a, b, c, tol, EXCT,f
    EXCT = (LOG(7.0)-LOG(3.0))/((3*(LOG(3.0)))-(2*(LOG(5.0))))
    
    a = 11
    b = 12
    tol = 0.0001
    max_iter = 1000
    
    write(*,*)"Iteration No.    Pn-2       Pn-1                f(Pn-1)               f(Pn-1)           Pn             Abs.err"
    write(*,*)"==============================================================================================================="
    
    if (f(a)*f(b)>= 0) then
        print *, "Error: initial guesses do not bracket the root"
        stop
    end if
    
    do iter = 1, max_iter
    
        c = (a*f(b) - b*f(a)) / (f(b) - f(a))
        
        write(*,"(I5,10x,F10.5,F10.5,10x,E15.5,5x,E15.4,5x,F10.5,5x,F10.5)")iter,a,b,f(a),f(b),c,abs(c-EXCT)

        if (abs(c-EXCT) < tol) then
            print *, "Root found: x =", c
            stop
        endif
    
        if (f(c)*f(b) < 0) then
            a = c
        else
            b = c
        end if
        
    end do
    
    print *, "Error: method did not converge within", max_iter, "iterations"
    
    end program false_position

    real(kind=16) function f(x)
    implicit none
        real(kind=16),intent(in) :: x 
    
        f=(3**(3*x + 1))-(7*((5)**(2*x)))
    
    end function