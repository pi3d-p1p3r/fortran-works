program false_position
    implicit none
    integer :: max_iter, iter = 1000
    real :: a, b, c, tol, EXCT,fa,fb,fc
    EXCT = (LOG(7.0)-LOG(3.0))/((3*(LOG(3.0)))-(2*(LOG(5.0))))
    
    a = 11
    b = 12
    fa=(3**(3*a + 1))-(7*((5)**(2*a)))
    fb=(3**(3*b + 1))-(7*((5)**(2*b)))
    tol = 0.0001
    
    write(*,*)"No.         Pn-2      Pn-1         f(Pn-1)          f(Pn-1)             Pn            Abs.err"
    write(*,*)fa,fb
    
    if (f(a)*f(b)>= 0) then
        print *, "Error: initial guesses do not bracket the root"
        stop
    end if
    
    do iter = 1, max_iter

        fa=(3**(3*a + 1))-(7*((5)**(2*a)))
        fb=(3**(3*b + 1))-(7*((5)**(2*b)))
        c = (a*f(b) - b*f(a)) / (f(b) - f(a))
        fc=(3**(3*c + 1))-(7*((5)**(2*c)))
        write(*,"(I5,10x,F10.5,F10.5,10x,E15.5,5x,E15.4,5x,F10.5,5x,F10.5)")iter,a,b,fa,fb,c,abs(c-EXCT)

        if (abs(c-EXCT) < tol) then
            print *, "Root found: x =", c
            stop
        end if
    
        if (fc*fb < 0) then
            a = c
        else
            b = c  
        end if

    end do
    
    print *, "Error: method did not converge within", max_iter, "iterations"
    
    end program false_position