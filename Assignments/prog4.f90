program false_position
implicit none

integer :: max_iter, iter
real :: a, b, c, tol, fa, fb, fc, EXCT
EXCT = (LOG(7.0)-LOG(3.0))/((3*(LOG(3.0)))-(2*(LOG(5.0))))

a = 11
b = 12
tol = 0.0001
max_iter = 100000

call f(a, fa)
call f(b, fb)

if (fa*fb >= 0.0) then
    print *, "Error: initial guesses do not bracket the root"
    stop
end if

do iter = 1, max_iter

    c = (a*fb - b*fa) / (fb - fa)
    call f(c, fc)

    if (abs(fc) < tol) then
        print *, "Root found: x =", c
        stop
    endif

    if (fc*fb < 0.0) then
        a = b
        fa = fb
    endif

    
    b = c
    fb = fc

end do

print *, "Error: method did not converge within", max_iter, "iterations"

contains

subroutine f(x, fx)
    real, intent(in) :: x
    real, intent(out) :: fx
    fx = ((3**(3*x + 1))) - 7*(5**(2*x))
end subroutine f

end program false_position
