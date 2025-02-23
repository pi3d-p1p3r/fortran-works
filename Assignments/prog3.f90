program newton_raphson
implicit none
integer :: max_iter, iter
real :: x0, x, tol, fx, dfx,dfx0

x0 = 1.0
tol = 0.00001
max_iter = 100

    write(*,*)"Iteration no.      Pn-1          f'(Pn-1)             Pn            f(Pn)"

do iter = 1, max_iter

    call f(x0, fx)
    call df(x0, dfx)

    if (abs(fx) < tol) then
        print *, "Root found: x =", x0
        stop
    end if

    x = x0 - fx / dfx

    if (abs(x - x0) < tol) then
        print *, "Root found: x =", x
        stop
    end if

    write(*,*)iter,x0,dfx0,x,fx

    x0 = x
    dfx0 = dfx

end do

print *, "Error: method did not converge within", max_iter, "iterations"

contains

subroutine f(x, fx)
    real:: x
    real :: fx
    fx = 16*x**4 + 88*x**3 + 159*x**2 + 76*x - 240
end subroutine f

subroutine df(x, dfx)
    real:: x
    real :: dfx
    dfx = 64*x**3 + 264*x**2 + 318*x + 76
end subroutine df

end program newton_raphson
