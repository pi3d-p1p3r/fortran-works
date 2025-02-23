program bisection
implicit none
integer::i,iter=1000
real::a,b,p,f,tol = 10e-6
a=-1.0
b=2.0

if((f(a)*f(b))<0) then

do i = 1,iter
    p = (a+b)/2
    if(abs(f(p))<tol) then
        write(*,*)"The root found at iteration no. ",i,"is",p
        stop
    end if

    write(*,*)a,b,p,f(p)

    if(f(a)*f(p)>0) then
    a=p
    else
    b=p
    end if


end do
else
write(*,*)"Root not found"
end if


end program

real function f(x)
implicit none
real,intent(in)::x
f = (x**3)/16 - sin(x)
end function