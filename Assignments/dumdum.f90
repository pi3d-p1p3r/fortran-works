program numerical_integration
implicit none
integer::i,j,k,n
real::a,b,exact,h,f,g,it1,it2,it3,it4,sum=0
a=0.0
b=1.2
n=30
h=(b-a)/n
it1 = 0.0; it2 = 0.0; it3 = 0.0; it4 = 0.0
do i = 1,n-1

    it1 = it1 + f(a+(i*h))

    if(mod(i,2)==0)then
        it2 = it2 + 2*f(a+(i*h))
    else
        it2 = it2 + 4*f(a+(i*h))
    end if

    if(mod(i,3)==0)then
        it3 = it3 + 2*(f(a+(i*h)))
    else
        it3 = it3 + 3*(f(a+(i*h)))
    end if

    if(mod(i,2)==0 .and. mod(i,6)==0 .and. mod(i,3)==0)then
        it4 = it4 + 2*f(a+(i*h))
    else if(mod(i,2)==0)then
        it4 = it4 + f(a+(i*h))
    else if(mod(i,3)==0)then
        it4 = it4 + 6*f(a+(i*h))
    else
        it4 = it4 + 5*f(a+(i*h))
    end if

end do
write(*,*)"Trapezoidal rule: "
it1 = (f(a)+f(b)+2*it1)*(h/2)
write(*,*)it1

write(*,*)"Simpson's 1/3rd rule: "
it2 = (f(a)+f(b)+it2)*(h/3)
write(*,*)it2

write(*,*)"Simpson's 3/8 rule: "
it3 = (f(a)+f(b)+it3)*((3*h)/8)
write(*,*)it3

write(*,*)"Weddle's rule: "
it4 = (f(a)+f(b)+it4)*((3*h)/10)
write(*,*)it4



write(*,*)
write(*,2)"exact soln","**** rule", "abs error", "rel error"
write(*,*)"=============================================================================="
2 format(a15,5x,a15,5x,a15,5x,a15)

write(*,3)(g(b)-g(a)),"Trapezoidal",abs(g(b)-g(a)-it1),(abs(g(b)-g(a)-it1))/abs(g(b)-g(a))
3 format(f15.8,5x,a15,5x,f15.8,5x,f15.8)

write(*,3)(g(b)-g(a)),"Simpson's 1/3rd",abs(g(b)-g(a)-it2),(abs(g(b)-g(a)-it2))/abs(g(b)-g(a))
write(*,3)(g(b)-g(a)),"Simpson's 3/8 rule",abs(g(b)-g(a)-it3),(abs(g(b)-g(a)-it3))/abs(g(b)-g(a))
write(*,3)(g(b)-g(a)),"Weddle",abs(g(b)-g(a)-it4),(abs(g(b)-g(a)-it4))/abs(g(b)-g(a))

do i=0,n-1,6
        sum= sum +f(a+i*h)+ 5*f(a+(i+1)*h) + f(a+(i+2)*h) + 6*f(a+(i+3)*h) + f(a+(i+4)*h) + 5*f(a+(i+5)*h) + f(a+(i + 6)*h)
        write(*,*)"sum = ",(3*h/10)*sum
    end do
    print*,(3*h/10)*sum

end program

real function f(x)
real::x
f = (tan(x))**3
end function

real function g(x)
real::x
g = log(abs(cos(x)))+0.5*(tan(x)**2)
end function

