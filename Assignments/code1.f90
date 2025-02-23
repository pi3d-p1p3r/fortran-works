real function f(x)
f = exp(-x**2)
end function

program rom
implicit none
integer,parameter::n=4
integer::i,j,k,l,m
real::a,b,h,f,r(n,n),sum1
a=0
b=1

h= b-a
r(1,1)=0.5*h(f(a)+f(b))
write(*,*)r(1,1)
do i = 2,n
sum1 = 0
do k = 1,2**(i-2)
sum1 = sum1 + f(a+(k-0.5)*h)
end do
r(2,1) = 0.5*(r(1,1)+h*sum1)
do j = 2,i
r(2,j) = r(2,j-1)+(r(2,j-1)-r(1,j-1))/(4**(j-1)-1)
write(*,*)((r(m,l),m=2,2),l=1,i)
end do
h=h/2
do j = 1,i
r(1,j) = r(2,j)
end do
end do
end program

