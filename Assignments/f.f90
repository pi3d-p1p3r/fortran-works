program a1q2
implicit none
real::a,b,c,d
complex::x1,x2
open(22,file="in_a1q2.txt")
open(32,file="op_a1q2.txt")
read(22,*)a,b,c
write(32,*)"The equation is ax^2+bx+c=0"
write(32,'(/)')
write(32,*)"The constant terms are: "
write(32,'((F5.2,/))')a,b,c
write(32,'(/)')
write(32,*)"The roots are x1 and x2"
d=(b**2-(4*a*c))
x1=(-b+sqrt(complex(d,0.)))/(2*a)
x2=(-b-sqrt(complex(d,0.)))/(2*a)
if(d>=0)then
	write(32,*)"The roots are real"
	write(32,*)"The roots are: "
	2 format("x1= ",F5.2,/,"x2= ",F5.2 )
	write(32,2)real(x1),real(x2)
	else
	write(32,*)"The roots are complex"
	write(32,*)"The roots are: "
	3 format("x1= ",F5.2,"+"F7.2,"i",/,"x2= ",F5.2,"+"F7.2,"i")
	write(32,3)x1,x2
	end if
end program
