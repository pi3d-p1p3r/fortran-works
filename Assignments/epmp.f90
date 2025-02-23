program a5q1
implicit none
integer::i
real::end3,end5,mid3,mid5,h,x(5)
real,dimension(2007:2015)::f
open(1,file='in.txt')
x=(/2007,2009,2011,2013,2015/)
h=2.0
do i = 2007,2015,2
    read(1,*)f(i)
end do
end3 = (-3*f(x(1))+4*f(x(1)+h)-f(x(1)+2*h))/(2*h)
write(*,*)"The 3 point end point value is: ", end3
end5 = (-25*f(x(1))+48*f(x(1)+h)-36*f(x(1)+2*h)+16*f(x(1)+3*h)-3*f(x(1)+4*h))/(12*h)
write(*,*)"The 5 point end point value is: ", end5
mid3=(f(x(3)+h)-f(x(3)-h))/(2*h)
write(*,*)"The 3 point mid point value is: ", mid3
mid5=(f(x(3)-2*h)-8*f(x(3)-h)+8*f(x(3)+h)-f(x(3)+2*h))/(12*h)
write(*,*)"The 5 point mid point value is: ",mid5
end program