program midend
integer::i
real::f(2015),x(5),mid3,mid5,end3,end5,h=2.0
open(1,file="midendinp.txt")
read(1,*)(x(i),i=1,5)
do i = 2007,2015,2
read(1,*)f(i)
end do
do i = 1,5
    write(*,*)x(i)
end do
do i = 2007,2015,2
    write(*,*)f(i)
end do
mid3 = (f(INT(x(3)+h))-f(INT(x(3)-h)))/(2*h)
print*,"3 point mid point formula",mid3
end program