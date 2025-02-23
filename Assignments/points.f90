program a5q1
    implicit none
    real,dimension(0:4) :: f
    real :: mpt5,pt,h,ept5,ept3,mpt3
    integer :: x
    h=2
    f =(/2408,6570,14652,28574,60640/)

    print*,"enter the point to apprx(mid-point) :"
    read(*,*)pt 
    x=int((pt-2007)/h) !2007 is the initial value..
    
        mpt5= (1/(12.0*h))*(f(x - 2)- 8*f(x - 1) + 8*f(x + 1)-f(x + 2))
        print*,"approximation using 5 point mid-point formula :",mpt5 
        mpt3=(1/(2.0*h))*(f(x+1)-f(x-1))
        print*,"approximation using 3 point mid-point formula :",mpt3

    print*,"enter the point to apprx(end-point)"
    read(*,*)pt
    x=int((pt-2007)/h)

        ept5=(1/(12.0*h))*(-25*f(x)+48*f(x+1)-36*f(x+2)+16*f(x+3)-3*f(x+4))
        print*,"approximation using 5 point end-point formula :",ept5

        ept3=(1/2.0*h)*(-3*f(x)+4*f(x+1)-f(x+2))
        print*,"approximation using 3 point end-point formula :",ept3

end program