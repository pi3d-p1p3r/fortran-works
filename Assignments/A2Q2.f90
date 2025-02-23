program div
    implicit none
    integer :: s2,s,i1,divisor,n=0

    print*,divisor(220)
        do i1=10,10000
            
            s=1
            s=divisor(i1)
           
            if(s>i1)then    
            s2=1
            s2=divisor(s)
                if (s2==i1) then
                    print*,i1,s
                    n=n+1
                end if
            end if
            
            if(n==10)then
                 stop
            end if
        end do
    
end program div

integer function divisor(i2)
    implicit none
    integer :: j,su=1
    integer,intent(in) :: i2
    
    do j=2,i2/2
        if ( mod(i2,j)==0 ) then
            su=su+i2/j
        end if
    end do
    
    divisor=su
end function divisor