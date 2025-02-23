program divisor 
    implicit none 
    integer :: n 
    read(*,*)n 

    call div(n)
end program 

subroutine div(num)
implicit none 

integer :: num,j
    do j=2,num-1
        do while(mod(num,j)==0) 
            print*,j
            num=num/j
        end do
    end do
end subroutine