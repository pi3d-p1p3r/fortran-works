program arr
    implicit none
    integer,allocatable,dimension(:) :: num
    integer ::i=1,j,temp,n

    write(*,*)"Enter the length of the array: "
    read(*,*)n
    allocate(num(n))
   
    read(*,*)(num(i),i=1,n)

    do i=1,n
        do j=i,n 
            if(num(i)>num(j))then
                temp=num(i)
                num(i)=num(j)
                num(j)=temp
            end if
        end do
    end do

    write(*,*)(num(i),i=1,n)
    
    do i=1,n
        do j=i,n 
            if(num(i)<num(j))then
                temp=num(i)
                num(i)=num(j)
                num(j)=temp
            end if
        end do
    end do

    write(*,*)(num(i),i=1,n)
    
end program arr