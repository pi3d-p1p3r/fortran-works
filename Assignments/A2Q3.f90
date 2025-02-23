program stern
    implicit none
    integer,allocatable,dimension(:) :: arr
    integer :: n,i 

    write(*,*)"Enter the number of terms in the array :"
    read(*,*)n

    allocate(arr(n+1))

    arr(0)=0
    arr(1)=1

    do i=2,n
        if(mod(i,2)==0)then
            arr(i)=arr(i/2)
        else
            arr(i)=arr((i+1)/2)+arr((i-1)/2)
        end if
    end do

    do i = 0, n
        write(*,*)i,arr(i)
    end do
end program stern