program bit_strings
    implicit none
    integer::n,i,j,k
    character,allocatable,dimension(:)::bitstring

    write(*,*)"Enter the length of bit strings (n)"
    read(*,*)n
    allocate(bitstring(n))

    do i = 0, 2**n-1
        k=i
        do j = n,1,-1
            if(mod(k,2)==0)then
                bitstring(j)='0'
            else
                bitstring(j)='1'
            end if
            k=k/2
        end do
        if (index(bitstring, '111') == 0) then
            write(*,*)bitstring
        end if
    end do

end program