program bits
implicit none
integer::n,i,k,j
character,allocatable,dimension(:)::bitstring 

n = 3

ALLOCATE(bitstring(n))

do i = 0, 2**n-1
    k=i
    do j = n,1,-1
        if(mod(k,2)==0) then
        bitstring(j) = '0'
        else
        bitstring(j) = '1'
        end if
        k=k/2
    end do
    write(*,*)bitstring
end do
end program