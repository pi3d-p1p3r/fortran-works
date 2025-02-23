program bit_strings
implicit none
integer::n,i,j,k,t,s
character,allocatable,dimension(:)::bitstring
s=0

write(*,*)"Enter the length of bit strings (n)"
read(*,*)n
allocate(bitstring(n))

write(*,*)"Ascending order"
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
    write(*,*)bitstring
end do

write(*,*)"Descending order"
do i = 2**n-1,0,-1
    k=i
    do j = n,1,-1
        if(mod(k,2)==0)then
            bitstring(j)='0'
        else
            bitstring(j)='1'
        end if
        k=k/2
    end do
    write(*,*)bitstring
end do

write(*,*)"Removing 111"
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
    
    do t = 1,n
    if(index(bitstring(t), "1")==0) then
    s = s + 1
    else
    s = 0
    end if
    end do
    write(*,*)s
    if(s==3) then
    
    end if
end do

end program