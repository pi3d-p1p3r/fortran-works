program try1
implicit none
integer,ALLOCATABLE,DIMENSION(:)::arr
integer n,i
read(*,*)n
ALLOCATE(arr(n+1))
arr(0)=0
arr(1)=1
do i = 2,n
    if(mod(i,2)==0) then
        arr(i) = arr(i/2)
    end if
    if(mod(i,2)==1) then
        arr(i) = arr((i+1)/2) + arr((i-1)/2)
    end if
end do
do i = 0,n
    write(*,*)i,arr(i)
end do
end program