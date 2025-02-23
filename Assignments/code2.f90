program test
implicit none
integer,dimension(0:10)::arr
integer::i

arr(0)=0
arr(1)=1

do i = 2,10
if(mod(i,2)==0) then
arr(i) = arr(i/2)
else
arr(i)=arr((i+1)/2)+arr((i-1)/2)
end if
end do
do i = 0,10
write(*,*)arr(i)
end do
end program