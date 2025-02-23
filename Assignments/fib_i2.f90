program fibonacci

implicit none
integer :: f0=0, f1=1, series=0, n, i, sum=0 

write(*,*)"Enter the number of terms in the sequence: "

read (* , *)n

write(*,*)f0

do i=1, n*2-1
if (mod (i,2)==0) then
sum=sum+series
write(*, *)series
end if
series=f0+f1
f0=f1
f1=series
end do
write(*,*)"The sum of the first n odd number of fibonacci series are :", sum 
end program fibonacci