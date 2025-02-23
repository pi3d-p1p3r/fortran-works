program fibonacci
implicit none
integer::a,b,x,sum,n,i,j
a=0
b=1
sum=b
write(*,*)"Please write the number of terms you want to know in the fibonacci sequence:"
read(*,*)n
write(*,*)a

do i = 1,n-2
    x = a + b
    a = b
    b = x

    if (mod(i,2)==1) then
    sum = sum + x
    write(*,*)x
    end if

end do

write(*,*)"The sum of the first n odd terms is ", sum

end program