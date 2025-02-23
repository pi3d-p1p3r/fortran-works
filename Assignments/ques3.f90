program fibonacci
implicit none
integer::a,b,x,sum,n,i,j
i=1
j=1
a=0
b=1
sum=b
write(*,*)"Please write the number of terms you want to know in the fibonacci sequence:"
read(*,*)n

do while(4<5)
    x = a + b
    a = b
    b = x
    
    if (mod(x,2)==1) then
    sum = sum + x
    write(*,*)x
    j=j+1
    end if
    
    if (j==n) exit
    i=i+1
end do

write(*,*)"The sum of the first n odd terms is ", sum

end program