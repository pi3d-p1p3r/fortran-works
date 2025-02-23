program am2
implicit none
integer::amic,s,s1,n,s2,i,k=0
do n = 2,10000
    s=amic(n)
    if(s>n) then
        s1 = amic(s)
        if(s1==n) then
        write(*,*)n,s
        k=k+1
        if(k==10) stop
        end if
    end if
end do


end program

integer function amic(n)
implicit none
integer::i,n,sum,s
sum = 1
s=int(sqrt(real(n)))
do i = 2,s-1
    if((mod(n,i))==0) then
        sum = sum + i + n/i
    end if
end do
amic = sum
end function