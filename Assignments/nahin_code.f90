program amicable
implicit none
integer::s,s1,sf,n,k
k=0
do n = 2,100000
    s=sf(n)
    if(s>n)then
        s1=sf(s)
        if(s1==n)then
        write(*,*)n,s
        k=k+1
            if(k==10)then
            stop
            end if
        end if
    end if
end do
end program

integer function sf(n)
implicit none
integer,intent(in)::n
integer::sum,s,i
sum=1
s=int(sqrt(real(n)))
do i = 2,s-1
    if(mod(n,i)==0)then
    sum = sum + i + n/i
    end if
end do
sf = sum
end function