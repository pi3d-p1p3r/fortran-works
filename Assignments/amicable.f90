program bleh
implicit none
integer:: s1,s2,s,i,n=0

    do i = 10,100000
        s1=1
        s2=1
        s=1
        call divisors(i,s)
        s1=s
        call divisors(s,s2)
        if(i==s2 .and. i/=s) then
           print*,i,s1
           n=n+1
        end if
        if(n==10) then
        stop 
        end if
    end do
end program bleh

subroutine divisors(inp,sum)
implicit none
    INTEGER:: sum, inp,j
    do j = 2,inp/2
        if (mod(i2,j)==0) then
            sum = sum + inp/j
        end if
    end do
end subroutine