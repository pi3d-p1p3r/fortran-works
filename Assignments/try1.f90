program tryHard
implicit none
INTEGER:: s1,s2,s,i,n

do i = 10,10000
    call divisors(i,s)
    s1=s
    call divisors(s,s2)
    if(i==s2 .and. i/=s) then
    write(*,*)s1,s2
    n=n+1
    end if
    if(n==10) stop
end do

end program

subroutine divisors(inp,sum)
implicit none
integer::inp,sum,j

do j = 2,inp/2
    if(mod(inp,j)==0) sum = sum + inp/j
end do

end subroutine