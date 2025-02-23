program perm
IMPLICIT NONE
integer(kind=8)::npr,ncr,n,r,fact
n=6
r=2
npr = fact(n)/fact(n-r)
ncr = fact(n)/(fact(r)*fact(n-r))
write(*,*)ncr,npr

end program

recursive function fact(num) result(res)
integer(kind=8)::num,res
if(num==0) then
    res = 1
else
    res = num*fact(num-1)
end if
end function