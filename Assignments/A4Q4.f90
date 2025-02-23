program perm
implicit none
real::n,P,fact,m,nCr,nPr,q

print*,"Enter the value of n,m"
read(*,*)n,m

write(*,*)"The combination of nCr is:"

nCr = fact(n)/(fact(m)*fact(n-m))
write(*,*)int(nCr)

print*,"Enter the value of p,q"
read(*,*)p,q

write(*,*)"The permutation of nPr is:"
nPr = fact(p)/(fact(p-q))
write(*,*)int(nPr)
end program

recursive function fact(s) result(ans)
real::s,ans

if(s==0) then
    ans = 1
else
    ans = s*fact(s-1)
end if

end function