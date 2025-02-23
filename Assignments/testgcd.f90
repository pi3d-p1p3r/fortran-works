program recur_rel
implicit none
integer::ans,a,b,gcd
read(*,*)a,b
ans = gcd(a,b)
write(*,*)ans
end program

recursive function gcd(a,b) result(ans)
implicit none
integer,INTENT(IN)::a,b
integer::ans
if(mod(a,b)==0) then
    ans = b
else
    ans = gcd(b,mod(a,b))
end if
end function