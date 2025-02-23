program recur_rel
    implicit none
    integer :: a,b,gcd,ans
    print*,"enter the number a and b : "
    read(*,*)a,b 
    ans=gcd(a,b)
    print*,ans
end program recur_rel

recursive function gcd(a,b) result(ans)
implicit none 
integer,intent(in) :: a,b 
integer :: ans
if(mod(a,b)==0)then 
    ans=b 
else 
    ans=gcd(b,mod(a,b))
end if
end function