program recur_rel
    implicit none
    integer :: n,gcd,ans,i,tag 

    print*,"enter the number n : "
    read(*,*)n
    tag=0
    do i=1,n
        ans=0
    ans=gcd(n,i)
    if(ans==1)then 
        tag=tag+1 
    end if 

    end do
    print*,tag
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