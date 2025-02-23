program fibonacci
implicit none
integer :: f0=0, f1=1, series=0, n, i, sum=0,k


do i = 1,n
    do j = 1,n
    
        if(i==j) then
        k = 1
        end if
        if(i/=j) then
        k = 0
        end if
        f = f+k
    end do
end do
if(f==0) then
end if
write(*,*)"Not an LF"

end program fibonacci