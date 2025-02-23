program hanoi
implicit none
integer,parameter::iLONG = selected_int_kind(30)
integer,parameter::fLONG = selected_int_kind(30)
integer(kind=iLONG)::ans,i,H
integer(kind=iLONG),DIMENSION(8)::movesNo,n
real(kind=fLONG),dimension(8)::day

write(*,'(A2,20x,A,20x,A)') "n","Hn","Days"

do i = 1,8
    n(i)=i**2
    movesNo(i) = H(n(i))
    day(i) = real(movesNo(i))*(5.0/86400.0)
    write(*,"(I2,5X,I25,5X,I25)") n(i),movesNo(i),ceiling(day(i),kind=iLONG)
end do

end program

recursive function H(n) result(ans)
implicit none
integer,parameter::LONG = selected_int_kind(30)
integer(kind=LONG)::n,ans

if(n==1)then
    ans = 1
else if(n>1)then
    ans = 2*H(n-1)+1
end if
end function