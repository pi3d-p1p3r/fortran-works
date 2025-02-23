program sort
integer::i,j
real::temp
real,dimension(6)::a
a = (/4,2,-1,7,-6,0/)
do i=1,6
    do j = i,6
        if(a(i)>a(j)) then
            temp = a(i)
            a(i) = a(j)
            a(j) = temp
        end if
    end do
end do
write(*,*)a
end program