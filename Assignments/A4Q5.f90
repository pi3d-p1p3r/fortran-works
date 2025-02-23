program bool
implicit none
integer::i,j,l1,l2
logical::p,q
print*,"Truth table for (p^q)' is:"
write(*,100)"p","q","(p^q)'"
write(*,*)"-----------------"
do i = 0,1
    do j = 0,1
        p=i
        q=j
        write(*,101)p,q,.not.(p.and.q)
    end do
end do
100 format(A,5X,A,5X,A)
101 format(L,5X,L,7X,L)
print*,"Truth table for (p'.or.q') is:"
write(*,100)"p","q","(p'.or.q')"
write(*,*)"---------------------"
do i = 0,1
    do j = 0,1
        p=i
        q=j
        write(*,101)p,q,.not.(p).or..not.(q)
    end do
end do
do i = 0,1
    do j = 0,1
        p=i
        q=j
        if((.not.(p.and.q)).neqv.(.not.(p).or..not.(q)))then
        write(*,*)"De Morgan's law is invalid for p=",p," and q=",q
        end if
    end do
end do
write(*,*)"De Morgan's law is valid"
end program

