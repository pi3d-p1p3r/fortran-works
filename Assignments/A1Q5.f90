program a1q5
    implicit none
    real,dimension(2,3)::a,bt,c
    real,dimension(3,2)::b,at,lhs1,rhs1
    real,dimension(2,2)::ab,lhs,rhs
    integer::i,j,t=0

    open(1,file="mata.txt")
    open(2,file="matb.txt")

    do i =1,2
        read(1,*)(a(i,j),j=1,3)
    end do
    do i =1,3
        read(2,*)(b(i,j),j=1,2)
    end do

    bt=transpose(b)

    at=transpose(a)

    do i = 1,2
        do j = 1,3
            c(i,j)=a(i,j)+bt(i,j)
        end do
    end do

    lhs1=transpose(c)

    do i = 1,3
        do j = 1,2
            rhs1(i,j)=at(i,j)+b(i,j)
        end do
    end do

    print*,"(at+b)"

    do i = 1,3
    write(*,'(2f5.1)')(rhs1(i,j),j=1,2)
    end do

    print*,"(a+bt)^t"

    do i = 1,3
    write(*,'(2f5.1)')(lhs1(i,j),j=1,2)
    end do

    ab = matmul(a,b)

    lhs = transpose(ab)

    print*,"lhs"

    do i = 1,2
    write(*,'(2f6.2)')(lhs(i,j),j=1,2)
    end do

    rhs = matmul(bt,at)

    print*,"rhs"

    do i = 1,2
    write(*,'(2f6.2)')(rhs(i,j),j=1,2)
    end do

    do i = 1,2
        do j = 1,2
            if(lhs(i,j)==rhs(i,j)) then
            t=t+1
            end if
        end do
    end do

    if(t==4) then
    write(*,*)"LHS = RHS(proved)"
    end if

end program