program A1Q5
    real,DIMENSION(2,3)::A
    real,DIMENSION(2,3)::B
    real,DIMENSION(2,3)::ab
    real,DIMENSION(2,3)::ba
    real,DIMENSION(2,3)::at
    real,DIMENSION(2,3)::bt
    INTEGER,DIMENSION(3,2)::lhs
    integer,DIMENSION(2,3)::rhs
    real,DIMENSION(2,2)::lhs_main
    REAL,DIMENSION(2,2)::rhs_main

    INTEGER::t,x,y
    real::e(5,5)
    real,DIMENSION(3,2)::k

    open(15,file = "alq5docu.txt")

    do i = 1,2
        read(15,*)(A(i,j),j=1,3)
    end do

    print*,"A:"
    do i = 1,2
        print*,(A(i,j),j=1,3)
    end do

    do i =1,3
        read(15,*)(B(i,j),j=1,2)
    end do

    print*,"B:"

    do i = 1,3
        print*,(B(i,j),j=1,2)
    end do 
    print*
    ab=MATMUL(A,B)
    write(*,*)"AB= ",ab

    print*
    at=TRANSPOSE(A)
    write(*,*)"Transpose of A is: ",at

    print*
    bt=TRANSPOSE(B)
    write(*,*)"Transpose of A is: ",bt

    print*

    print*,"Final calculation"

    lhs_main=TRANSPOSE(ab)
    rhs_main=MATMUL(bt,at)

    do i = 1,2
        do j = 1,2
        if(lhs_main(i,j)==rhs_main(i,j)) then
        t=t+1;
        end if
        end do
    end do
    if(t==4) then
    write(*,*)"The matrices are equivalent."
    end if
end program