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
    t=1

    lhs_main(1,1) = 1
    lhs_main(1,2) = 2
    lhs_main(2,1) = 3
    lhs_main(2,2) = 4

    rhs_main(1,1) = 1
    rhs_main(1,2) = 2
    rhs_main(2,1) = 3
    rhs_main(2,2) = 4
    write(*,*)lhs_main
    write(*,*)rhs_main

    do i = 1,2
        do j = 1,2
        if(lhsii(i,j)==rhsii(i,j)) then
        write(*,*)t
        t=t+1;
        end if
        end do
    end do
    if(t==4) then
    write(*,*)"The matrices are equivalent."
    end if
end program