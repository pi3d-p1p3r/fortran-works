program a1q5
    implicit none
    real,dimension(2,3)::a
    real,dimension(3,2)::b
    integer::i,j,t=0

    open(1,file="mata.txt")
    open(2,file="matb.txt")

    do i =1,2
        read(1,*)(a(i,j),j=1,3)
    end do
    do i =1,3
        read(2,*)(b(i,j),j=1,2)
    end do

    do i =1,2
        write(*,*)(a(i,j),j=1,3)
    end do
    do i =1,3
        write(*,*)(b(i,j),j=1,2)
    end do

end program