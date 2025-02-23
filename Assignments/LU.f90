program lul
implicit none
integer::i,j,k,n
real,allocatable,dimension(:,:)::A,U,L
real,allocatable,dimension(:)::B,X,Y
n = 4
allocate(A(n,n),L(n,n),U(n,n))
allocate(B(n),X(n),Y(n))
open(1,file="LUinp.txt")
do i = 1,n
    read(1,*)(A(i,j),j=1,n)
end do
do i = 1,n
read(1,*)B(i)
end do
write(*,*)"A matrix is"
do i = 1,n
write(*,"(4f6.1)")A(i,:)
end do
write(*,*)"B matrix is"
write(*,"(f6.1)")B

do k = 1,n-1
    do i = k+1,n
    L(i,k) = A(i,k)/A(k,k)
        do j = k+1,n
            A(i,j) = A(i,j) - L(i,k)*A(k,j)
        end do
    end do
end do

do i = 1,n
    L(i,i) = 1.0
    do j = 1,n
        if(i>j) then
            U(i,j) = 0.0
        else
            U(i,j)=A(i,j)
        end if
    end do
end do

Y(1)=B(1)/L(1,1)
do i = 2,n
    Y(i) = B(i)
    do j = 1,i-1
        Y(i) = Y(i) - L(i,j)*Y(j)
    end do
    Y(i) = Y(i)/L(i,i)
end do

X(n) = Y(n)/U(n,n)
do i = n-1,1,-1
    X(i) = Y(i)
    do j = i+1,n
    X(i) = X(i) - U(i,j)*X(j)
    end do
    X(i) = X(i)/U(i,i)
end do

write(*,*)"L matrix is"
do i = 1,n
write(*,"(4f6.1)")L(i,:)
end do
write(*,*)"U matrix is"
do i = 1,n
write(*,"(4f6.1)")U(i,:)
end do

write(*,*)X

end program