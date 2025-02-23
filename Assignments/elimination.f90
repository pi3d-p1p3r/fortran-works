Program a6q1a

    Implicit None
    Real,Dimension(:,:), Allocatable :: a
    Real,dimension(:), Allocatable :: x
    Real :: k1,k2,c
    Integer :: i,j,k,n

    Open(56,file="a6q1a.txt")

    Write(,)"Please enter the no. of rows"
    Read(,)n
    Allocate(a(n,n+1),x(n))

    Write(,)""
    Write(,)"Enter the elements in file"
    Write(,)""

    Do i=1,n
    Read(56,*)(a(i,j),j=1,n+1)
    End Do

    Write(,)"The augmented matrix without pivoting is :"
    Do i=1,n
        Write(*,15)(a(i,j),j=1,n+1)
    End Do

    Do k=1,n-1
        k1=a(k,k)
        Do i=k+1,n
            k2=a(i,k)/k1
            Do j=k,n+1
                a(i,j)=a(i,j)-k2*a(k,j)
            End Do
        End Do
    End Do

    Write(,)"The augmented matrix after pivoting is :"
    Do i=1,n
        Write(*,15)(a(i,j),j=1,n+1)
    End Do

    15 FORMAT(5(5X,F10.2))
    x(n)=a(n,n+1)/a(n,n)
    Do i=n-1,1,-1
        c=0
        Do j=i+1,n
            c=c+a(i,j)*x(j)
        End Do
        x(i)=(a(i,n+1)-c)/a(i,i)
    End Do

    Write(,)"======================================================================"
    Write(,)"The solutions are :"
    Write(,)"x=",x(1)
    Write(,)"y=",x(2)
    Write(,)"z=",x(3)
    Write(,)"t=",x(4)

End Program