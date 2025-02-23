program RowReduce
    implicit none
    
    integer, parameter :: n = 3 ! Size of the matrix
    real :: A(n,n) ! Input matrix
    integer :: i, j, k, pivot_row
    real :: pivot, factor
    
    ! Read the matrix elements
    print *, "Enter the elements of the matrix:"
    do i = 1, n
        do j = 1, n
            read *, A(i,j)
        end do
    end do
    
    ! Perform row reduction
    do k = 1, n
        pivot_row = k
        pivot = A(k,k)
        
        ! Find the pivot row
        do i = k+1, n
            if (abs(A(i,k)) > abs(pivot)) then
                pivot_row = i
                pivot = A(i,k)
            end if
        end do
        
        ! Swap rows if necessary
        if (pivot_row /= k) then
            do j = 1, n
                A(k,j) = A(k,j) + A(pivot_row,j)
                A(pivot_row,j) = A(k,j) - A(pivot_row,j)
                A(k,j) = A(k,j) - A(pivot_row,j)
            end do
        end if
        
        ! Scale the pivot row
        do j = k+1, n
            A(k,j) = A(k,j) / pivot
        end do
        
        A(k,k) = 1.0
        
        ! Eliminate other rows
        do i = 1, n
            if (i /= k) then
                factor = A(i,k)
                do j = k, n
                    A(i,j) = A(i,j) - factor * A(k,j)
                end do
                A(i,k) = 0.0
            end if
        end do
    end do
    
    ! Print the row reduced echelon form
    print *, "Row reduced echelon form:"
    do i = 1, n
        do j = 1, n
            print *, A(i,j)
        end do
    end do
    
end program RowReduce