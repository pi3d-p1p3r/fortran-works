PROGRAM gaussian_elimination

  INTEGER :: n, i, j, k
  REAL :: A(100, 100), b(100), x(100), f

  WRITE(*,*) 'Enter the size of the matrix (n): '
  READ(*,*) n

  WRITE(*,*) 'Enter the elements of the matrix A: '
  DO i = 1, n
     DO j = 1, n
        READ(*,*) A(i,j)
     END DO
  END DO

  WRITE(*,*) 'Enter the elements of the vector b: '
  DO i = 1, n
     READ(*,*) b(i)
  END DO

  DO k = 1, n-1
     DO i = k+1, n
        f = A(i,k)/A(k,k)
        DO j = k+1, n
           A(i,j) = A(i,j) - f*A(k,j)
        END DO
        b(i) = b(i) - f*b(k)
     END DO
  END DO

  DO k = n, 1, -1
     x(k) = b(k)/A(k,k)
     DO i = 1, k-1
        b(i) = b(i) - A(i,k)*x(k)
     END DO
  END DO

  WRITE(*,*) 'The solution is: '
  DO i = 1, n
     WRITE(*,*) x(i)
  END DO

END PROGRAM gaussian_elimination
