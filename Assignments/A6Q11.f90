PROGRAM LUSOLVE
      IMPLICIT NONE

      INTEGER n, i, j, k
      PARAMETER (n = 4)

      REAL A(n,n), L(n,n), U(n,n), B(n), X(n), Y(n)
      
      ! Assign values to the matrix A and vector B
      
      WRITE(*,*) 'Enter the elements of the matrix A: '
  DO i = 1, n
     DO j = 1, n
        READ(*,*) A(i,j)
     END DO
  END DO

  WRITE(*,*) 'Enter the elements of the vector b: '
  DO i = 1, n
     READ(*,*) B(i)
  END DO

      ! Compute LU decomposition of matrix A
      
      DO j = 1, n
         DO i = 1, j
            U(i,j) = A(i,j)
            DO k = 1, i-1
               U(i,j) = U(i,j) - L(i,k)*U(k,j)
            END DO
         END DO
         DO i = j+1, n
            L(i,j) = A(i,j)
            DO k = 1, j-1
               L(i,j) = L(i,j) - L(i,k)*U(k,j)
            END DO
            L(i,j) = L(i,j) / U(j,j)
         END DO
      END DO

      ! Solve the system of equations
      
      DO i = 1, n
         Y(i) = B(i)
         DO j = 1, i-1
            Y(i) = Y(i) - L(i,j)*Y(j)
         END DO
      END DO
      
      DO i = n, 1, -1
         X(i) = Y(i)
         DO j = i+1, n
            X(i) = X(i) - U(i,j)*X(j)
         END DO
         X(i) = X(i) / U(i,i)
      END DO

      ! Print the solution
      
      WRITE (*,*) 'Solution:'
      DO i = 1, n
         WRITE (*,*) X(i)
      END DO

      END
