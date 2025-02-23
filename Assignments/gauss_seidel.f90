PROGRAM GaussSeidel
  IMPLICIT NONE
  
  INTEGER, PARAMETER :: N = 3 
  REAL*8, PARAMETER :: TOL = 1.0E-6 
  INTEGER :: i, j, iter
  REAL*8 :: A(N, N), B(N), X(N), X_new(N), diff, sum
  
  DATA A /1.0, -0.2, 0.1,
         0.1, 0.8, -0.2,
         0.3, 0.1, 1.0/
  
  DATA B /1.0, 2.0, 3.0/
  
  DATA X /0.0, 0.0, 0.0/
  
  DO iter = 1, 1000
    diff = 0.0
    DO i = 1, N
      sum = 0.0
      DO j = 1, N
        IF (i /= j) THEN
          sum = sum + A(i, j) * X(j)
        END IF
      END DO
      X_new(i) = (B(i) - sum) / A(i, i)
      diff = MAX(diff, ABS(X_new(i) - X(i)))
    END DO
    
    IF (diff < TOL) THEN
      PRINT *, 'Converged after', iter, 'iterations.'
      EXIT
    END IF
    
    X = X_new
  END DO
  
  PRINT *, 'Solution:'
  DO i = 1, N
    PRINT *, 'X(', i, ') =', X(i)
  END DO
  
END PROGRAM GaussSeidel
