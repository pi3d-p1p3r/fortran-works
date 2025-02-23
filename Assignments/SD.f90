PROGRAM StandardDeviation
  IMPLICIT NONE

  INTEGER, PARAMETER :: N = 10
  REAL :: X(N), SD

  DATA X/1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0/

  CALL CalcSD(X, N, SD)

  WRITE(*,*) 'Standard Deviation = ', SD

END PROGRAM StandardDeviation


SUBROUTINE CalcSD(X, N, SD)
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: N
  REAL, INTENT(IN) :: X(N)
  REAL, INTENT(OUT) :: SD

  INTEGER :: I
  REAL :: SUM, SUMSQ, MEAN

  SUM = 0.0
  SUMSQ = 0.0
  DO I=1, N
    SUM = SUM + X(I)
    SUMSQ = SUMSQ + X(I)*X(I)
  END DO
  MEAN = SUM / N
  SD = SQRT(SUMSQ/N - MEAN*MEAN)
  
END SUBROUTINE CalcSD
