PROGRAM Fermat_Numbers

  IMPLICIT NONE
  LOGICAL::is_prime=.true.
  INTEGER(KIND=16) :: i,j,n
  INTEGER(KIND=16),DIMENSION(0:5) :: fermat
  
  WRITE(*,*) 'The first 6 Fermat numbers are:'
  WRITE(*,*) 'n            Fn'
  WRITE(*,*) '=           ===='
  
  DO i = 0, 5
    fermat(i) = 2**(2**(i)) + 1
    WRITE(*,'(I2,1X,I20)') i, fermat(i)
  END DO

 DO i = 0,5
    n = fermat(i)
    DO j = 2,n-1
        IF(MOD(n,j)==0) THEN
        is_prime = .false.
        EXIT
        END IF
    END DO
    IF(is_prime .eqv. .true.) WRITE(*,'(A10,I12,A12)')"The number",fermat(i),"is prime."
    IF(is_prime .eqv. .false.) WRITE(*,'(A10,I12,A16)')"The number",fermat(i),"is not prime."
 END DO
END PROGRAM Fermat_Numbers