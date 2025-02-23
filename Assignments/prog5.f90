program newton_interpolation
  implicit none
  
  integer, parameter :: n = 6 
  real :: x(n), y(n) 
  real :: t, f 
  real :: d(n,n) 
  integer :: i, j 
  
  x = (/ 1.0, 2.0, 3.0, 4.0, 6.0, 10.0 /)
  y = (/ 181.0, 155.0, 161.5, 183.0, 214.1, 319.0 /)
  
  d(:,1) = y
  do j = 2, n
     do i = j, n
        d(i,j) = (d(i,j-1) - d(i-1,j-1)) / (x(i) - x(i-j+1))
     end do
  end do
  
  t = 7
  f = y(1)
  do i = 2, n
     f = f + d(i,i) * product(t - x(1:i-1))
  end do
  
  write(*,*) "Interpolated value at t = ", t, " is ", f
  
contains

  real function product(xdiff)
    real, intent(in) :: xdiff(:)
    integer :: Ln
    integer :: count,i

    count = 1
    count = count + 1

    do i = 1,n
        if(i==count) Ln = count
    end do

    product = 1.0
    do i = 1, Ln
       product = product * xdiff(i)
    end do
  end function product
  
end program newton_interpolation
