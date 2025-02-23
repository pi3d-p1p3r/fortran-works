program bisection_method
  implicit none
  
  real :: a=0.0, b=2.0, c, fa, fb, fc, tolerance=1.0e-6
  integer :: i, max_iterations=1000
  
  do i = 1, max_iterations
    fa = a**2 - 2
    fb = b**2 - 2
    c = (a + b) / 2.0
    fc = c**2 - 2
    
    if (abs(fc) < tolerance) then
      write(*,*) "Root found: x =", c, "f(x) =", fc
      exit
    endif
    
    if (sign(1.0, fc) == sign(1.0, fa)) then
      a = c
    else
      b = c
    endif
    
  end do
  
  write(*,*) "Maximum iterations reached without finding root."
  
end program bisection_method