program romberg
implicit none
integer :: n, i, j, k, l
real :: a, b, exact, x(0:100), y(0:100), h, tr, rom(20,20)

exact = atan(1.)-atan(0.); n = 5; a = 0.0; b = 1.0; h = (b-a)/n
y(0) = 1;
y(1) = 1/(1+(1.0)**2)
do i = 1,n
  l = 2**(i-1)
  h = (b-a)/l

  do j = 0,l
    x(j) = a + j*h
    y(j) = 1/(1+(x(j))**2)
  end do

  tr = 0;
  do j = 1,l-1
    tr = tr + y(j)
  end do
  rom(i,1) = h*(y(0) + 2*tr + y(l))/2
end do

do j = 2,n
  do i = j,n
    rom(i,j) = rom(i,j-1) + (rom(i,j-1) - rom(i-1,j-1))/(4**(j-1) - 1)
  end do
end do

write(*,*)"Extrapolation table are shown below: "
write(*,*)
write(*,*)" O(h2) "," O(h4) ", " O(h6) ", " O(h8) ", " O(h10) "
write(*,*)" ------------------------------------------------- "

do i = 1,n
  write(*,'(5f12.8)') (rom(i,j),j=1,i)
end do

write(*,*)
write(*,*)"Exact result is: ",exact
write(*,*)"Romberg result is: ",rom(n,n)
write(*,*)"Absolute Error is: ",abs(exact-rom(n,n))
end program