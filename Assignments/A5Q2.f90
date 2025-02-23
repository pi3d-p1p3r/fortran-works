program richardson_extrapolation
  implicit none
  integer :: i,j,k,n
  real::l,h,exact,f,x0
  real,allocatable,dimension(:,:)::ri
  write(*,*)"Please enter the order you want to find: "
  read(*,*)n
  allocate(ri(n,n))
  x0=2.0
  exact = exp(x0)*(1+x0)
  do j = 1,n
    h=0.2
    h=h/2**(j-1)
    ri(j,1)=(f(x0+h)-f(x0-h))/(2*h)
  end do
  do j = 2,n
    do i = j,n
      ri(i,j)=ri(i,j-1)+(ri(i,j-1)-ri(i-1,j-1))/(4**(j-1)-1)
    end do
  end do

  write(*,'(5(2x,a6,8x))')"O(h2)","O(h4)","O(h6)","O(h8)","O(h10)"
  write(*,*)"========================================================================"

  do i = 1,n
    write(*,'(5f15.8)')(ri(i,j),j=1,i)
  end do

end program richardson_extrapolation

real function f(x)
implicit none
real::x
f = x*exp(x)
end function