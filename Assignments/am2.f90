program amicable_numbers
  implicit none
  integer :: i, sum1, sum2

  do i = 1, 10000
    sum1 = sum_of_proper_divisors(i)
    sum2 = sum_of_proper_divisors(sum1)
    if (i == sum2 .and. sum1 .ne. i) print*, i, sum1
  end do

contains
  function sum_of_proper_divisors(n) result(s)
    integer :: i, n, s
    s = 0
    do i = 1, n-1
      if (mod(n,i) == 0) s = s + i
    end do
  end function sum_of_proper_divisors
end program amicable_numbers