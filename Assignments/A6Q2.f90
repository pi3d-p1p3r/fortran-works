program jacobi
   implicit none
   integer, parameter :: n=3
   double precision:: a(n,n+1),e(0:n),x(n,0:100)
   integer::i,j,k
   real::norm,s

   open(3,file='in6q2.txt')
   open(4,file='outq2.txt')

   read(3,*)((a(i,j),j=1,n+1),i=1,n)
   write(4,"(A)")"(A|b)="
   write(4,"(4(f8.2))")((a(i,j),j=1,n+1),i=1,n)

   do i = 1,n
      x(i,0)=0.0
   end do
   e(0)=0.0

   do j=1,n+1
   call swapRows(n,a,j)
   end do
   write(4,"(A)")"After Partial Pivoting, (A|b)="
   write(4,"(4(f8.2))")((a(i,j),j=1,n+1),i=1,n)

   write(4,"(/,A,5x,A,9x,A,8x,A)")"k","x1","x2","x3"
   k=1
   do 
   do i = 1, n
      s=a(i,n+1)
      do j = 1,n
         if(i==j) cycle
         s=s-a(i,j)*x(j,k-1)
      end do

      x(i,k)=s/a(i,i)
      write(4,"(I0)",Advance="No")k
      write(4,"(F10.5)",Advance="No")x(i,k)
      e(i)=ABS(x(i,k)-x(i,k-1))
      if(e(i)>e(i-1)) norm = e(i)
   end do

   if(norm<0.0005) exit 
   k = k + 1
   write(4,"(/)")
   end do

   end program

   subroutine swapRows(n,a,j)
   implicit none

   integer::n,i,j,p
   double precision :: a(n,n+1),t(n+1)

   p=j
   do i = j,n
   if(abs(a(i,j))>abs(a(p,j))) p=i
   end do

   t = a(j,:)
   a(j,:)=a(p,:)
   a(p,:)=t

   end subroutine
