program rom
  implicit none 
  real::x(0:100),y(0:100),a=0.0,b=1.0,h,sum,r(20,20)
  integer::i,j,l,n=5
  y(0)=1.0
  y(1)=0.5
   h=(b-a)/n
    do i=1,n 
      l=2**(i-1)
      h=(b-a)/l
        do j=0,l 
          x(j)=a+j*h
          y(j)=1./(1.+(x(j)**2))
        end do 
          sum=0;
            do j=1,l-1
              sum=sum+y(j)
            end do 
               r(i,1)=h*(((y(0)+y(l))/2)+sum)
      end do
        
        do j=2,n
          do i=j,n 
            r(i,j)=r(i,j-1)+((r(i,j-1)-r(i-1,j-1))/((4**(j-1))-1))
          end do 
        end do    

        do i=1,n 
          write(*,'(5f12.8)')(r(i,j),j=1,i)
        end do  


end program  