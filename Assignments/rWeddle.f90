program a5q4 
    implicit none 
        real::f,x,h,sum,odd,even,mult,rem,sumw,ev=Log(169./25.)
        real::t,s1,s2,w,abs1,abs2,abs3,abs4
        integer::i,j
        !trapezoidal 
            h=2.4/30
            sum=0.0
            do i=1,29
                sum=sum+f(i*h)
            end do 
            t=h*(((f(0.)+f(2.4))/2.)+sum)
            write(*,*)t

        !1/3 
            odd=0.0
            do i=0,14   
                odd=odd+f(((2*i)+1)*h)    
            end do 
            
            even=0.0
            do i=0,14   
                even=even+f((2*i+2)*h)    
            end do 

            s1=(h/3)*((f(0.)+f(2.4))+(4*odd)+(2*even))   
            write(*,*)s1

         !3/8 
            mult=0.0
            do i=1,9  
                mult=mult+f((3*i)*h)    
            end do 
            
            rem=0.0
            do i=1,20   
                rem=rem+f(i*h)    
            end do 

            s2=((3*h)/8.)*((f(0.)+f(2.4))+(3*rem)+(2*mult))   
            write(*,*)s2


            !weddle 15 16 15 25
            do i=1,29
                if(mod(i,2)==0 .and. mod(i,6)==0 .and. mod(i,3)==0)then
                    sumw=sumw+2*f(0+i*h)
                else if(mod(i,2)==0)then 
                    sumw=sumw+f(0+i*h)  
                else if(mod(i,3)==0)then 
                    sumw=sumw+6*f(0+i*h)
                else    
                    sumw=sumw+5*f(0+i*h)
                end if 
            end do 

                w=((3.*h)/10.)*((f(0.)+f(2.4))+sumw)        

            abs1=abs(ev-t)
            abs2=abs(ev-s1)
            abs3=abs(ev-s2)
            abs4=abs(ev-w)
            

            write(*,*)"Tabular data-"
            write(*,2)
            2 format(6x,"actual",14x,"rules",19x,"abs error",12x,"rel error")
            write(*,*)"__________________________________________________________________________________"
            write(*,*)ev,"trapezoidal:",t,abs1,abs1/ev 
            write(*,*)ev,"1/3 rule:",s1,abs2,abs2/ev
            write(*,*)ev,"3/8 rule:",t,abs3,abs3/ev
            write(*,*)ev,"weddle's rule:",w,abs4,abs4/ev 
            
             
            
end program 


function f(x)
    implicit none 
    real::f,x 
        f=(2.*x)/(1.+(x**2))
end function        