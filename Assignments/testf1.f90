real function f(x)
real::x
f=(tan(x))**3
end function
real function g(x)
real::x
g=log(abs(cos(x)))+0.5*((tan(x))**2)
end function
program integration
    implicit none
    integer::i,n
    real::it1,it2,it3,it4,f,g,a,b,h,exact
    a=0;b=1.2;n=30
    it1=0;it2=0;it3=0;it4=0
    h=(b-a)/n
    exact=g(b)-g(a)
    do i=1,n-1
        it1=it1+f(a+(i*h))
        if(mod(i,2)==0)then[
            it2=it2+2*f(a+(i*h))
            else
                it2=it2+4*f(a+(i*h))
        end if
        if(mod(i,3)==0)then
            it3=it3+2*(f(a+(i*h)))
            else
                it3=it3+3*(f(a+(i*h)))
        end if
        if(mod(i,2)==0 .and. mod(i,6)==0 .and. mod(i,3)==0)then
            it4=it4+2*(f(a+(i*h)))
            else if(mod(i,2)==0)then
                it4=it4+(f(a+(i*h)))
                else if(mod(i,3)==0)then
                    it4=it4+6*f(a+(i*h))
                    else
                        it4=it4+5*f(a+(i*h))
        end if
    end do
    it1=(f(a)+f(b)+2*it1)*(h/2)
    write(*,*)it1
    it2=(f(a)+f(b)+it2)*(h/3)
    write(*,*)it2
    it3=(f(a)+f(b)+it3)*(h/8)
    write(*,*)it3
    it4=(f(a)+f(b)+it4)*(3*h/8)
    write(*,*)it4
    write(*,"(f15.8,5x,A,f15.8,5x,f15.8)")exact,"trap",abs(exact-it1),abs((exact-it1)/exact)
    write(*,"(f15.8,5x,A,f15.8,5x,f15.8)")exact,"sim 1/3",abs(exact-it2),abs((exact-it2)/exact)
    write(*,"(f15.8,5x,A,f15.8,5x,f15.8)")exact,"sim 3/8",abs(exact-it3),abs((exact-it3)/exact)
    write(*,"(f15.8,5x,A,f15.8,5x,f15.8)")exact,"weddle",abs(exact-it4),abs((exact-it4)/exact)
end program