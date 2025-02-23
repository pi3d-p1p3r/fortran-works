Program Sub_routine
Implicit None
Real::a,b,c
Read(*,*)a,b
Call hypo(a,b,c)
Write(*,*)c
end program


Subroutine hypo(x,y,z)
Implicit none
Real,INTENT(IN)::x,y
real,INTENT(OUT)::z
z=sqrt(x**2+y**2)
End Subroutine hypo