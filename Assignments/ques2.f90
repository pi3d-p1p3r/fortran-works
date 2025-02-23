program quad_eqn
implicit none
REAL::a,b,c
COMPLEX::x1,x2,ai,bi,ci

write(*,*)"Please enter the values of a, b and c: "
read(*,*)a,b,c
ai=COMPLEX(a,0)
bi=COMPLEX(b,0)
bi=COMPLEX(c,0)

x1 = (-bi + SQRT(bi**2-4*ai*c))/(2*ai)
x2 = (-bi + SQRT(bi**2-4*ai*c))/(2*ai)

if(REAL(b)**2-4*REAL(a)*REAL(c)>=0) then
    write(*,*)"The roots are real"
    else if (REAL(b)**2-4*REAL(a)*REAL(c)<0) then
    write(*,*)"The roots are complex"
end if

write(*,*)"The first root is ", x1
write(*,*)"The second root is ", x2

write(*,*)
end program