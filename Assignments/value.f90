program value

    implicit none
    real::f,x
    
    x=
    f=(EXP((-1)*x))*(COS((SQRT(3.0))*x) + (2*SQRT(3.0))*SIN((SQRT(3.0))*x))-20
    write(*,*)f
    

end program 
