Program NewtonRaphson
    Implicit None
    REAl :: f,df,x0,x1,tol,soln,off
    Integer :: i

    tol=10**(-5)
    x0=-4

   Write(*,21)
   21 FORMAT ("The calculation table of the Newton Raphson is given below :",///)

   Write(*,23)
   23 FORMAT ("No of Iteration",10X,"Pn-1",15X,"f'(Pn-1)",10X,"Pn",15X,"f(Pn)",/)

    Do i = 1,100
    f=16*x0**4+88*x0**3+159*x0**2+76*x0-240
    df=64*x0**3+264*x0**2+318*x0+76
        x1=x0-(f/df)
        Write (*,33) i,x0,df,x1,f
        33 Format(5X,I0,10X,F15.10,10X,F15.10,10X,F15.10,10X,F15.10)

        If(f<tol)then
            soln=x1
            Exit
        End If
        x0=x1
    End Do

End Program NewtonRaphson
