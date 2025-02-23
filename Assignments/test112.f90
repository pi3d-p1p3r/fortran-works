Program sqroot
Implicit None
Real,Dimension(10)::Root
Real,Dimension(10)::Digit
Integer::i,j
Do i=1,10
    Digit(i)=Real(i)
    Root(i)=Real(Sqrt(Digit(i)))
    Write(*,5)Digit(i),Root(i)
    5 format("Number = ",f5.1,4x,"Root = ",f4.2)
End Do
End Program