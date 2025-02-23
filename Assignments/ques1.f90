program marks
implicit none
integer::a
real::c,mid,final,total
write(*,*)"Enter the value of attendance, CT, mid and final"
read(*,*)a,c,mid,final

total = 0.1*a + 0.2*c + 0.2*mid + 0.5*final

if(total>=90)then
        print*,"the grade is A"
    else if(total>=80 .AND. total<90) then
        print*,"the grade is B"
    else if(total>=70 .AND. total<80) then
        print*,"the grade is C"
    else if(total>=60 .AND. total<70) then
        print*,"the grade is D"
    else
        print*,"the grade is F"
end if

end program