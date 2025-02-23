program test
implicit none
integer::n
real::tax,tax_final,medicare
write(*,*)"Please write down your annual income in AUD:"
read(*,*)n

medicare = 0.015*n

    if (n<6001) then 
    tax = 0
    else if ((n>=6001) .and. (n<=34000)) then 
    tax = 0.15*(n-6000)
    else if ((n>=34001) .and. (n<=80000)) then 
    tax = 4200 + 0.3*(n-34000)
    else if ((n>=80001) .and. (n<=180000)) then 
    tax = 18000 + 0.4*(n-80000)
    else if (n>180000) then 
    tax = 58000 + 0.45*(n-180000)
    else
    write(*,*)"Invalid amount"
    end if

tax_final = medicare + tax

write(*,*)"The income tax is",tax
write(*,*)"The Medicare Levy is",medicare
write(*,*)"The total tax is",tax_final
end program