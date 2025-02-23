program fuck
    implicit none
    integer::i
    real,dimension(1:10)::num1,num2,num3,num4,num5,total_num,final_num
    open(6,file="final_numbers.txt")
    do i=1,10
    read(6,"(4x,f4.1,4x,f4.1,4x,f4.1,4x,f4.1,4x,f4.1)")num1(i),num2(i),num3(i),num4(i),num5(i)
end do
write(6,"(4x,f4.1,4x,f4.1,4x,f4.1,4x,f4.1,4x,f4.1)")num1(i),num2(i),num3(i),num4(i),num5(i)

end program