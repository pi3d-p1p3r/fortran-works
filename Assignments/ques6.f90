program numbers_game
implicit none 
INTEGER::i
REAL,DIMENSION(1:10)::num1,num2,num3,num4,num5, total_num, percent_num

    open(unit=3,file="final_numbers.txt")

        do i = 1,10    
        read(3,'(5(4x, f4.1))')num1(i),num2(i),num3(i),num4(i),num5(i)
        end do

    do i = 1,10
        total_num(i) = num1(i)+num2(i)+num3(i)+num4(i)+num5(i)
        percent_num(i) = (total_num(i)/500)*100
       write(*,'(4x, i4, 4x, f5.1, 4x, f4.1)')i,total_num(i),percent_num(i)
    end do    

end program 