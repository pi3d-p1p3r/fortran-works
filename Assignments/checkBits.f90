program generate_bitstrings
    implicit none
    integer :: n, i, j
    character(len=3) :: bitstring

    ! Read the size of the bitstrings
    write(*,*) "Enter the size of the bitstrings (n): "
    read(*,*) n

    ! Generate and display the bitstrings
    do i = 0, 2**n - 1
        bitstring = ""
        do j = 1, n
            if (i % 2 == 0) then
                bitstring = "0" // bitstring
            else
                bitstring = "1" // bitstring
            end if
            i = i / 2
        end do

        ! Check if the bitstring contains "111" using scan
        if (scan(bitstring, "111") == 0) then
            write(*,*) bitstring
        end if
    end do

end program generate_bitstrings