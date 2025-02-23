program start
            implicit none
            integer::i,j,k,m,n,p,q
            integer,allocatable,dimension(:,:)::mata,matb,matc,matd
            open(2,file="ip.txt")
            open(3,file="op.txt")
            write(3,*)"read m,n,p,q: "
            read(2,*)m,n,p,q
            allocate(mata(m,n),matb(p,q),matc(m,q))
            write(3,*)"a: "
            read(2,*)((mata(i,j),j=1,n),i=1,m)
            do i=1,m
                write(3,*)(mata(i,j),j=1,n)
            end do
            write(3,'(/)')
            write(3,*)"b: "
            read(2,*)((matb(i,j),j=1,q),i=1,p)
            do i=1,p
                write(3,*)(matb(i,j),j=1,q)
            end do
            write(3,'(/)')
            write(3,*)"the product is: "

            call multiplication(mata,matb,m,n,p,q,matc)
            write(3,'(/)')
            write(3,*)"the matmul product is: "

            matd=matmul(mata,matb)
            do i=1,m
            write(3,*)(matd(i,j),j=1,q)
            end do
            write(3,'(/)')
            if(matd(m,q)==matc(m,q))then
                write(3,*)"verified"
            end if
       end program

         subroutine multiplication(matm,matn,m,n,p,q,matmn)
            implicit none
            integer,intent(in)::m,n,p,q
            integer,intent(in),dimension(m,n)::matm
            integer,intent(in),dimension(p,q)::matn
            integer,intent(out),dimension(m,q)::matmn
            integer::i,j,k
            if(n==p)then
                do i=1,m
                    do j=1,q
                        matmn(i,j)=0

                        do k=1,n
                            matmn(i,j)=matmn(i,j)+matm(i,k)*matn(k,j)
                        end do
                    end do
                end do
                else
                    write(3,*)"cant"
            end if
            do i=1,m
                write(3,*)(matmn(i,j),j=1,q)
            end do
         end subroutine