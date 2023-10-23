program marrays_4
        
        implicit none

        integer :: ROWS, COLS
        parameter(ROWS=10, COLS=10)

        integer :: arr(ROWS, COLS)
        integer :: i, j
        integer :: N, prev, offs

        write(*,*) 'Input N [<=10]:'
        read(*,*) N
        prev = 1 
        offs = 0
        do i = 1, N
                do j = 1, N
                        arr(i, j) =  prev + offs
                        prev = arr(i,j)
                        offs = offs + 1
                end do
                prev = arr(i,2)
                offs = offs - N + 2
        end do

        open(2, file="marrays_4.txt", status="unknown")
        write(2, *) "Generated array:"
        do i = 1, N
                 write(2,'(100i5)')  (arr(i,j),j=1,N)
        end do
        close(2)


end
