program marrays_5
        ! Сформируйте матрицу размерностью N×N   в соответствии с приведённой ниже схемой заполнения   
        ! ячеек целыми числами. 

        implicit none

        integer :: ROWS, COLS
        parameter(ROWS=10, COLS=10)

        integer :: arr(ROWS, COLS)
        integer :: i, j, c
        integer :: N, side, ring
        
        write(*,*) 'Input N [<=10]:'
        read(*,*) N

        c = 0

        ! итерирование по числу колец к центру
        do ring = 0, (N/2)
                side = N - ring*2 - 1

                ! итерирование по стороне текущего кольца
                do i = 1, side
                        arr(ring + 1, i + ring) = c + i ! верхняя сторона кольца
                        arr(i + ring, N - ring) = c + side + i ! правая сторона
                        arr(N - ring, N - ring - i + 1) = c + side*2 + i ! нижняя сторона
                        arr(N - ring - i + 1, ring + 1) = c + side*3 + i ! левая сторона
                end do
                c = c + (side)*4
        end do

        ! для матриц нечетного N, финальный центральный элемент
        if (side == 0) then
                arr(ring, ring) = c + 1
        endif

        open(2, file="marrays_5.txt", status="unknown")
        write(2, *) "Generated array:"
        do i = 1, N
                 write(2,'(100i5)')  (arr(i,j),j=1,N)
        end do
        close(2)

end 
