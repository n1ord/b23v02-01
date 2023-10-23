program marrays_3
        ! Сформируйте матрицу из целых чисел размерностью N×N (предельный размер 10×10) в соответствии с 
        ! приведённой ниже схемой заполнения ячеек целыми числами. Элементы главных диагоналей матрицы равны нулю. Остальные секторы матриц заполнить в соответствии с Рис.3. Рассмотрите матрицы с чётным и нечётным значением N. Полученные матрицы вывести на экран в виде двумерных таблиц. 
        implicit none

        integer :: ROWS, COLS
        parameter(ROWS=10, COLS=10)

        integer :: arr(ROWS, COLS)
        integer :: labels(4)
        integer :: i, j, rj, ix1, ix2, sec_num
        integer :: N

        labels = [1, 4, 3, 2]
        write(*,*) 'Input N [<=10]:'
        read(*,*) N
       
        do i = 1, N
                do j = 1, N
                        ix1 = i - j
                        ix2 = i - (N - j + 1)
                        ix1 = (ix1/abs(ix1) + 1)/2
                        ix2 = (ix2/abs(ix2) + 1)/2

                        ! вычисление номера сектораш
                        sec_num = (ix1)*2 + (ix2)*1 + 1 
                                
                        if ((i == j).or.(i == (N - j+1))) then
                                arr(i, j) = 0
                        else
                                arr(i,j) = labels(sec_num)
                        endif
                end do
        end do

        open(2, file="marrays_3.txt", status="unknown")
        write(2, *) "Generated array:"
        do i = 1, N
                ! write(2,'(2x,i2)')  (arr(i,j),j=1,N)
                 write(2,'(100i5)')  (arr(i,j),j=1,N)
        end do
        close(2)

end
