program marrays_2
        ! Заполните массив размерностью 10×10 целыми числами в диапазоне от -5 до 5. 
        ! Найдите нулевые элементы массива. Запомнить их координаты. Массив вывести на экран в виде таблицы.

        implicit none

        integer :: ROWS, COLS, MAX_VALUE, MIN_VALUE
        parameter(ROWS=10, COLS=10, MAX_VALUE=5, MIN_VALUE=-5)

        integer :: arr(ROWS, COLS)
        integer :: ix_zeros(ROWS * COLS, 2)
        integer i, j, seed, zeros
        real nm, r
   
        seed = 12
        zeros = 0
        nm = aint(rand(seed)*1000)
        do i = 1, ROWS
                do j = 1, COLS
                        r = aint(rand(seed)*(MAX_VALUE - MIN_VALUE)) + MIN_VALUE
                        arr(i, j) = r
                        seed = (seed+nm)*nm

                        if (arr(i,j) == 0) then
                                zeros = zeros + 1
                                ix_zeros(zeros, 1) = i
                                ix_zeros(zeros, 2) = j
                        endif
                end do
        end do

        open(2, file="marrays_2.txt", status="unknown")
        write(2, *) "Generated array:"
        write(2,'(10(2x,i2))')  ((arr(i,j),j=1,COLS),i=1,ROWS)
        write(2,*) "Coordinates of zeros:"
        do i = 1, zeros
                write(2, '("(",i2,", ",i2,")")') ix_zeros(i,1), ix_zeros(i,2)
        end do
        close(2)
end
