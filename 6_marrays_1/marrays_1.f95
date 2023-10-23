program marrays_1
        ! Заполните массив размерностью 5×6 вещественными числами (положительными и отрицательными). 
        ! Найдите минимальный и максимальный по модулю элементы массива. Запомните их координаты. 
        ! Массив вывести на экран в виде таблицы.

        implicit none

        integer :: ROWS, COLS, MAX_VALUE, MIN_VALUE
        parameter(ROWS=5, COLS=6, MAX_VALUE=100, MIN_VALUE=-100)

        real :: arr(ROWS, COLS)
        integer :: ix_max(2), ix_min(2)
        integer i, j, seed
        real nm, r, max_val, min_val
   
        seed = 12
        max_val = MIN_VALUE - 1 
        min_val = MAX_VALUE + 1

        nm = aint(rand(seed)*1000)
        do i = 1, ROWS
                do j = 1, COLS
                        r = (rand(seed)*(MAX_VALUE - MIN_VALUE)) + MIN_VALUE
                        arr(i, j) = r
                        seed = (seed+nm)*nm

                        if (arr(i,j) > max_val) then
                                max_val = arr(i,j)
                                ix_max(1) = i
                                ix_max(2) = j
                        endif

                        if (arr(i,j) < min_val) then
                                min_val = arr(i,j)
                                ix_min(1) = i
                                ix_min(2) = j
                        endif

                end do
        end do

        open(2, file="marrays_1.txt", status="unknown")
        write(2, *) "Generated array:"
        write(2,'(6(2x,f8.4))')  ((arr(i,j),j=1,COLS),i=1,ROWS)
        write(2,'("minimum=",f9.4," at (",i2,",",i2," )")') min_val,ix_min(1),ix_min(2) 
        write(2,'("maximum=",f9.4," at (",i2,",",i2," )")') max_val,ix_max(1),ix_max(2) 
        close(2)

end

