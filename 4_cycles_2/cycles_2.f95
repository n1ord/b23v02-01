program cycles_2
        ! В цикле с клавиатуры вводятся произвольные числа до тех пор, пока не будет введено отрицательное число. 
        ! Посчитать, сколько введено положительных чисел. 

        implicit none
        integer :: a

        a = 0
        do while (a >= 0)
                write(*,*) "Input negative number to stop"
                read(*,*) a
        end do
end

