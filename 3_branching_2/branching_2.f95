program branching_2
        ! Добавьте в предшествующую программу поиск минимального числа и выведите числа в следующем порядке – 
        ! «минимальное число, среднее число, максимальное число» - 
        ! числовые значения с соответствующими текстовыми пояснениями 
        ! (среднее число - среднее по величине из трёх введённых чисел, а не среднеарифметическое).

        implicit none
        real :: x1, x2, x3, tmp

        print*, "Input 3 numbers here:"
        read(*,*) x1,x2,x3

        open(2, file="branching_2.txt", status="unknown")
        write(2,*)  'Input:', x1, x2, x3

        ! ищем минимум среди [x1,x2,x3] и перемещаем его в x1
        if ((x1 >  x2).or.(x1 > x3)) then
                if (x2 < x3)  then
                        tmp = x1
                        x1 = x2
                        x2 = tmp
                else
                        tmp = x1
                        x1 = x3
                        x3 = tmp
                endif        
        endif

        ! ищем минимум среди остатка [x2, x3] и перемещаем его в x2
        if (x2 > x3) then
                tmp = x2
                x2 = x3
                x3 = tmp
        endif
        
        write(2,*)  'Minumum =',x1,' Average =',x2,'Maximum =',x3
        close(2)

end
