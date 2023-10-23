program branching_3_1
        ! Задание 3, формула №1
        implicit none
        real :: x, y

        print*, 'Input x(real):'
        read(*,*) x

        open(2, file="branching_3_1.txt", status="unknown")
        write(2, *) "x =", x
        if ((-9 <= x).and.(x < -6)) then
                write(2,*) "-9 <= x < -6"
                y = -3 + sqrt(9 - (9+x)**2) 
        elseif ((-6 <= x).and.(x < -3)) then
                write(2,*) "-6 <= x < -3"
                y = 3 + x
        elseif ((-3 <= x).and.(x < 0)) then
                write(2,*) "-3 <= x < 0"
                y = sqrt((3 + x)**2)
        else
                write(2,*) 'y value is undefined for this x'
                call exit(1)
        endif
        
        write(2, *) y
        close(2)
end
