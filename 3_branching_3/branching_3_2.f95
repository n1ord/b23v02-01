program branching_3_2
        ! Задание 3, формула №2
        implicit none
        real :: x, y

        print*, 'Input x(real):'
        read(*,*) x


        open(2, file="branching_3_2.txt", status="unknown")
        write(2, *) "x =", x
        if (x <= -2) then
                write(2, *) "x <= -2"
                y = -2
        elseif ((-2 < x).and.(x < 0)) then
                write(2, *) "-2 <= x < 0"
                y = 2 + x
        else
                write(2, *) 'y value is undefined'
                call exit(1)
        endif
        
        write(2, *) y
        close(2)
end
