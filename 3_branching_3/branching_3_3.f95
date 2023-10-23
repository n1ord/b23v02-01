program branching_3_3
        ! Задание 3, формула №3
        implicit none
        real :: x, y
        
        print*, 'Input x(real):'
        read(*,*) x
        
        open(2, file="branching_3_3.txt", status="unknown")
        write(2, *) "x=", x
        if ((-2 < x).and.(x <= 2)) then
                write(2, *) "-2 < x <= 2"
                y = sqrt(4 - x**2)
        elseif ((2 < x).and.(x <= 4)) then
                write(2, *) "2 < x <= 4"
                y = x - 2
        else
                write(2, *) 'y value is undefined'
                call exit(1)
        endif
        
        write(2, *) y
        close(2)
end
