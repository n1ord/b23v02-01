program branching_1
        !  Задайте три вещественных числа и найдите наибольшее из них.
        implicit none
        real :: x1, x2, x3

        print*, "Input 3 numbers here:"
        read(*,*) x1,x2,x3

        open(2, file="branching_1.txt", status="unknown")
        if ((x1 > x2).and.(x1 > x3)) then
                write(2,*) "first is maximum"
                write(2,*) x1
        elseif ((x2 > x1).and.(x2 > x3))  then
                write(2,*) "second is maximum"
                write(2,*) x2
        else
                write(2,*) "third is maximum"
                write(2,*) x3
        endif
        close(2)
end
