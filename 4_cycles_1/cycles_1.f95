program cycles_1
        ! Найдите сумму членов арифметической прогрессии
        !       a, a + d, a + 2d, ..., a + (n-1)d
        ! Значения величин a, d, n  вводить с клавиатуры.
        
        implicit none

        integer :: a, d, n, i, res

        res = 0

        print*, "Input a,d,n:"
        read(*,*) a,d,n
        do i=1, n, 1
                res =  res + (a + (i-1)*d)
        end do

        open(2, file="cycles_1.txt", status="unknown")
        write(2,*) res
        close(2)
end
