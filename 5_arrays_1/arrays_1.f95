program arrays_1
        ! Используя генератор случайных чисел, создайте массив из 10 элементов целого типа и вычислите их сумму.

        implicit none 
        real :: nm
        integer :: i, s, n
        integer :: arr(10)

        n = 42
        nm = aint(rand(n)*1000)
        s = 0

        open(2, file="arrays_1.txt", status="unknown")
        do i = 1, 10
                n = (n + nm)*nm
                arr(i) = aint(rand(n)*10)
                s = s + arr(i)
                ! write(2,'(i2,") ",i2)') i, arr(i)
        end do

        write(2, '(2x,i3)')  (arr(i), i=1,10)
        write(2,'("Array sum=",i3)') s
        close(2)
end
