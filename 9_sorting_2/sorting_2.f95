program sorting_2

        implicit none

        integer :: MAX_VALUE = 100
        integer ::  MIN_VALUE = 0
        integer :: i, j, n, v, k, kk
        integer, dimension(:), allocatable :: arr
        real :: r

        call srand(42)
        r = rand()

        print*, "Input N:"
        read(*,*) n
        
        k = 0
        kk = 0

        open(2, file="sorting_2.txt", status="unknown")
        write(2,*) "N=",n
        allocate(arr(1:n))
        
        ! Инициализация массива
        do i = 1, n
                arr(i) = aint(rand()*(MAX_VALUE-MIN_VALUE)) + MIN_VALUE
        end do

        ! Тестовый набор данных из примера
        ! n = 10
        ! arr(1) = 6
        ! arr(2) = 95
        ! arr(3) = 33
        ! arr(4) = 84
        ! arr(5) = 18
        ! arr(6) = 32
        ! arr(7) = 57
        ! arr(8) = 68
        ! arr(9) = 94
        ! arr(10) = 74
       
        write(2, *) "Исходный массив:" 
        write(2, '(10i3)')  (arr(i), i=1,n)

        ! Сортировка
        do i = 1, n
                j = i
                v = arr(i)
                do while ((j > 1).and.(arr(j-1) > v))
                        arr(j) = arr(j-1) 
                        k = k + 1
                        kk = kk + 1
                        j = j - 1
                end do
                arr(j) = v
        end do
        
        write(2, *) "Отсортированный массив"
        write(2, '(10i3)')  (arr(i), i=1,n)
        write(2, *) "Сравнений:",k,"Перестановок:",kk
        deallocate(arr)
        close(2)
end
