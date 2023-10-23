program arrays_3
        ! Используя генератор случайных чисел, создайте массив из N элементов целого типа в диапазоне от -100 до +100. 
        ! Вычислите количество положительных и отрицательных чисел

        implicit none

        integer :: MAX_VALUE(100)
        integer ::  MIN_VALUE(-100)
        integer :: seed, i, n, positives, negatives
        integer, dimension(:), allocatable :: arr
        real :: nm

        seed = 42
        nm = aint(rand(seed)*1000)

        print*, "Input N:"
        read(*,*) n
        

        open(2, file="arrays_3.txt", status="unknown")
        write(2,*) "N=",n
        allocate( arr(1:n) )
        
        do i = 1, n
                seed = (seed + nm)*nm
                arr(i) = aint(rand(seed)*(MAX_VALUE-MIN_VALUE)) + MIN_VALUE
                if (arr(i) > 0 ) then
                        positives = positives + 1
                else if (arr(i) < 0) then 
                        negatives = negatives + 1
                endif
        end do
        
        write(2, '(2x,i3)')  (arr(i), i=1,n)
        write(2, '("positive numbers: ",i2,"; negative numbers: ",i2)') positives, negatives

        deallocate(arr)
        close(2)
end
