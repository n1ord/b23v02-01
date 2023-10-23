program arrays_2
        ! Используя генератор случайных чисел, создайте массив из
        ! N элементов целого типа. Найдите элементы, имеющие минимальное
        ! и максимальное значения, запомните их местоположение в массиве
        ! (порядковые индексы) и вычислите количество элементов массива,
        ! которые расположены между ними. Для определения наибольшего
        ! элемента введите дополнительную переменную (например, maxel) и
        ! занесите в неё значение первого элемента массива. В цикле
        ! последовательно сравнивайте элементы массива с содержимым
        ! переменной maxel. Если какой-либо из очередных элементов имеет
        ! большее значение, чем maxel, то это значение нужно занести в
        ! maxel, заменив прежнее значение. Одновременно нужно запоминать
        ! местоположение наибольшего элемента в массиве (т.е. его
        ! порядковый номер).  Аналогично определите наименьший элемент
        ! массива.
        implicit none

        real :: nm
        integer :: MAX_RANGE = 100 ! верхняя граница значений массива
        integer :: i, s, n, seed
        integer, dimension(:), allocatable :: arr
        integer :: maxel, minel, ix_maxel, ix_minel

        maxel = 0
        minel = MAX_RANGE
        ix_maxel = -1
        ix_minel = -1
        seed = 42
        nm = aint(rand(seed)*1000)
        s = 0

        print*, "Input N:"
        read(*,*) n

        open(2, file="arrays_2.txt", status="unknown")
        write(2,*) "N=",n
        allocate( arr(1:n) )
        do i = 1, n
                seed = (seed + nm)*nm
                arr(i) = aint(rand(seed)*MAX_RANGE)

                if (arr(i) > maxel) then
                        maxel = arr(i)
                        ix_maxel = i
                endif

                if (arr(i) < minel) then
                        minel = arr(i)
                        ix_minel = i
                endif
        end do
        
        write(2, '(2x,i3)')  (arr(i), i=1,n)
        write(2,'("min=",i2," at ",i2,"; max=",i2," at ",i2)') minel, ix_minel, maxel, ix_maxel 
        write(2,'("distance=",i3)') abs(ix_maxel-ix_minel-1)

        deallocate( arr )
        close(2)
end
