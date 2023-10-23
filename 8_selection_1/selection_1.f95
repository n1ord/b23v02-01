program selection_1
        implicit none

        integer :: seed, i, found, k, key, l, max_val, max_ix, tmp
        integer :: N, MAX_VALUE, M, U, P
        parameter(N=100, MAX_VALUE=11)
        integer :: arr(N) ! исходный массив, размера N
        integer :: arr2(N) ! массив с уникальными значениями, размера M
        integer :: idx(N) ! массив индексов найденных элементов, размера P        
        integer :: uniques(0:MAX_VALUE) ! вспомогательный массив для
                                        ! определения дубликатов,
                                        ! размера U
        real :: r
        seed = 42
        call srand(seed)
        !call random_seed(seed)

        write(*,*) "Введите k [0..11]:"
        read(*,*) k     

        if ((k > MAX_VALUE).or.(k < 0)) then
                write(*,*) "Некорректный k"
                call exit(1)
        endif
        
        ! инициализация всп. массива нулями
        U = MAX_VALUE
        do i = 0, U
                uniques(i) = 0
        end do

        ! заполнение исх. массива случ. числами
        ! с одновременным отделением уникальных элементов
        ! во вспомогательный массив
        M = 0
        do i = 1, N
                !call random_seed(seed)
                !call random_number(r)
                r = rand()
                arr(i) = aint(r * MAX_VALUE)
                if (uniques(arr(i)) == 0) then
                        uniques(arr(i)) = i
                        M = M + 1
                        arr2(M) = arr(i)
                endif
        end do

        
        ! поиск key - сортировка хвоста вспомог. массива, вплоть до k-того
        ! элемента от начала
        ! ( вообще массив uniques заведомо отсортирован по возрастанию и можно бы
        ! сделать быстрее: key = arr(uniques(k)) )
        l = M
        do while (l >= k)
                max_val = -1
                max_ix = 0
                do i = 1, l
                        if (arr2(i) > max_val) then
                                max_val = arr2(i)
                                max_ix = i
                        endif
                end do
                tmp = arr2(l)
                arr2(l) = arr2(max_ix)
                arr2(max_ix) = tmp
                l = l - 1
        end do
        key = arr2(k)
       
        ! Поиск индексов элементов исх.массива, равных
        ! искомому key
        P = 0
        do i = 1, N
                if (arr(i) == key) then
                        P = P + 1
                        idx(P) = i
                endif
        end do
        
        open(2, file="selection_1.txt", status="unknown")
        write(2, *) "Размер массива:", N
        write(2, *) "Исходный массив"
        write(2,'(10i3)')  (arr(i),i=1,N)
        write(2, *) "Вспомогательный массив (отсортирован c k-того эл-та)"
        write(2,'(10i3)')  (arr2(i),i=1,M)
        write(2, *) "Значение k-того элемента:", key
        write(2, *) "Найденные индексы"
        write(2,'(10i3)')  (idx(i),i=1,P)
        close(2)
end
