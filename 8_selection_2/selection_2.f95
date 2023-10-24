program selection_2
        implicit none

        integer :: seed, i, found, k, key, l, max_val, max_ix, tmp
        integer :: p1, p2, j1, j2, n1, n2
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
        r = rand()

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
                r = rand()
                arr(i) = aint(r * MAX_VALUE)
                if (uniques(arr(i)) == 0) then
                        uniques(arr(i)) = i
                        M = M + 1
                        arr2(M) = arr(i)
                endif
        end do

        
        ! поиск алгоритмом Хоара, он же quickselect
        n1 = 1
        n2 = M
        do while ((n1 < n2).and.(p /= k))
                ! выбор pivot точки 
                p = (n2 + n1) / 2
                p1 = arr2(p)

                ! установка текущих границ циклов
                j1 = n1
                j2 = n2

                ! индексы j1, j2 "сходятся" друг другу, на каждом шаге
                ! выполняется перестановка элементов в массиве arr2 так,
                ! что элементы меньше или равно arr2(p) лежат слева, больше - справа
                do while (j1 < j2)
                        if (arr2(j1) < p1) then
                                j1 = j1 + 1
                        else if (arr2(j2) > p1) then
                                j2 = j2 - 1
                        else
                                call swap(arr2(j1), arr2(j2))
                        endif
                end do
                
                ! установка новых границ для следующего цикла поиска
                p = j1
                if (p < k) then
                        n1 = p + 1
                else if (p > k) then
                        n2 = p - 1
                endif            

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
        
        open(2, file="selection_2.txt", status="unknown")
        write(2, *) "Размер массива:", N
        write(2, *) "Исходный массив"
        write(2,'(10i3)')  (arr(i),i=1,N)
        write(2, *) "Вспомогательный массив"
        write(2,'(10i3)')  (arr2(i),i=1,M)
        write(2, *) "Значение k-того элемента:", key
        write(2, *) "Найденные индексы"
        write(2,'(10i3)')  (idx(i),i=1,P)
        close(2)
end

subroutine swap(a, b)
  integer, intent(inout) :: a, b
  integer :: temp
  temp = a
  a = b
  b = temp
end subroutine swap
