program sorting_3

        implicit none
        
        integer :: MAX_SIZE
        parameter(MAX_SIZE=200)
        integer :: MAX_VALUE = 200
        integer ::  MIN_VALUE = 0
        integer :: i, j, n, v, k, l
        integer :: s0(MAX_SIZE)
       
        integer :: base
        parameter(base=10)
        integer :: rf
        parameter(rf=3)
        integer :: p
        integer :: counts(0:(base-1))
        integer :: buckets(0:base-1,0:MAX_SIZE-1)
       
        real :: r

        call srand(42)
        r = rand()

        print*, "Input N:"
        read(*,*) n

        open(2, file="sorting_3.txt", status="unknown")
        write(2,*) "N=",n
        
        ! Генерация массива
        do i = 1, n
                s0(i) = aint(rand()*(MAX_VALUE-MIN_VALUE)) + MIN_VALUE
        end do

        
        write(2, *) "Исходный массив::" 
        write(2, '(10i4)')  (s0(i), i=1,n)

        ! Сортировка bucket sort
        ! Цикл идет по числу разрядов (в задаче их 3)
        do i = 0, rf-1
                p = base ** i ! текущий разряд (степень 10)

                ! обнуление карманов и их длин
                do j = 0, base-1
                        counts(j) = 0
                        buckets(j,:) = 0
                end do

                ! подсчет позиций по остаткам от деления на текущий разряд
                ! и рассовывание элементов массива по карманам
                do j = 1, n 
                       k = mod((s0(j) / p), base)
                       buckets(k, counts(k)) = s0(j)
                       counts(k) = counts(k) + 1  
                end do
                
                ! обратная сборка линейного массива из карманов
                l = 1
                do j = 0, base-1
                        do k = 0, counts(j)-1
                                s0(l) = buckets(j,k)
                                l = l + 1
                        end do
                end do
        end do
        
        write(2, *) "Отсортированный массив"
        write(2, '(10i4)')  (s0(i), i=1,n)
        close(2)
end
