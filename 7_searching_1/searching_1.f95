program searching_1
        integer :: seed, i, N, found, key
        parameter(N=100)
        integer :: arr(N)
        integer :: ixs(N)
        real :: r
        
        found = 0
        seed = 42
        call random_seed(seed)
        
        write(*,*) "Введите ключ [0..10]:"
        read(*,*) key
        
        ! filling
        do i = 1, N
                call random_number(r)
                arr(i) = aint(r*10)
        end do

        ! searching
        do i = 1, N
                if (arr(i) == key) then
                        ixs(found+1) = i
                        found = found + 1
                endif
        end do
        

        open(2, file="searching_1.txt", status="unknown")
        write(2, *) "Размер массива:", N, " ключ:", key
        write(2,'(10i2)')  (arr(i),i=1,N)
        if (found == 0) then
                write(2,*) "Вхождений ключа не найдено"
        else
                write(2,*) "Индексы ключевых элементов:"
                write(2,'(10i5)')  (ixs(i),i=1,found)

        endif
        close(2)
end
