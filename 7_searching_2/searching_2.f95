program searching_2
        ! Двоичный поиск

        integer :: seed, i, N, found, key, MAX_VALUE
        parameter(N=100, MAX_VALUE=10)
        integer :: arr(N)
        integer :: ixs(N)
        integer :: n1, n2, m
        real :: r
        
        found = 0 ! число найденных индексов
        m = -1 ! останется -1, если ключей не найдется
        n1 = 1 ! границы двоичного поиска
        n2 = N

        write(*,*) "Введите ключ [0..10]:"
        read(*,*) key
        
        ! заполнение массива значениями по-возрастанию
        do i = 1, N
                arr(i) = (i-1) * MAX_VALUE / N
        end do

        ! двоичный поиск со сдвигом границ [n1..n2]
        do while ((n2-n1) > 1)
                m = (n2+n1)/2
                if (arr(m) < key) then
                        n1 = m
                else if (key < arr(m)) then
                        n2 = m
                else if (arr(m) == key) then
                        exit
                endif        
        end do

        ! искомое множество элементов точно содержится в границах
        ! [n1..n2] после успешного двоичного поиска
        if (m /= -1)  then
                do i = n1, n2
                        if (arr(i) == key) then
                                found = found + 1
                                ixs(found) = i
                        endif
                end do
        endif


        open(2, file="searching_2.txt", status="unknown")
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
