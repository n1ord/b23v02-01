program case_3
        ! Задайте целое число в пределах n≤100 (возраст человека). Сформируйте правильную фразу, используя фрагменты определения - «…год» «…года», «… лет», и выведите её на экран.
        implicit none

        integer :: age, last_digit
        character(10) :: age_form 

        last_digit = 0

        print*, 'Введите возраст [1..100]:'
        read(*,*) age

        ! определяем словоформу по возрасту
        if (age < 10) then
                last_digit = age
        elseif (age < 20) then
                last_digit = 0 ! 11,12,13,... -> лет
        elseif (age < 100) then
                last_digit = age - (age/10)*10
        endif
        
        ! выводим
        open(2, file="case_3.txt", status="unknown")
        write(2, *) "Введенный возраст:", age
        select case(last_digit)
        case(1)
                age_form = 'год'
        case(2,3,4)
                age_form = 'года'
        case default
                age_form = 'лет'
        end select

        write(2,*) 'Возраст: ', age, ' ', age_form
        close(2)
end



