program case_1
        ! Определите время года по номеру месяца. Структура оператор case должна содержать 
        ! четыре позиции по временам года – зима, весна, лето, осень – соответствующие им 
        ! константы (метки) означают номер месяца в порядке следования.

        implicit none
        integer :: month

        print*, 'Input month number [1..12]:'
        read(*,*) month

        open(2, file="case_1.txt", status="unknown")
        write(2,*) 'Your month number:', month
        select case (month)
        case (12,1,2)
                write(2,*) 'Season: Winter'
        case (3,4,5)
                write(2,*) 'Season: Spring'
        case (6,7,8)
                write(2,*) 'Season: Summer'
        case (9,10,11)
                write(2,*) 'Season: Autumn'
        case default
                write(2,*) 'error: month number not in range [1..12]'
        end select
        close(2)
end
