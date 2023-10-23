program linear
    ! Задайте значения параметров переменными типа real. 
    ! Вычислите следующие выражения. 
    ! Для тестирования задайте значения a, b, c, d  в диапазоне [1  4]
    ! в алгебраических выражениях и a=Pi/4, Pi/2, Pi в тригонометрических выражениях.
    implicit none

    real :: a,b,c,d
    real :: a2,b2,c2,d2
    real :: pi = 3.14159265358979323

    write(*,*)  'Input a,b,c,d in range of [1..4], then input a,b,c,d in range of [0..Pi]'
    read(*,*) a,b,c,d,a2,b2,c2,d2

    open(2, file="linear_3.txt", status="unknown")
    
    write(2, *) ( ((a + b)*(b + c) + d)*(a - b) )
    write(2, *) ( ((a/b + 1)*(b/a - 1) - 1)/(a+b) )
    write(2, *) ( cos(a2) + sin(a2) + cos(3*a2) + sin(3*a2) )
    write(2, *) ( (sin(2*a2) + sin(5*a2) - sin(3*a2))/( cos(a2) + 1 - 2*sin(2*a2)**2 ) )
    write(2, *) ( sin(a2)**2 + cos(a2)**2 )
    write(2, *) ( atan( pi*3/4 + a2*3/2 ) )
    write(2, *) ( (1 - tan(a2))/(1 + tan(a2)) )
    write(2, *) ( (sqrt(a) + sqrt(b))/a )
    write(2, *) ( 1 + (b**2 + c**2 - a**2)/(2*b*c) )
    write(2, *) ( (a**2-1)/(sqrt(a) - sqrt(b)) )
    close(2)
end
