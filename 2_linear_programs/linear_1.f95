program linear_1
    implicit none

    integer :: a,b
    real ::  c,d

    open(2, file="linear_1.txt", status="unknown")
    write(*,*) 'Input integer a, b, and real c, d:'
    read(*,*) a,b,c,d
    write(2,*) "Your input: a, b, c, d", a,b,c,d
    write(2,'(a8,i3,a8,i3,a7,f5.2,a8,f5.2)') 'a + 1 = ',(a+1),' b - 2 = ',(b-2), ' c*2 = ', (c*2), ' d/2 = ', (d/2)
    close(2)
end