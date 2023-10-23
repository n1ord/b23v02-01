program linear_2
    real x
    print*, 'Write x (real):'
    read(*,*) x

    open(2, file="linear_2.txt", status="unknown")
    write(2,*) 'y=', (2*x**4 - 3*x**3 + 4*x**2 - 5*x + 6)
    write(2,*) 'y=', (1 - 2*x + 3*x**2 - 4*x**3)
    write(2,*) 'y=', (1 + 2*x + 3*x**2 + 4*x**3)
    close(2)
end