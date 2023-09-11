program PruebaLectura
    implicit none

    character(25) :: nombre
    integer :: cantidad
    real :: precio

    ! Declarar la ruta del archivo con barras diagonales simples
    character(256) :: ruta_archivo
    ruta_archivo = '../SistemaGestionDeInventario/Inventario/inventario.txt'

    open(20, file=ruta_archivo,status="old")
    read(20,*) nombre,cantidad,precio
    close(20)

    print *, nombre, cantidad, precio

end program PruebaLectura
