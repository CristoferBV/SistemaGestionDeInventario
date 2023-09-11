program PruebaEscritura
    implicit none

    ! Declarar la ruta del archivo con barras diagonales simples
    character(256) :: ruta_archivo
    ruta_archivo = '../SistemaGestionDeInventario/Inventario/inventario.txt'

    open(11, file=ruta_archivo)
    write(11,*) 'Producto 6', 147, 14.0
    close(11)

end program PruebaEscritura
