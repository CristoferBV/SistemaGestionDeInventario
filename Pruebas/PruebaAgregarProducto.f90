program AgregarProducto
use, intrinsic :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
    implicit none

    integer :: opcion
    character(50) :: nombre
    integer :: cantidad, cantidad_vendida
    real :: precio

    do
        call MostrarMenu()
        read(*, *) opcion

        select case(opcion)
            case (1)
                call RegistrarProducto(nombre, cantidad, precio)
            case (5)
                EXIT
        end select
    end do

contains

    subroutine MostrarMenu()
        print *, ""
        print *, "Bienvenido al Sistema de Gestion de Inventarios"
        print *, ""
        print *, "1. Registrar producto"
        print *, "2. Vender producto"
        print *, "3. Consultar por nombre"
        print *, "4. Consultar por precio"
        print *, "5. Salir"
        print *, ""
        print *, "Seleccione una opcion: "

end subroutine MostrarMenu

    subroutine RegistrarProductos(nombre, cantidad, precio)
    character(50) :: nombre
    integer :: cantidad, num_productos
    real :: precio
    integer :: i
    integer :: ios

    ! Preguntar al usuario cuántos productos quiere agregar
    print *, "¿Cuántos productos desea agregar?"
    read(*, *) num_productos

    ! Abrir el archivo en modo update
    open(20, file="../SistemaGestionDeInventario/Inventario/inventario.txt", status="unknown", &
         action="write", iostat=ios)

    ! Verificar si se pudo abrir el archivo correctamente
    if (ios /= 0) then
        print *, "Error al abrir el archivo."
        return
    end if

    ! Mover el puntero de archivo al final del archivo
    do i = 1, num_productos
        read(20, *, iostat=ios)
        if (ios /= 0) then
            exit
        end if
    end do

    ! Ciclo para registrar cada producto
    do i = 1, num_productos
        print *, ""
        print *, "---Registre el producto ", i, "---"
        print *, ""
        
        print *, "Nombre: "
        read(*, *) nombre

        print *, "Cantidad: "
        read(*, *) cantidad

        print *, "Precio: "
        read(*, *) precio

        ! Escribir el producto al final del archivo
        write(20, *) nombre, cantidad, precio
    end do

    ! Cerrar el archivo
    close(20)
end subroutine RegistrarProductos



end program AgregarProducto
