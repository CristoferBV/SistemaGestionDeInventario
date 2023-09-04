program SistemaInventario
    use, intrinsic :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT
    implicit none

    integer :: opcion
    character(25) :: nombre
    integer :: cantidad, cantidad_vendida
    real :: precio

    do
        call MostrarMenu()
        read(*, *) opcion

        select case(opcion)
            case (1)
                call RegistrarProductos()
            case (2)
                call VenderProducto(nombre, cantidad_vendida)
            case (3)
                call ConsultarPorNombre(nombre)
            case (4)
                call ConsultarPorPrecio(precio)
            case (5)
                EXIT
            case default
                call ManejarError(1) ! Opción inválida
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

    subroutine RegistrarProductos()
        character(25) :: nombre
        integer :: cantidad, numProductos
        real :: precio
        integer :: i
        integer :: ios

        print *, ""
        print *, "Cuantos productos desea agregar?"
        read(*, *) numProductos

        ! Abrir el archivo en modo append (adición) si existe, de lo contrario, crearlo
        open(20, file="../SistemaGestionDeInventario/Inventario/inventario.txt", status="old", action="readwrite", iostat=ios)

        if (ios == 0) then
            ! Si el archivo existe, mover el puntero al final
            do i = 1, numProductos
                read(20, *, iostat=ios)
                if (ios /= 0) then
                    exit
                end if
            end do
        else
            ! Si el archivo no existe, cambiar a modo de escritura desconocido para crearlo
            close(20)
            open(20, file="../SistemaGestionDeInventario/Inventario/inventario.txt", status="unknown", action="write", iostat=ios)
        end if

        do i = 1, numProductos
            print *, ""
            print *, "---Registre el producto---  ", i
            print *, ""

            print *, "Nombre: "
            read(*, *) nombre
            print *, ""

            print *, "Cantidad: "
            read(*, *) cantidad
            print *, ""

            print *, "Precio: "
            read(*, *) precio
            print *, ""

            ! Usar un formato fijo para escribir en el archivo
            write(20, '(A15, I5, F10.2)') trim(nombre), cantidad, precio
        end do

        close(20)
    end subroutine RegistrarProductos

    subroutine VenderProducto(nombre, cantidad_vendida)
        character(25) :: nombre
        integer :: cantidad_vendida
        ! Implementar la lógica para vender un producto
    end subroutine VenderProducto

    subroutine ConsultarPorNombre(nombre)
        character(25) :: nombre
        ! Implementar la lógica para consultar por nombre
    end subroutine ConsultarPorNombre

    subroutine ConsultarPorPrecio(precio)
        real :: precio
        ! Implementar la lógica para consultar por precio
    end subroutine ConsultarPorPrecio

    subroutine ManejarError(codigo)
        integer :: codigo
        select case(codigo)
            case (1)
                print *, "Opcion invalida. Por favor, seleccione una opción valida."
            ! Agregar más casos para manejar otros errores
        end select
    end subroutine ManejarError

end program SistemaInventario
