program SistemaInventario

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
            case (2)
                call VenderProducto(nombre, cantidad_vendida)
            case (3)
                call ConsultarPorNombre(nombre)
            case (4)
                call ConsultarPorPrecio()
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

    subroutine RegistrarProducto(nombre, cantidad, precio)
        character(50), intent(in) :: nombre
        integer, intent(in) :: cantidad
        real, intent(in) :: precio
        ! Implementar la lógica para registrar un producto en el inventario
    end subroutine RegistrarProducto

    subroutine VenderProducto(nombre, cantidad_vendida)
        character(50), intent(in) :: nombre
        integer, intent(in) :: cantidad_vendida
        ! Implementar la lógica para vender un producto
    end subroutine VenderProducto

    subroutine ConsultarPorNombre(nombre)
        character(50), intent(in) :: nombre
        ! Implementar la lógica para consultar por nombre
    end subroutine ConsultarPorNombre

    subroutine ConsultarPorPrecio()
        ! Implementar la lógica para consultar por precio
    end subroutine ConsultarPorPrecio

    subroutine ManejarError(codigo)
        integer, intent(in) :: codigo
        SELECT CASE(codigo)
            CASE (1)
                PRINT *, "Opcion invalida. Por favor, seleccione una opción valida."
            ! Agregar más casos para manejar otros errores
        END SELECT
    end subroutine ManejarError

end program SistemaInventario
