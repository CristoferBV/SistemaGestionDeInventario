program SistemaInventario
    use, intrinsic :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT !ERROR_UNIT = Esto se utiliza para definir la unidad de error estándar en Fortran.
    implicit none                                       !módulo ISO_FORTRAN_ENV

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
                call VenderProducto()
            case (3)
                call ConsultarPorNombre(nombre)
            case (4)
                call ConsultarPorPrecio(precio)
            case (5)
                call MostrarInventario()
            case (6)
                exit
            case default
                call ManejarError(1)
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
        print *, "5. Mostrar inventario"
        print *, "6. Salir"
        print *, ""
        print *, "Seleccione una opcion: "
    end subroutine MostrarMenu

    subroutine RegistrarProductos()
        character(25) :: nombre
        integer :: cantidad, numProductos
        real :: precio
        integer :: i
        integer :: iostat
        character(100) :: file_path
        file_path = "../SistemaGestionDeInventario/Inventario/inventario.txt"

        call System("CLS")
        print *, ""
        print *, "Cuantos productos desea agregar?"
        read(*, *) numProductos

        ! Abrir el archivo en modo append (adición) si existe, de lo contrario, crearlo
        open(20, file=file_path, status="old", action="readwrite", position="append", iostat=iostat)

        ! Verificar si hubo errores al abrir el archivo
        if (iostat /= 0) then
            write(*,*) "Error al abrir el archivo."
            stop
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

            write(20, '(A15, I5, F10.2)') trim(nombre), cantidad, precio
        end do

        close(20)
        call System("CLS")
    end subroutine RegistrarProductos

    subroutine VenderProducto()
        implicit none
        character(50) :: producto_eliminar
        integer :: num_productos, i
        logical :: encontrado
        character(50) :: nombre_producto
        real :: precio_producto
        integer :: cantidad_producto
        character(100) :: file_path
        file_path = "../SistemaGestionDeInventario/Inventario/inventario.txt"

        ! Abrir el archivo de inventario
        open(20, file=file_path, status='old')

        ! Inicializar la bandera encontrado
        encontrado = .false.

        call system("CLS")
        ! Mostrar el encabezado del inventario
        print *, "Inventario:"
        print *, "-----------------------------------------------------------------------------------"
        print *, "Nombre                                            |       Cantidad   |    Precio"
        print *, "-----------------------------------------------------------------------------------"

        ! Leer los productos del archivo
        num_productos = 0
        do
            read(20, *, end=10) nombre_producto, cantidad_producto, precio_producto
            print *, nombre_producto, "|", cantidad_producto, "     |", precio_producto
            num_productos = num_productos + 1
        end do

        10  continue

        ! Pedir al usuario que elija un producto para eliminar
        print *, ""
        print *, "Ingrese el nombre del producto que desea vender:"
        read *, producto_eliminar

        ! Buscar el producto en el inventario
        rewind(20)
        do i = 1, num_productos
            read(20, *) nombre_producto, cantidad_producto, precio_producto
            if (trim(nombre_producto) == trim(producto_eliminar)) then
                encontrado = .true.
                exit
            end if
        end do

        ! Cerrar el archivo de inventario
        close(20)

        ! Si se encontró el producto, eliminarlo y actualizar el archivo
        if (encontrado) then
            open(20, file=file_path, status='replace')
            rewind(20)
            do i = 1, num_productos
                read(20, *) nombre_producto, cantidad_producto, precio_producto
                if (trim(nombre_producto) /= trim(producto_eliminar)) then
                    write(20, *) nombre_producto, cantidad_producto, precio_producto
                end if
            end do
            close(20)
            print *, "Producto vendido con éxito."
        else
            print *, "Producto no encontrado en el inventario."
        end if

    end subroutine VenderProducto


    subroutine ConsultarPorNombre(nombre)
        character(25) :: nombre
        ! Implementar la lógica para consultar por nombre
    end subroutine ConsultarPorNombre

    subroutine ConsultarPorPrecio(precio)
        real :: precio
        ! Implementar la lógica para consultar por precio
    end subroutine ConsultarPorPrecio

    subroutine MostrarInventario()
        character(25) :: nombre_producto
        integer :: cantidad_inventario, iostat
        real :: precio_producto
        character(100) :: file_path
        file_path = "../SistemaGestionDeInventario/Inventario/inventario.txt"

        ! Abrir el archivo de inventario en modo lectura
        open(20, file=file_path, status="old")

        ! Verificar si hubo errores al abrir el archivo
        if (iostat /= 0) then
            write(*,*) "Error al abrir el archivo de inventario."
            stop
        end if

        call System("CLS")
        ! Mostrar encabezado de la lista de inventario
        print *, "    Inventario de Productos:"
        print *, "---------------------------------"
        print *, "Nombre     Cantidad    Precio"
        print *, "---------------------------------"

        ! Leer y mostrar los productos del inventario
        do
            read(20, *, iostat=iostat) nombre_producto, cantidad_inventario, precio_producto
            if (iostat /= 0) exit ! Fin del archivo
            print *, trim(nombre_producto), cantidad_inventario, precio_producto
        end do

        ! Cerrar el archivo
        close(20)
        call system("PAUSE")
        call system("CLS")
    end subroutine MostrarInventario

    subroutine ManejarError(codigo)
        integer :: codigo
        select case(codigo)
            case (1)
                print *, ""
                call system("CLS")
                print *, "[ERROR]: Por favor, seleccione una opcion valida."
                print *, ""
                call system("PAUSE")
                call system("CLS")
        end select
    end subroutine ManejarError

end program SistemaInventario