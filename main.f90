program SistemaInventario
    use, intrinsic :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT !ERROR_UNIT = Esto se utiliza para definir la unidad de error estándar en Fortran.
    implicit none                                       !módulo ISO_FORTRAN_ENV !Ambiente

    integer :: opcion

    do
        call MostrarMenu()
        read(*, *) opcion

        select case(opcion)
            case (1)
                call RegistrarProductos()
            case (2)
                call VenderProducto()
            case (3)
                call ConsultarPorNombre()
            case (4)
                call ConsultarPorPrecio()
            case (5)
                call MostrarInventario()
            case (6)
                call ActualizarInventario()
            case (7)
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
        print *, "6. Actualizar Inventario"
        print *, "7. Salir"
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
        file_path = "C:\\Users\\Cristofer\\Documentos\\SistemaDeGestionDeInventarios\\Inventario\\inventario.txt"

        call System("CLS")
        print *, ""
        do
            print *, "Cuantos productos desea agregar?"
            read(*, *, iostat=iostat) numProductos
            if (iostat == 0) then
                exit
            else
                print *, "Cantidad invalida. Intente de nuevo."
            end if
        end do

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

            ! Validación del nombre
            do
                print *, "Nombre: "
                read(*, *, iostat=iostat) nombre
                if (iostat == 0) then
                    exit
                else
                    print *, "Nombre invalido. Intente de nuevo."
                end if
            end do

            ! Validación de la cantidad (no acepta letras ni valores negativos)
            do
                print *, "Cantidad: "
                read(*, *, iostat=iostat) cantidad
                if (iostat == 0 .and. cantidad >= 0) then
                    exit
                else
                    print *, "Cantidad invalida. Intente de nuevo."
                end if
            end do

            ! Validación del precio (no acepta letras ni valores negativos)
            do
                print *, "Precio: "
                read(*, *, iostat=iostat) precio
                if (iostat == 0 .and. precio >= 0.0) then
                    exit
                else
                    print *, "Precio invalido. Intente de nuevo."
                end if
            end do

            write(20, '(A15, I5, F10.2)') trim(nombre), cantidad, precio
        end do

        close(20)
        call System("CLS")
    end subroutine RegistrarProductos


    subroutine VenderProducto()
        implicit none
        character(50) :: nombre_producto
        character(500) :: file_path, temp_file_path
        integer :: i, iostat, cantidad_producto, venta_unitaria
        logical :: encontrado
        real :: precio_producto
        character(100) :: dato_a_buscar

        file_path = "C:\\Users\\Cristofer\\Documentos\\SistemaDeGestionDeInventarios\\Inventario\\inventario.txt"
        temp_file_path = "C:\\Users\\Cristofer\\Documentos\\SistemaDeGestionDeInventarios\\Inventario\\temp_inventario.txt"

        ! Abrir el archivo de inventario en modo lectura y escritura
        open(20, file=file_path, status='old', action='readwrite')

        call system("CLS")
        print *, "Inventario:"
        print *, "------------------------------------------------------------------------------------"
        print *, "Nombre                                            |       Cantidad   |     Precio"
        print *, "------------------------------------------------------------------------------------"

        do
            read(20, *, iostat=iostat) nombre_producto, cantidad_producto, precio_producto
            if (iostat /= 0) exit
            print *, nombre_producto, "|", cantidad_producto, "     |", precio_producto
        end do
        print *, ""
        close(20)

        print *, "Escriba el producto que desea eliminar"
        read(*, *) dato_a_buscar

        encontrado = .false.

        ! Abrir el archivo temporal para mostrar el inventario actualizado
        open(30, file=temp_file_path, status='unknown')

        ! Leer los productos del archivo original y eliminar si se encuentra el dato
        open(20, file=file_path, status='old', action='readwrite')
        do
            read(20, *, iostat=iostat) nombre_producto, cantidad_producto, precio_producto
            if (iostat /= 0) exit ! Fin del archivo original

            ! Si se encuentra un producto con el dato buscado, marcar como encontrado
            if (trim(nombre_producto) == trim(dato_a_buscar)) then
                print *, "Producto eliminado:", nombre_producto
                encontrado = .true.
            else
                ! Escribir la línea en el archivo temporal si no se debe eliminar
                write(30, '(A15, I5, F10.2)') nombre_producto, cantidad_producto, precio_producto
            end if
        end do
        close(20)

        close(30)

        ! Reemplazar el archivo original con el archivo temporal si se encontró el dato
        if (encontrado) then
            call System("copy /Y " // trim(temp_file_path) // " " // trim(file_path))
            call system('DEL "' // trim(temp_file_path) // '"')
            call system('REN "' // trim(temp_file_path) // '" "' // trim(file_path) // '"')

        end if

        ! Mostrar los datos actualizados
        if (encontrado) then
            call system("CLS")
            print *, "Producto eliminado con exito!!"
            call system("PAUSE")
            call system("CLS")
        else
            call system("CLS")
            print *, "Producto no encontrado en el inventario."
        end if

    end subroutine VenderProducto


    subroutine ConsultarPorNombre()
        character(25) :: nombre
        character(25) :: nombre_producto
        integer :: cantidad_inventario
        real :: precio_producto
        logical :: encontrado
        integer :: iostat
        character(100) :: file_path
        file_path = "C:\\Users\\Cristofer\\Documentos\\SistemaDeGestionDeInventarios\\Inventario\\inventario.txt"

        ! Abrir el archivo de inventario en modo lectura
        open(20, file=file_path, status="old")

        call System("CLS")
        print *, "Ingrese el nombre del producto que desea consultar:"
        read *, nombre
        print *, ""

        encontrado = .false.

        print *, "    Inventario de Productos:"
        print *, "---------------------------------"
        print *, "Nombre     Cantidad    Precio"
        print *, "---------------------------------"
        print *, ""

        ! Leer y buscar los productos del inventario
        do
            read(20, *, iostat=iostat) nombre_producto, cantidad_inventario, precio_producto
            if (iostat /= 0) exit 

            ! Si se encuentra un producto con el nombre buscado, mostrarlo
            if (trim(nombre_producto) == trim(nombre)) then
                print *, trim(nombre_producto), cantidad_inventario, precio_producto
                encontrado = .true.
            end if
        end do

        close(20)

        if (.not. encontrado) then
            print *, "Producto no encontrado en el inventario."
        end if

        print *, ""
        call system("PAUSE")
        call system("CLS")
    end subroutine ConsultarPorNombre


    subroutine ConsultarPorPrecio()
        real :: precio, precio_producto
        character(25) :: nombre_producto
        integer :: cantidad_inventario, iostat
        logical :: encontrado
        integer :: iostat_price
        character(100) :: file_path
        file_path = "C:\\Users\\Cristofer\\Documentos\\SistemaDeGestionDeInventarios\\Inventario\\inventario.txt"

        ! Abrir el archivo de inventario en modo lectura
        open(20, file=file_path, status="old")

        call System("CLS")

        do
            print *, "Ingrese el precio del producto que desea consultar:"
            read(*, *, iostat=iostat_price) precio
            if (iostat_price == 0.and. precio >= 0.0) then
                exit
            else
                print *, "Precio invalido. Intente de nuevo."
            end if
        end do

        encontrado = .false.

        print *, "    Inventario de Productos:"
        print *, "---------------------------------"
        print *, "Nombre     Cantidad    Precio"
        print *, "---------------------------------"

        ! Leer y buscar los productos del inventario
        do
            read(20, *, iostat=iostat) nombre_producto, cantidad_inventario, precio_producto
            if (iostat /= 0) exit

            ! Si se encuentra un producto con el precio buscado, mostrarlo
            if (precio_producto == precio) then
                print *, trim(nombre_producto), cantidad_inventario, precio_producto
                encontrado = .true.
            end if
        end do

        close(20)

        if (.not. encontrado) then
            print *, "Producto no encontrado en el inventario."
        end if

        call system("PAUSE")
        call system("CLS")
    end subroutine ConsultarPorPrecio


    subroutine MostrarInventario()
        character(25) :: nombre_producto, opc
        integer :: cantidad_inventario, num_productos, iostat
        real :: precio_producto
        character(100) :: file_path
        logical :: archivo_abierto
        file_path = "C:\\Users\\Cristofer\\Documentos\\SistemaDeGestionDeInventarios\\Inventario\\inventario.txt"

        do
            archivo_abierto = .false.
            if (.not. archivo_abierto) then
                open(20, file=file_path, status="old", iostat=iostat)

                ! Verificar si hubo errores al abrir el archivo
                if (iostat /= 0) then
                    write(*,*) "Error al abrir el archivo de inventario."
                    stop
                end if
                archivo_abierto = .true.
            end if

            call System("CLS")
            print *, "Inventario:"
            print *, "-----------------------------------------------------------"
            print *, "Nombre                   |       Cantidad   |     Precio"
            print *, "-----------------------------------------------------------"

            do
                read(20, *, iostat=iostat) nombre_producto, cantidad_inventario, precio_producto
                if (iostat /= 0) exit
                print *, nombre_producto, "|", cantidad_inventario, "     |", precio_producto
            end do

            print *, ""
            print *, "Desea salir del inventario s/n"
            read *, opc

            if (opc /= 's' ) then
                archivo_abierto = .true.
                close(20)
            else
                archivo_abierto = .true.
                close(20)
                call system("CLS")
                exit
            end if
        end do
    end subroutine MostrarInventario

    subroutine ActualizarInventario()
        character(25) :: nombre
        character(25) :: nombre_producto
        integer :: cantidad_inventario, iostat
        real :: precio_producto
        logical :: encontrado
        character(100) :: file_path
        character(100) :: temp_file_path
        integer :: iunit
        integer :: opcion

        file_path = "C:\\Users\\Cristofer\\Documentos\\SistemaDeGestionDeInventarios\\Inventario\\inventario.txt"
        temp_file_path = 'C:\\Users\\Cristofer\\Documentos\\SistemaDeGestionDeInventarios\\Inventario\\temp_inventario.txt'

        open(20, file=file_path, status="old")

        call System("CLS")
        print *, "    Inventario de Productos:"
        print *, "---------------------------------"
        print *, "Nombre     Cantidad    Precio"
        print *, "---------------------------------"

        ! Leer y mostrar los productos del inventario
        do
            read(20, *, iostat=iostat) nombre_producto, cantidad_inventario, precio_producto
            if (iostat /= 0) exit
            print *, trim(nombre_producto), cantidad_inventario, precio_producto
        end do

        close(20)

        print *, ""
        print *, "Seleccione una opcion:"
        print *, "1. Editar producto"
        print *, "2. Volver al menu principal"
        read(*, *) opcion
        call System("CLS")

        if (opcion == 1) then
            call System("CLS")
            do
                print *, "Ingrese el nombre del producto que desea editar:"
                read(*, *, iostat=iostat) nombre
                if (iostat == 0) then
                    exit
                else
                    print *, "Nombre invalido. Intente de nuevo."
                end if
            end do
            print *, ""

            encontrado = .false.

            open(20, file=file_path, status="old", action="readwrite")

            ! Abrir un archivo temporal para escribir los datos editados
            open(iunit, file=temp_file_path, status="replace")

            ! Leer y editar los productos del inventario
            do
                read(20, *, iostat=iostat) nombre_producto, cantidad_inventario, precio_producto
                if (iostat /= 0) exit 

                if (trim(nombre_producto) == trim(nombre)) then
                    print *, "Editando producto:", nombre_producto

                    do
                        print *, "Ingrese el nuevo nombre:"
                        read(*, *, iostat=iostat) nombre_producto
                        if (iostat == 0) then
                            exit
                        else
                            print *, "Nombre invalido. Intente de nuevo."
                        end if
                    end do

                    ! Validación de la cantidad (no acepta letras ni valores negativos)
                    do
                        print *, "Ingrese la nueva cantidad:"
                        read(*, *, iostat=iostat) cantidad_inventario
                        if (iostat == 0 .and. cantidad_inventario >= 0) then
                            exit
                        else
                            print *, "Cantidad invalida. Intente de nuevo."
                        end if
                    end do

                    ! Validación del precio (no acepta letras ni valores negativos)
                    do
                        print *, "Ingrese el nuevo precio:"
                        read(*, *, iostat=iostat) precio_producto
                        if (iostat == 0 .and. precio_producto >= 0.0) then
                            exit
                        else
                            print *, "Precio invalido. Intente de nuevo."
                        end if
                    end do

                    encontrado = .true.
                end if

                ! Escribir en el archivo temporal
                write(iunit, *) trim(nombre_producto), cantidad_inventario, precio_producto
            end do

            close(20)
            close(iunit)

            ! Reemplazar el archivo original con el archivo temporal
            call System("copy /Y " // trim(temp_file_path) // " " // trim(file_path))

            if (.not. encontrado) then
                print *, "Producto no encontrado en el inventario."
            else
                print *, "Producto editado exitosamente."
            end if

            call system("PAUSE")
            call System("CLS")
        end if
    end subroutine ActualizarInventario


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