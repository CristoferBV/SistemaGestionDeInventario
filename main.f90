program SistemaInventario
    use, intrinsic :: ISO_FORTRAN_ENV, ONLY: ERROR_UNIT !ERROR_UNIT = Esto se utiliza para definir la unidad de error estándar en Fortran.
    implicit none                                       !módulo ISO_FORTRAN_ENV !Ambiente

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
                call ConsultarPorNombre()
            case (4)
                call ConsultarPorPrecio()
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
    character(50) :: nombre_producto
    character(500) :: file_path, temp_file_path
    integer :: num_productos, i, iostat, cantidad_producto
    logical :: encontrado
    real :: precio_producto
    character(100) :: dato_a_buscar

    ! Definir la ruta de archivo principal
    character(500) :: base_path
    base_path = "C:\\Users\\Cristofer\\OneDrive - Universidad Nacional de Costa Rica\\Escritorio\\" // &
                "Sistema de Gestion de Inventarios\\SistemaGestionDeInventario\\Inventario\\"

    ! Combinar la ruta base con los nombres de archivos
    file_path = trim(base_path) // "inventario.txt"

    ! Archivo temporal para escritura
    temp_file_path = trim(base_path) // "temp_inventario.txt"

    ! Dato a buscar en el archivo y eliminar
    dato_a_buscar = "Pescado"

    ! Abrir el archivo de inventario en modo lectura
    open(20, file=file_path, status='old')

    ! Abrir el archivo temporal en modo escritura
    open(30, file=temp_file_path, status='replace')

    call system("CLS")
    print *, "Inventario:"
    print *, "-----------------------------------------------------------------------------------"
    print *, "Nombre                                            |       Cantidad   |    Precio"
    print *, "-----------------------------------------------------------------------------------"

    ! Inicializar la bandera "encontrado"
    encontrado = .false.

    ! Leer los productos del archivo y eliminar si se encuentra el dato
    num_productos = 0
    do
        read(20, *, iostat=iostat) nombre_producto, cantidad_producto, precio_producto
        if (iostat /= 0) exit ! Fin del archivo

        ! Si se encuentra un producto con el dato buscado, marcar como encontrado
        if (trim(nombre_producto) == trim(dato_a_buscar)) then
            encontrado = .true.
        else
            ! Escribir la línea en el archivo temporal si no se debe eliminar
            write(30, '(A, I6, F8.2)') nombre_producto, cantidad_producto, precio_producto
            num_productos = num_productos + 1
        end if
    end do

    ! Cerrar los archivos
    close(20)
    close(30)

    ! Reemplazar el archivo original con el archivo temporal si se eliminó una línea
    if (encontrado) then
        call system("rename " // trim(temp_file_path) // " " // "inventario.txt")
    else
        ! Eliminar el archivo temporal si no se eliminó ninguna línea
        call system("erase " // trim(temp_file_path))
    end if

    ! Volver a abrir el archivo temporal para mostrar su contenido
    open(30, file=temp_file_path, status='old')

    ! Mostrar el contenido del archivo temporal en la consola
    do
        read(30, *, iostat=iostat) nombre_producto, cantidad_producto, precio_producto
        if (iostat /= 0) exit ! Fin del archivo
        print *, nombre_producto, "|", cantidad_producto, "     |", precio_producto
    end do

    ! Cerrar el archivo temporal
    close(30)

    call system("PAUSE")
    call system("CLS")

end subroutine VenderProducto



    subroutine ConsultarPorNombre()
        character(25) :: nombre
        character(25) :: nombre_producto
        integer :: cantidad_inventario
        real :: precio_producto
        logical :: encontrado
        integer :: iostat
        character(100) :: file_path
        file_path = '../SistemaGestionDeInventario/Inventario/inventario.txt'

        ! Abrir el archivo de inventario en modo lectura
        open(20, file=file_path, status="old")

        call System("CLS")
        ! Pedir al usuario el nombre del producto a consultar
        print *, "Ingrese el nombre del producto que desea consultar:"
        read *, nombre
        print *, ""

        ! Inicializar la bandera "encontrado"
        encontrado = .false.

        ! Mostrar encabezado de la lista de inventario
        print *, "    Inventario de Productos:"
        print *, "---------------------------------"
        print *, "Nombre     Cantidad    Precio"
        print *, "---------------------------------"
        print *, ""

        ! Leer y buscar los productos del inventario
        do
            read(20, *, iostat=iostat) nombre_producto, cantidad_inventario, precio_producto
            if (iostat /= 0) exit ! Fin del archivo

            ! Si se encuentra un producto con el nombre buscado, mostrarlo
            if (trim(nombre_producto) == trim(nombre)) then
                print *, trim(nombre_producto), cantidad_inventario, precio_producto
                encontrado = .true.
            end if
        end do

        ! Cerrar el archivo
        close(20)

        ! Mostrar mensaje si el producto no se encontró
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
        integer :: cantidad_inventario
        logical :: encontrado
        integer :: iostat
        character(100) :: file_path
        file_path = '../SistemaGestionDeInventario/Inventario/inventario.txt'

        ! Abrir el archivo de inventario en modo lectura
        open(20, file=file_path, status="old")

        call System("CLS")
        ! Pedir al usuario el precio del producto a consultar
        print *, "Ingrese el precio del producto que desea consultar:"
        read *, precio
        print *, ""

        ! Inicializar la bandera "encontrado"
        encontrado = .false.

        ! Mostrar encabezado de la lista de inventario
        print *, "    Inventario de Productos:"
        print *, "---------------------------------"
        print *, "Nombre     Cantidad    Precio"
        print *, "---------------------------------"

        ! Leer y buscar los productos del inventario
        do
            read(20, *, iostat=iostat) nombre_producto, cantidad_inventario, precio_producto
            if (iostat /= 0) exit ! Fin del archivo

            ! Si se encuentra un producto con el precio buscado, mostrarlo
            if (precio_producto == precio) then
                print *, trim(nombre_producto), cantidad_inventario, precio_producto
                encontrado = .true.
            end if
        end do

        ! Cerrar el archivo
        close(20)

        ! Mostrar mensaje si el producto no se encontró
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
        file_path = "../SistemaGestionDeInventario/Inventario/inventario.txt"

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