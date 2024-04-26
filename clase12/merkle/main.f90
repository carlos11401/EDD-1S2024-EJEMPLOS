program main
    use merkle_m
    implicit none
    
    type(merkle) :: m
    character(len=:), allocatable :: val

    val = "prueba1"
    call m%insert(val)
    val = "prueba2"
    call m%insert(val)
    val = "prueba3"
    call m%insert(val)
    val = "prueba4"
    call m%insert(val)
    val = "prueba5"
    call m%insert(val)
    val = "prueba6"
    call m%insert(val)

    call m%generate_tree()

    call m%dot()
end program main