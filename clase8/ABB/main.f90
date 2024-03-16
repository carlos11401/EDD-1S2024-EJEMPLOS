program main
    use abb_m
    implicit none
    
    type(abb) :: tree
    integer :: del, i
    integer :: values(17) = [20, 8, 3, 1, 0, 15, 30, 48, 26, 10, 7, 5, 60, 19, 11, 21, 3]

    ! Insertar valores de values
    do i = 1, size(values)
        call tree%insert(values(i))
    end do 

    call tree%graph("inserted")

    del = 30
    call tree%delete(del)
    call tree%graph("deleted1")
    del = 26
    call tree%delete(del)
    call tree%graph("deleted2")
    del = 21
    call tree%delete(del)
    call tree%graph("deleted3")
    del = 11
    call tree%delete(del)
    call tree%graph("deleted4")

    write(*, '(A)') "Escribiendo en preorden: "
    call tree%preorder()

    write(*, '(A)') "Escribiendo en inorder: "
    call tree%inorder()

    print *, "Escribiendo en posorden: "
    call tree%posorder()
end program main