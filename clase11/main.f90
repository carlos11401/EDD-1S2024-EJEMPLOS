program main
    use adjacency_list2_m
    implicit none
    
    ! type(AdjacencyList) :: mygraph
    ! mygraph = newAdjacencyList(7)
    ! call mygraph%insert("36", 0)
    ! call mygraph%insert("25", 1)
    ! call mygraph%insert("10", 2)
    ! call mygraph%insert("100", 3)
    ! call mygraph%insert("150", 4)
    ! call mygraph%insert("1", 5)
    ! call mygraph%insert("6", 6)

    ! call mygraph%createConnection(0,1)
    ! call mygraph%createConnection(1,0)

    ! call mygraph%createConnection(1,2)
    ! call mygraph%createConnection(2,1)

    ! call mygraph%createConnection(2,3)
    ! call mygraph%createConnection(3,2)

    ! call mygraph%createConnection(2,4)
    ! call mygraph%createConnection(4,2)

    ! call mygraph%createConnection(1,3)
    ! call mygraph%createConnection(3,1)

    ! call mygraph%createConnection(4,0)
    ! call mygraph%createConnection(0,4)

    ! call mygraph%createConnection(5,6)
    ! call mygraph%createConnection(6,5)

    ! call mygraph%createConnection(5,0)
    ! call mygraph%createConnection(0,5)

    ! call mygraph%print()


    type(AdjacencyList2) :: mygraph
    call mygraph%insert(36)
    call mygraph%insert(25)
    call mygraph%insert(10)
    call mygraph%insert(100)
    call mygraph%insert(150)
    call mygraph%insert(1)
    call mygraph%insert(6)

    call mygraph%createConnection(36,25)
    call mygraph%createConnection(25,36)

    call mygraph%createConnection(25,10)
    call mygraph%createConnection(10,25)

    call mygraph%createConnection(10,100)
    call mygraph%createConnection(100,10)

    call mygraph%createConnection(10,150)
    call mygraph%createConnection(150,10)

    call mygraph%createConnection(25,100)
    call mygraph%createConnection(100,25)

    call mygraph%createConnection(150,36)
    call mygraph%createConnection(36,150)

    call mygraph%createConnection(6,1)
    call mygraph%createConnection(1,6)
    
    call mygraph%createConnection(1,36)
    call mygraph%createConnection(36,1)

    call mygraph%graph("graph2")
end program main