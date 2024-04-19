module adjacency_list2_m
    implicit none

    type, private :: neighbor
        integer :: value
        type(neighbor), pointer :: next => null()
    end type neighbor

    type, private :: node
        integer :: value
        type(node), pointer :: next => null()
        type(neighbor), pointer :: head => null()
    contains
        procedure :: insertNeighbor
        procedure :: graphNeighbors
    end type node

    type :: AdjacencyList2
        private
        type(node), pointer :: head => null()
    contains
        procedure :: insert
        procedure :: createConnection
        procedure :: graph
    end type AdjacencyList2

contains
    subroutine insert(self, value)
        class(AdjacencyList2), intent(inout) :: self
        integer, intent(in) :: value

        type(node), pointer :: aux
        type(node), pointer :: new

        allocate(new)
        new%value = value
        ! If the list is empty
        if(.not. associated(self%head)) then
            self%head => new
        else
            ! If the value is less than the head
            aux => self%head
            if(value < self%head%value) then
                new%next => self%head
                self%head => new
            else
                ! If the value is greater than the head
                aux => self%head
                do while(associated(aux%next))
                    if(value < aux%next%value) then
                        new%next => aux%next
                        aux%next => new
                        exit
                    end if
                    aux => aux%next
                end do
                ! If the value is greater than all the nodes
                if(.not. associated(aux%next)) then
                    aux%next => new
                end if
            end if
        end if
    end subroutine insert

    subroutine createConnection(self, origin, destiny)
        class(AdjacencyList2), intent(inout) :: self
        integer, intent(in) :: origin
        integer, intent(in) :: destiny

        type(node), pointer :: current
        current => self%head

        do while(associated(current))
            if(current%value == origin) then
                call current%insertNeighbor(destiny)
                exit
            end if
            current => current%next
        end do
    end subroutine createConnection

    subroutine graph(self, filename)
        class(AdjacencyList2), intent(in) :: self

        character(len=*) :: filename
        type(node), pointer :: current
        character(len=150) :: node_dec
        character(len=100) :: command
        character(len=20) :: name
        character(len=10) :: str_aux
        integer :: io
        integer :: i
    
        current => self%head
        io = 1
        
        ! Create the .dot file
        open(newunit=io, file="./"//filename//".dot")

        write(io, *) "digraph g {"
        write(io, *) "rankdir=LR;"
        do while(associated(current))
            write(str_aux, '(I10)') current%value
            name = '"Nodo'//trim(adjustl(str_aux))//'"'
            node_dec = trim(adjustl(name))//'[label = "'//trim(adjustl(str_aux))//'"]'
            write(io, *) node_dec
            call current%graphNeighbors(io)
            current => current%next
        end do
        write(io, *) "}"
        close(io)
        
        ! Create the image
        command = "dot -Tpng ./"//filename//".dot -o ./"//filename//".png"    
        call execute_command_line(command, exitstat=i)

        if ( i == 1 ) then
            print *, "Ocurrió un error al momento de crear la imagen"
        else
            print *, "La imagen fue generada exitosamente"
        end if
    end subroutine graph

    subroutine insertNeighbor(self, destiny)
        class(node), intent(inout) :: self
        integer, intent(in) :: destiny

        type(neighbor), pointer :: new
        type(neighbor), pointer :: current

        allocate(new)
        new%value = destiny
        ! If the list is empty
        if(.not. associated(self%head)) then
            self%head => new
        else
            ! Search the destiny node
            current => self%head
            do while(associated(current%next))
                ! If the destiny node already exists
                if(current%value == destiny) then
                    return
                end if
                current => current%next
            end do
            ! If the destiny node does not exist
            current%next => new
        end if
    end subroutine insertNeighbor

    subroutine graphNeighbors(self, io)
        class(node), intent(inout) :: self
        integer, intent(in) :: io

        type(neighbor), pointer :: aux
        character(len=20) :: origin_name
        character(len=20) :: destiny_name
        character(len=10) :: str_aux
        aux => self%head

        write(str_aux, '(I10)') self%value
        origin_name = '"Nodo'//trim(adjustl(str_aux))//'"'

        do while(associated(aux))
            if(aux%value < self%value) then
                write(str_aux, '(I10)') aux%value
                destiny_name = '"Nodo'//trim(adjustl(str_aux))//'"'

                write(io, *) origin_name//'->'//destiny_name// '[dir = both]'
            end if
            aux => aux%next
        end do
    end subroutine graphNeighbors
end module adjacency_list2_m