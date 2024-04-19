module adjacency_list_m
    implicit none

    type, private :: neighbor
        integer :: value
        type(neighbor), pointer :: next => null()
    end type neighbor

    type, private :: node
        character(:), allocatable :: data
        type(neighbor), pointer :: head => null()

    contains
        procedure :: insertNeighbord
        procedure :: printNeighbordList
    end type node

    type :: AdjacencyList
        private
        type(node), allocatable :: arr(:)

    contains
        procedure :: insert
        procedure :: createConnection
        procedure :: print
    end type AdjacencyList

contains

    !Inicializar una lista de adyacencia
    function newAdjacencyList(size) result(list)
        integer, intent(in) :: size
        type(AdjacencyList) :: list

        allocate(list%arr(0:size-1))
        list%arr(:) = node()
    end function

    subroutine insert(self, value, origin)
        class(AdjacencyList), intent(inout) :: self
        character(len=*), intent(in) :: value
        integer :: value_len
        integer, intent(in) :: origin

        if(origin >= 0 .and. origin < size(self%arr)) then
            if (.not. allocated(self%arr(origin)%data)) then
                value_len = len(value)
                allocate(character(len=value_len) :: self%arr(origin)%data)
            end if
            self%arr(origin)%data = value
            self%arr(origin)%head => null()
        end if
    end subroutine insert

    subroutine createConnection(self, origin, destiny)
        class(AdjacencyList), intent(inout) :: self
        integer, intent(in) :: origin
        integer, intent(in) :: destiny

        if(origin >= 0 .and. origin < size(self%arr)) then
            if(destiny >= 0 .and. destiny < size(self%arr)) then
                call self%arr(origin)%insertNeighbord(destiny)
            end if      
        end if
    end subroutine createConnection

    subroutine print(self)
        class(AdjacencyList), intent(inout) :: self
        integer :: i
        
        do i = 0, size(self%arr) - 1
            write(*, '(a a i0 a)', advance='no') self%arr(i)%data, ' [', i, ']'
            call self%arr(i)%printNeighbordList()
            print *, ''
        end do
    end subroutine print

    subroutine insertNeighbord(self, i)
        class(node), intent(inout) :: self
        integer, intent(in) :: i

        type(neighbor), pointer :: new
        type(neighbor), pointer :: current

        allocate(new)
        new%value = i

        if(.not. associated(self%head)) then
            self%head => new
        else
            current => self%head
            do while(associated(current%next))
                current => current%next
            end do
            current%next => new
        end if
    end subroutine insertNeighbord

    subroutine printNeighbordList(self)
        class(node), intent(inout) :: self
        type(neighbor), pointer :: aux
        aux => self%head

        do while(associated(aux))
            write(*, '(a i1 a)', advance='no') '-> [', aux%value, ']'
            aux => aux%next
        end do
    end subroutine printNeighbordList
end module adjacency_list_m