module hash_table_m
    implicit none
    private
    integer :: table_size = 7
    real, parameter :: R = 0.618034
    integer, parameter :: MAX_USED_PERCENTAGE = 75

    type, public :: HashTable
        integer :: elements = 0
        integer, allocatable :: array(:)

        contains
        procedure :: insert
        procedure :: print
        procedure :: search
        procedure, private :: solve_collision
    end type HashTable
contains
    subroutine insert(self, key)
        class(HashTable), intent(inout) :: self
        type(HashTable) :: newTable
        integer , intent(in) :: key
        integer, allocatable :: oldArray(:)
        real :: used_percentage
        integer :: pos
        
        ! If the table is empty, allocate it
        if(.not. allocated(self%array)) then
            allocate(self%array(0:table_size-1))
            self%array(:) = -1  ! Initialize all the elements to -1
        end if

        pos = get_position(key)

        ! If the position is already occupied, solve the collision
        if(self%array(pos) /= -1 .and. self%array(pos) /= key) then
            call self%solve_collision(pos)
        end if

        ! Store the key in the table
        self%array(pos)=key
        self%elements = self%elements + 1

        ! Check if the table is more than 75% full
        used_percentage = (self%elements * 1.0/table_size) * 100
        if(used_percentage > MAX_USED_PERCENTAGE) then
            ! Deallocate the table
            oldArray = self%array
            deallocate(self%array)
            ! Rehash the table
            newTable = rehashing(oldArray)
            self%array = newTable%array
            self%elements = newTable%elements
        end if
    end subroutine insert

    function rehashing(oldArray) result(newTable)
        integer, intent(in) :: oldArray(:)
        integer :: i
        type(HashTable) :: newTable
        
        ! Initialize the new table
        table_size = table_size*2
        allocate(newTable%array(0:table_size-1))
        newTable%array(:) = -1
        ! Insert the elements in the new table
        do i = 1, size(oldArray)
            if(oldArray(i) /= -1) then
                call newTable%insert(oldArray(i))
            end if
        end do
    end function rehashing

    subroutine solve_collision(self, pos)
        class(HashTable), intent(inout) :: self
        integer, intent(inout) :: pos
        ! Hash function h'(k)
        do while(self%array(pos) /= -1)
            pos = pos + 1
            pos = mod(pos, table_size)
        end do
    end subroutine solve_collision

    function get_position(key) result(pos)
        integer, intent(in) :: key
        real :: t
        integer :: pos
        ! Hash function h(k)
        ! Multiplicative hashing
        t = R*key - floor(R*key)
        pos = floor(table_size*t)
    end function get_position

    subroutine search(self, key)
        class(HashTable), intent(inout) :: self
        integer, intent(in) :: key
        integer :: pos

        pos = get_position(key)
        ! If the key is not in the table
        !
        !
        print '(a i0 a i0)' , "Position: ", pos, " Key: ", self%array(pos)
    end subroutine search

    subroutine print(self)
        class(HashTable), intent(inout) :: self
        print '(a, i0)', "Size on table: ", table_size
        print '(a, i0)', "Elements on table: ", self%elements
        print '(i10)', self%array
    end subroutine print
end module hash_table_m