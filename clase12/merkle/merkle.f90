module merkle_m
    use bitsy
    implicit none
    
    integer :: uid = 1

    type :: data_node
        integer :: uid
        character(len = :), allocatable :: valor
        type(data_node), pointer :: next => null()
    end type data_node

    type :: hash_node
        integer :: uid
        character(len= : ), allocatable :: hash
        type(hash_node), pointer :: left => null()
        type(hash_node), pointer :: right => null()
        type(data_node), pointer :: data => null()
    end type hash_node

    type :: merkle
        ! The root of the tree
        type(hash_node), pointer :: tophash => null()
        ! The list of data
        type(data_node), pointer :: first_data => null()
        type(data_node), pointer :: last_data => null()
        integer :: pos = 0
    
    contains 
        procedure :: insert
        procedure :: generate_tree
        procedure :: get_listSize
        procedure :: get_data
        procedure :: create_tree
        procedure :: create_hashes
        procedure :: dot
        procedure :: dot_rec
    end type
contains
    
    subroutine insert(self, value)
        class(merkle), intent(inout) :: self
        character(len = :), allocatable, intent(in) :: value
        type(data_node), pointer :: new

        allocate(new)
        allocate(new%valor, source = value)
        new%uid = uid
        uid = uid + 1

        if(.not. associated(self%first_data)) then
            self%first_data => new
            self%last_data => new
        else
            self%last_data%next => new
            self%last_data => new
        end if
    end subroutine insert

    subroutine generate_tree(self)
        class(merkle), intent(inout) :: self
        integer :: levels = 1
        integer :: total_leaves, listSize
        character(len=:), allocatable :: negative_val
        negative_val = "-1"
        
        ! it will calculate the number of levels that tree has
        listSize = self%get_listSize()
        do while(2**levels < listSize)
            levels = levels + 1
        end do

        ! insert -1 to fill the list
        total_leaves = 2**levels
        self%pos = total_leaves
        do while(listSize<total_leaves)
            call  self%insert(negative_val)
            listSize = listSize + 1
        end do

        allocate(self%tophash)
        call self%create_tree(self%tophash, levels)
        call self%create_hashes(self%tophash, total_leaves)
    end subroutine generate_tree

    recursive subroutine create_tree(self, node, levels)
        class(merkle), intent(inout) :: self 
        type(hash_node), pointer, intent(inout) :: node
        integer, intent(in) :: levels
        
        node%uid = uid
        uid = uid + 1
        ! Traversing the tree in preorder
        if(levels > 0) then
            allocate(node%left)
            allocate(node%right)
            call self%create_tree(node%left, levels - 1)
            call self%create_tree(node%right, levels - 1)
        end if
    end subroutine create_tree

    recursive subroutine create_hashes(self, node, total_leaves)
        class(merkle), intent(inout) :: self
        type(hash_node), pointer, intent(inout) :: node 
        integer, intent(in) :: total_leaves
        integer :: pos
        character(len = :), allocatable :: hash
        ! Traversing the tree in postorder
        if(associated(node)) then
            call self%create_hashes(node%left, total_leaves)
            call self%create_hashes(node%right, total_leaves)

            ! if the node is a leaf, it will get the data from the list
            if(.not. associated(node%left) .and. .not. associated(node%right)) then
                pos = total_leaves - self%pos
                node%data => self%get_data(pos)
                
                hash = node%data%valor
                node%hash = sha256(hash)

                self%pos = self%pos - 1
            ! if the node is not a leaf, it will concatenate the hash of its children
            else
                hash = adjustl(node%left%hash(1:len(node%left%hash)))
                hash = hash//adjustl(node%right%hash(1:len(node%right%hash)))
                node%hash = sha256(hash) 
            end if
        end if
    end subroutine create_hashes

    function get_listSize(self) result(res)
        class(merkle), intent(inout) :: self
        type(data_node), pointer :: current
        integer :: res    

        res = 0
        current => self%first_data
        do while(associated(current)) 
            res = res + 1
            current => current%next
        end do
    end function get_listSize

    function get_data(self, pos) result(current)
        class(merkle), intent(inout) :: self
        integer, intent(inout) :: pos
        type(data_node), pointer :: current

        current => self%first_data
        do while(associated(current))
            if(pos == 0) return
            pos = pos - 1
            current => current%next
        end do
    end function get_data

    subroutine dot(self)
        class(merkle), intent(inout) :: self
        integer :: io
        io = 1
        
        open(io, file='merkle.dot', status='replace')
        write(io, '(A)') 'graph{ node [shape=rectangle];'
        call self%dot_rec(self%tophash, io)
        write(io, '(A)') '}'
        close(io)

        call execute_command_line('dot -Tpng merkle.dot > merkle.png')
    end subroutine dot

    subroutine dot_rec(self, temp, io)
        class(merkle), intent(in) :: self
        class(hash_node), intent(in), pointer :: temp
        integer, intent(in) :: io

        if(.not. associated(temp)) return
        write (io, '(A,I5,A,A,A)') ' ', temp%uid, ' [label="', temp%hash, '"];'

        if (associated(temp%left)) then
            write (io, '(A,I5,A,I5,A)') ' ', temp%uid, ' -- ', temp%left%uid, ';'
        end if

        if (associated(temp%right)) then
            write (io, '(A,I5,A,I5,A)') ' ', temp%uid, ' -- ', temp%right%uid, ';'
        end if
    
        call self%dot_rec(temp%left, io)
        call self%dot_rec(temp%right, io)

        if (associated(temp%data)) then
            write (io, '(A,I5,A,A,A)') ' ', temp%data%uid, ' [label="', temp%data%valor, '" shape=rect];'
            write (io, '(A,I5,A,I5,A)') ' ', temp%uid, ' -- ', temp%data%uid, ';'
        end if
    end subroutine dot_rec

end module merkle_m