module header_m
  
  private

  type, public :: matrix_node
    integer :: value, row, col
    type(matrix_node), pointer :: left => null()
    type(matrix_node), pointer :: right => null()
    type(matrix_node), pointer :: up => null()
    type(matrix_node), pointer :: down => null()
  end type matrix_node

  !Definición del tipo nodo
  type, public:: header_node
    integer :: position = 0
    type(header_node), pointer :: next => null()
    type(header_node), pointer :: prev => null()
    type(matrix_node), pointer :: access => null()
    contains
        procedure :: init_h_n
  end type header_node

  !Definición de la cabecera
  type, public :: header_t
    type(header_node), pointer :: first => null()
    type(header_node), pointer :: last  => null()
    integer :: size = 0
    contains
      procedure, public :: init_h_t
      procedure :: add
      procedure :: print 
  end type header_t

  contains

  !Funciones
  subroutine init_h_t(self)
    class(header_t), intent(inout) :: self
    self%size = 0
    self%first => null()
    self%last => null()
  end subroutine init_h_t

  subroutine init_h_n(self)
    class(header_node), intent(inout) :: self
    self%position = 0
  end subroutine init_h_n

  function add(self, pos)
    class(header_t), intent(inout) :: self
    integer, intent(in) :: pos
    type(header_node), pointer :: new_node
    type(header_node), pointer :: current
    type(header_node), pointer :: add ! Value to return

    allocate(new_node)
    new_node%position = pos

    ! If the header is empty
    if(self%size == 0) then
      self%first => new_node
      self%last => new_node
      self%size = self%size + 1
      ! Return the new node
      add => new_node 
    else
      ! If the position is already in the first or last node
      if(pos == self%first%position) then
        add => self%first  ! Return the first node
        return
      else if(pos == self%last%position) then
        add => self%last ! Return the last node
        return
      end if
      ! Insert at the beginning
      if(pos < self%first%position) then
        new_node%next => self%first
        self%first%prev => new_node
        self%first => new_node
        self%size = self%size + 1
        ! Return the new node
        add => new_node
      ! Insert at the end
      else if(pos > self%last%position) then
        new_node%prev => self%last
        self%last%next => new_node
        self%last => new_node
        self%size = self%size + 1
        ! Return the new node
        add => new_node
      else
        ! Insert in the middle
        current => self%first%next
        do while(associated(current))
          ! If the position is already in the list
          if(current%position == pos) then
            deallocate(new_node)
            ! Return the current node
            add => current
            exit
          ! If the position is between two nodes
          else if(current%position > pos) then
            new_node%next => current
            new_node%prev => current%prev
            current%prev%next => new_node
            current%prev => new_node
            self%size = self%size + 1
            ! Return the new node
            add => new_node
            exit
          end if
          current => current%next
        end do
      end if
    end if
  end function add
  
  subroutine print(self)
      class(header_t), intent(inout) :: self
      type(header_node), pointer :: current
      current => self%first

      do while(associated(current))
          print *, current%position, ","
          current => current%next
      end do
  end subroutine

end module header_m
