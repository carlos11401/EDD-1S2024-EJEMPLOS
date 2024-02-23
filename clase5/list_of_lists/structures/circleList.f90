module circleList
  use linkedList
  implicit none

  type node
    integer :: id
    type(node), pointer :: next => null()
    type(node), pointer :: prev => null()
    type(linked_list) :: simpleList = linked_list() ! Simple list
  end type node

  type circle_list
    type(node), pointer :: head => null()
    type(node), pointer :: tail => null()
    integer :: size = 0
    contains
    procedure :: add 
    procedure :: add_cl 
    procedure :: add_sl
    procedure :: print
    procedure :: remove
    procedure :: print_dot
  end type circle_list

  contains

  subroutine add(self, id_cl, id_sl)
    class(circle_list), intent(inout) :: self
    integer, intent(in) :: id_cl
    integer, intent(in) :: id_sl
    logical :: found

    ! Add new value to the simple list if the id_cl is found
    found = self%add_sl(id_cl, id_sl)
    ! If the id_cl is not found, add a new node on the circle list
    ! and add the id_sl to the simple list
    if (.not. found) then
      call self%add_cl(id_cl, id_sl)
    end if

  end subroutine add

  subroutine add_cl(self, id_cl, id_sl)
    class(circle_list), intent(inout) :: self
    integer, intent(in) :: id_cl
    integer, intent(in) :: id_sl

    ! Create a new node
    type(node), pointer :: newNode
    allocate(newNode)

    ! Set the id of the new node
    newNode%id = id_cl 
    ! Add the simple list
    call newNode%simpleList%push(id_sl)

    
    ! If the list is empty, set the new node as the head and tail
    if (.not. associated(self%head)) then
      newNode%next => newNode
      newNode%prev => newNode
      self%head => newNode
      self%tail => newNode
    else
      ! Set the new node as the next of the tail
      self%tail%next => newNode
      ! Set the new node as the previous of the head
      self%head%prev => newNode
      ! Set the head as the next of the new node
      newNode%next => self%head
      ! Set the tail as the previous of the new node
      newNode%prev => self%tail
      ! Set the new node as the new tail
      self%tail => newNode
    end if
    ! increment the size of the list
    self%size = self%size + 1
  end subroutine add_cl
  
  function add_sl(self, id_cl, id_sl) result(found)
    class(circle_list), intent(inout) :: self
    integer, intent(in) :: id_cl
    integer, intent(in) :: id_sl
    type(node), pointer :: current
    integer :: i
    logical :: found ! If the id_cl is found in the list

    found = .false.
    current => self%head

    if (.not. associated(current)) then
      print *, "Empty list :("
      return
    else
      if(self%head%id == id_cl) then ! If the head is the one we are looking for
        call self%head%simpleList%push(id_sl)
        found = .true.
        return
      else if(self%tail%id == id_cl) then ! If the tail is the one we are looking for
        call self%tail%simpleList%push(id_sl)
        found = .true.
        return
      else ! If the head and tail are not the ones we are looking for
        i = 0
        do 
          if (current%id == id_cl ) then 
            ! Add to simple list
            call current%simpleList%push(id_sl)
            found = .true.
            return
          end if

          current => current%next ! Move to the next node 
          i = i + 1 ! Increment the counter
          ! If the counter is greater than the size of the list, exit the loop
          if(i >= self%size) exit 
        end do
      end if
    end if
  end function add_sl

  subroutine remove(self, id)
    class(circle_list), intent(inout) :: self
    integer, intent(in) :: id
    type(node), pointer :: current
    type(node), pointer :: next
    type(node), pointer :: prev

    current => self%head

    if (.not. associated(current)) then
      print *, "Empty list :("
      return
    else

      if(self%head%id == id) then
        self%head => current%next

        next => current%next
        prev => current%prev
        next%prev => prev
        prev%next => next
        deallocate(current)
        self%size = self%size - 1
        ! If the list is empty, set the head and tail to null
        if(self%size == 0) then
          self%head => null()
          self%tail => null()
        end if
      else if(self%tail%id == id) then
        current => self%tail
        self%tail => current%prev

        next => current%next
        prev => current%prev
        next%prev => prev
        prev%next => next
        deallocate(current)
        self%size = self%size - 1
      else
        do
          if (current%id == id ) then
                    
            next => current%next
            prev => current%prev
            next%prev => prev
            prev%next => next
            deallocate(current)
            self%size = self%size - 1
            return
          end if
          current => current%next
        end do
      end if
    end if
  end subroutine remove

  subroutine print(self)
    class(circle_list), intent(inout) :: self
    type(node), pointer :: current
    integer :: i

    current => self%head

    if (.not. associated(current)) then
      print *, "Empty list :)"
      return
    else
      i = 0
      do
        print '(I0)', current%id
        call current%simpleList%print()

        current => current%next
        i = i + 1

        if (i >= self%size) exit
      end do
    end if
  end subroutine print

  subroutine print_dot(self, filename)
    class(circle_list), intent(inout) :: self
    character(len=*), intent(in) :: filename
    type(node), pointer :: current
    integer :: id

    current => self%head

    ! Abrir el archivo DOT
    open(unit=10, file=filename, status='replace')

    ! Escribir el encabezado del archivo DOT
    write(10, '(a)') 'digraph CircularList {'
    write(10, '(a)') '  node [shape=circle];'
    write(10, '(a)') '  rankdir = LR;'

    if (.not. associated(current)) then
      write(10, '(a)') '  EmptyList [label="Empty list"];'
    else
        write(10, '(a, i0, a, i0)') '  Head -> Node', self%head%id,';'
        write(10, '(a, i0, a, i0)') '  Tail -> Node', self%tail%id,';'
        do
          id = current%id
          ! Escribir nodos
          write(10, '(a, i0, a, i0)') '  Node', id, ' [label="', id, '"];'

          ! Escribir aristas
          write(10, '(a, i0, a, i0)') '  Node', current%prev%id, ' -> Node', id, ';'
          write(10, '(a, i0, a, i0)') '  Node', current%next%id, ' -> Node', id, ';'

          current => current%next
          if (id == self%tail%id) exit
        end do
    end if

    ! Cerrar el archivo DOT
    write(10, '(a)') '}'
    close(10)
  end subroutine print_dot

end module CircleList