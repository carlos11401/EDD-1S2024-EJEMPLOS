module linkedList
  implicit none
  private

  !Definición del tipo nodo
  type node
      private
      integer :: value
      type(node), pointer :: next => null()
  end type node

  !Definición del tipo lista
  type, public :: linked_list
      private
      type(node), pointer :: head => null()
      
      contains
        procedure :: push
        procedure :: delete_by_position
        procedure :: delete_by_value
        procedure :: print
  end type linked_list

  contains

  !Funciones
  subroutine push(self, value)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: value
  
    type(node), pointer :: newNode !Create pointer
    allocate(newNode)              !Allocate memory for a new node and make newNode point to it

    !Initialize the new node
    newNode%value = value
    newNode%next => null()

    if(.not. associated(self%head)) then
        !If the list is empty, the new node is the head
        self%head => newNode
    else
        !If the list is not empty, the new node is the new head
         newNode%next => self%head
        self%head => newNode
    end if      

    print '(A, I0)', "Pushed: ", value
  end subroutine push
    
  subroutine delete_by_position(self, position)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: position
    type(node), pointer :: current
    type(node), pointer :: previous
    integer :: i

    current => self%head
    previous => null()

    if(.not. associated(current)) then
        print *, "La lista está vacía"
        return
    end if

    if(position == 1) then
        self%head => current%next
        deallocate(current)
        return
    end if

    i = 1
    do while(associated(current) .and. i < position)
        previous => current
        current => current%next
        i = i + 1
        ! i = 3
    end do

    if(.not. associated(current)) then
        print *, "La posicion ", position, " no existe"
        return
    end if

    previous%next => current%next
    deallocate(current)
  end subroutine delete_by_position

  subroutine delete_by_value(self, value)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: value
    type(node), pointer :: current
    type(node), pointer :: previous

    current => self%head
    previous => null()

    if(.not. associated(current)) then
        print *, "La lista está vacía"
        return
    end if

    do while(associated(current) .and. current%value /= value)
        previous => current
        current => current%next
    end do

    if(.not. associated(current)) then
        print *, "El valor ", value
        return
    end if

    if(.not. associated(previous)) then
        self%head => current%next
    else
        previous%next => current%next
    end if

    deallocate(current)
  end subroutine delete_by_value
  
  subroutine print(self)
      class(linked_list), intent(inout) :: self
      type(node), pointer :: current
      character(len=20) :: str_number
      character(len=:), allocatable :: result
      result = "["
      current => self%head

      do while(associated(current))
        ! Converting integer to string
        write(str_number, '(I0)') current%value
        result = result // trim(adjustl(str_number)) // ", "
        current => current%next
      end do

      result = result // "]"
      print *, trim(adjustl(result))
  end subroutine

end module linkedList
