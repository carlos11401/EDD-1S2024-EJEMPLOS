module linkedList
  implicit none

  type :: linked_list
    type(node), pointer :: head => null() ! head of the list

    contains
      procedure :: push
      procedure :: print
      procedure :: delete_by_position
  end type linked_list

  type :: node
    integer :: value
    type(node), pointer :: next
  end type node

  contains

  subroutine push(self, value)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: value

    type(node), pointer :: newNode
    allocate(newNode)

    newNode%value = value
    newNode%next => null()

    if (.not. associated(self%head)) then
      self%head => newNode
    else
      newNode%next => self%head
      self%head => newNode
    end if

    print *, 'pushed:: ', value
  end subroutine push

  subroutine delete_by_position(self, position)
    class(linked_list), intent(inout) :: self
    integer, intent(in) :: position
    type(node), pointer :: current, previous
    integer :: counter

    current => self%head
    previous => null()

    if(position == 1) then
      self%head => current%next
      deallocate(current)
      return
    end if

    counter = 1
    do while (associated(current) .and. counter < position)
      previous => current
      current => current%next
      counter = counter + 1
    end do

    if (.not. associated(current)) then
      print *, 'Position not found'
      return
    end if

    previous%next => current%next
    deallocate(current)
  end subroutine delete_by_position

  subroutine print(self)
    class(linked_list), intent(in) :: self

    type(node), pointer :: current

    current => self%head

    do while (associated(current))
      print *, current%value
      current => current%next
    end do
  end subroutine print
end module linkedList