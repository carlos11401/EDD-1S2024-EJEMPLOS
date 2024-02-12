module circleList

  type circle_list
    integer :: size = 0
    type(node), pointer :: head => null()
    type(node), pointer :: tail => null()

    contains
      procedure :: add
      procedure :: print
      procedure :: remove
  end type circle_list

  type node
    integer :: id
    type(node), pointer :: next => null()
    type(node), pointer :: prev => null()
  end type node

  contains

  subroutine add(self, id)
    class(circle_list), intent(inout) :: self
    integer, intent(in) :: id

    type(node), pointer :: newNode 
    allocate(newNode)

    newNode%id = id

    if(.not. associated(self%head)) then
      newNode%next => newNode
      newNode%prev => newNode
      self%head => newNode
      self%tail => newNode
    else
      self%tail%next => newNode
      self%head%prev => newNode
      newNode%next => self%head
      newNode%prev => self%tail
      self%tail => newNode
    end if

    self%size = self%size + 1
    
  end subroutine add
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
        print *, current%id
        current => current%next
        i = i + 1

        if (i >= self%size) exit
      end do
    end if
  end subroutine print

  subroutine remove(self, id)
    class(circle_list), intent(inout) :: self
    integer, intent(in) :: id
    type(node), pointer :: current
    type(node), pointer :: next
    type(node), pointer :: prev

    current => self%head

    if(.not. associated(self%head)) then
      print*, "Empty list"
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

      else if (self%tail%id == id) then
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
          if(current%id == id) then
            next => current%next
            prev => current%prev
            next%prev => prev
            prev%next => next
            deallocate(current)
            self%size = self%size - 1
            exit
          end if
          current => current%next
          
        end do
      end if
    end if
  end subroutine remove

end module circleList