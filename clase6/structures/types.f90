module types_m
  type node
    integer :: row
    integer :: col
    integer :: value
    type(node), pointer :: next
    type(node), pointer :: prev
    type(node), pointer :: top
    type(node), pointer :: bottom
  end type node

  contains

  function search(this, value) result(node)
    class(header), intent(inout) :: this
    integer, intent(in) :: value
    type(header_node) :: node

    type(header_node), pointer :: current
    ! if header is empty
    if(this%size == 0) then
      node = null()
      return
    else
      current => this%first
      do while(associated(current))
        if(current%position == value) then
          node = current
          return
        else
          current => current%next
        end if
      end do
    end if

  end function search

end module types_m