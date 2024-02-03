program myapp
  use linkedList
  implicit none

  type(linked_list) :: mylist

  call mylist%push(1)
  call mylist%push(2)
  call mylist%push(3)
  call mylist%push(7)

  call mylist%print()

  call mylist%delete_by_position(3)

  print *, "After deleting 3rd element"

  call mylist%print()

end program myapp