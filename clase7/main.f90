program myapp
  use AVL_Tree_M
  implicit none
  type(Tree_t), pointer :: tree
  integer :: keys(18) = [10, 5, 15, 3, 1, 12, 17, 7, 4, 6, 9, 11, 2, 20, 24, 21, 23, 28]
  integer :: i

  call tree%newTree()
  ! Insert keys into the AVL tree
  do i = 1, size(keys)
      call tree%insert(keys(i))
  end do
  print *, "Creating the graph of the tree"
  call tree%generateGraph()
  
end program myapp