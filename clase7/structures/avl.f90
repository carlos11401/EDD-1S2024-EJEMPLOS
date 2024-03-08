module AVL_Tree_M
  implicit none

  ! Cons
  integer, parameter :: LEFT_HEAVY = -1
  integer, parameter :: BALANCED = 0
  integer, parameter :: RIGHT_HEAVY = +1

  type Node_t
      integer :: Value
      integer :: Factor
      type(Node_t), pointer :: Left => null()
      type(Node_t), pointer :: Right => null()
  end type Node_t

  type Tree_t
      type(Node_t), pointer :: root => null()
      contains
      procedure :: newTree
      procedure :: insert
      procedure :: generateGraph
  end type Tree_t

  contains

  function NewNode(value) result(nodePtr)
    type(Node_t), pointer :: nodePtr
    integer, intent(in) :: value
    allocate(nodePtr)
    nodePtr%Value = value
    nodePtr%Factor = 0
    nodePtr%Left => null()
    nodePtr%Right => null()
  end function NewNode

  subroutine newTree(self)
    class(Tree_t), intent(inout) :: self
    self%root => null()
  end subroutine newTree

  function rotationII(n, n1) result(result_node)
      type(Node_t), pointer :: n, n1, result_node
      
      n%Left => n1%Right
      n1%Right => n
      if (n1%Factor == -1) then
          n%Factor = 0
          n1%Factor = 0
      else
          n%Factor = -1
          n1%Factor = 1
      end if
      result_node => n1
  end function rotationII

  function rotationDD(n, n1) result(result_node)
      type(Node_t), pointer :: n, n1, result_node

      n%Right => n1%Left
      n1%Left => n
      if (n1%Factor == 1) then
          n%Factor = 0
          n1%Factor = 0
      else
          n%Factor = 1
          n1%Factor = -1
      end if
      result_node => n1
  end function rotationDD

  function rotationDI(n, n1) result(result_node)
    type(Node_t), pointer :: n, n1, result_node, n2

    n2 => n1%Left
    n%Right => n2%Left
    n2%Left => n
    n1%Left => n2%Right
    n2%Right => n1
    if (n2%Factor == 1) then
        n%Factor = -1
    else
        n%Factor = 0
    end if
    if (n2%Factor == -1) then
        n1%Factor = 1
    else
        n1%Factor = 0
    end if
    n2%Factor = 0
    result_node => n2
  end function rotationDI

  function rotationID(n, n1) result(result_node)
      type(Node_t), pointer :: n, n1, result_node, n2
      n2 => n1%Right
      n%Left => n2%Right
      n2%Right => n
      n1%Right => n2%Left
      n2%Left => n1
      if (n2%Factor == 1) then
          n1%Factor = -1
      else
          n1%Factor = 0
      end if
      if (n2%Factor == -1) then
          n%Factor = 1
      else
          n%Factor = 0
      end if
      n2%Factor = 0
      result_node => n2
  end function rotationID

  recursive function insert2(root, value, increase) result(result_node)
      type(Node_t), pointer :: root, result_node, n1
      logical, intent(out) :: increase
      integer, intent(in) :: value

      if (.not. associated(root)) then
          allocate(result_node)
          root => NewNode(value)
          increase = .true.
      else if (value < root%Value) then
          root%Left => insert2(root%Left, value, increase)
          if (increase) then
            select case (root%Factor)
              case (RIGHT_HEAVY)
                  root%Factor = 0
                  increase = .false.
              case (BALANCED)
                  root%Factor = -1
              case (LEFT_HEAVY)
                  n1 => root%Left
                  if (n1%Factor == -1) then
                      root => rotationII(root, n1)
                  else
                      root => rotationID(root, n1)
                  end if
                  increase = .false.
            end select
          end if
      else if (value > root%Value) then
          root%Right => insert2(root%Right, value, increase)
          if (increase) then
              select case (root%Factor)
              case (RIGHT_HEAVY)
                  n1 => root%Right
                  if (n1%Factor == 1) then
                      root => rotationDD(root, n1)
                  else
                      root => rotationDI(root, n1)
                  end if
                  increase = .false.
              case (BALANCED)
                  root%Factor = 1
              case (LEFT_HEAVY)
                  root%Factor = 0
                  increase = .false.
              end select
          end if
      end if

      result_node => root
  end function insert2

  subroutine insert(tree, value)
    class(Tree_t), intent(inout) :: tree
    integer, intent(in) :: value
    logical :: increase

    increase = .false.
    tree%root => insert2(tree%root, value, increase)
  end subroutine insert



  subroutine GenerateGraph(this)
    class(Tree_t), intent(inout) :: this
    character(len=:), allocatable :: dotStructure
    character(len=:), allocatable :: createNodes
    character(len=:), allocatable :: linkNodes
    createNodes = ''
    linkNodes = ''


    dotStructure = "digraph G{" // new_line('a')
    dotStructure = dotStructure // "node [shape=circle];" // new_line('a')

    if (associated(this%root)) then
        call RoamTree(this%root, createNodes, linkNodes)
    end if

    dotStructure = dotStructure // trim(createNodes) // trim(linkNodes) // "}" // new_line('a')
    call write_dot(dotStructure)
    print *, "Archivo actualizado existosamente."
end subroutine GenerateGraph

recursive subroutine RoamTree(actual, createNodes, linkNodes)
    type(Node_t), pointer :: actual
    character(len=:), allocatable, intent(inout) :: createNodes
    character(len=:), allocatable, intent(inout) :: linkNodes
    character(len=20) :: address
    character(len=20) :: str_value

    if (associated(actual)) then
        ! SE OBTIENE INFORMACION DEL NODO ACTUAL
      address = get_address_memory(actual)
      write(str_value, '(I0)') actual%Value
      createNodes = createNodes // '"' // trim(address) // '"' // '[label="' // trim(str_value) // '"];' // new_line('a')
      ! VIAJAMOS A LA SUBRAMA IZQ
      if (associated(actual%Left)) then
        linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
        address = get_address_memory(actual%Left)
        linkNodes = linkNodes // '"' // trim(address) // '" ' &
                  // '[label = "L"];' // new_line('a')

      end if
      ! VIAJAMOS A LA SUBRAMA DER
      if (associated(actual%Right)) then
        address = get_address_memory(actual)
        linkNodes = linkNodes // '"' // trim(address) // '"' // " -> "
        address = get_address_memory(actual%Right)
        linkNodes = linkNodes // '"' // trim(address) // '" ' &
                  // '[label = "R"];' // new_line('a')
      end if

      call RoamTree(actual%Left, createNodes, linkNodes)
      call RoamTree(actual%Right, createNodes, linkNodes)
    end if
end subroutine RoamTree

  function get_address_memory(node) result(address)
    !class(matrix_t), intent(in) :: self
    type(Node_t), pointer :: node
    character(len=20) :: address
    ! integer 8
    integer*8 :: i

    i = loc(node) ! get the address of x
    ! convert the address to string
    write(address, 10) i 
    10 format(I0)

  end function get_address_memory

  subroutine write_dot(code)
    character(len=*), intent(in) :: code
    open(10, file='graph.dot', status='replace', action='write')
    write(10, '(A)') trim(code)
    close(10)
    ! Genera la imagen PNG
    call system("dot -Tpng graph.dot -o grafo.png")
  end subroutine write_dot

end module AVL_Tree_M
