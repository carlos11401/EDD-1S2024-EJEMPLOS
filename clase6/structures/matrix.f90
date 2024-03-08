module matrix_m
  use header_m
  implicit none

  type matrix_t
    type(header_t), pointer :: col => null()
    type(header_t), pointer :: row => null()
    !type(graph_t), pointer :: graph => null()
    contains
    procedure :: init
    procedure :: add
    procedure :: print
    procedure, private :: verify_column
    procedure, private :: move_first_lr_pointers
    procedure, private :: move_middle_lr_pointers
    procedure, private :: move_last_lr_pointers

    procedure, private :: verify_row
    procedure, private :: move_first_ud_pointers
    procedure, private :: move_middle_ud_pointers
    procedure, private :: move_last_ud_pointers

    procedure :: create_dot
    procedure, private :: get_content
    procedure, private :: align_col_nodes
    procedure :: get_address_memory
    procedure :: write_dot

  end type matrix_t

  contains
  subroutine init(self)
    class(matrix_t), intent(inout) :: self
    type(header_t), pointer :: row_hdr
    type(header_t), pointer :: col_hdr

    allocate(row_hdr)
    allocate(col_hdr)
    call row_hdr%init_h_t()
    call col_hdr%init_h_t()
    self%row => row_hdr
    self%col => col_hdr
  end subroutine init

  subroutine add(self, row, col, value)
    class(matrix_t), intent(inout) :: self
    integer, intent(in) :: row, col, value

    type(header_node), pointer :: row_hdr_n
    type(header_node), pointer :: col_hdr_n
    type(matrix_node), pointer :: new_mtx_n

    ! Add headers
    allocate(col_hdr_n)
    allocate(row_hdr_n)
    col_hdr_n => self%col%add(col)
    row_hdr_n => self%row%add(row)
    ! New matrix node
    allocate(new_mtx_n)
    new_mtx_n%value = value
    new_mtx_n%row = row_hdr_n%position
    new_mtx_n%col = col_hdr_n%position

    ! Move pointers
    call self%verify_column(row_hdr_n, new_mtx_n)
    call self%verify_row(col_hdr_n, new_mtx_n)

  end subroutine add

  subroutine verify_column(self, row_hdr_n, new_mtx_n)
    class(matrix_t), intent(inout) :: self
    type(header_node), pointer :: row_hdr_n
    type(matrix_node), pointer :: new_mtx_n
    type(matrix_node), pointer :: is_middle
    logical :: is_first

    is_first = move_first_lr_pointers(self, row_hdr_n, new_mtx_n)
    if(is_first) return

    is_middle => move_middle_lr_pointers(self, row_hdr_n, new_mtx_n)
    if(.not. associated(is_middle)) return

    ! Like the middle function returns a pointer of the last node
    ! that means that the new node is the last one
    call move_last_lr_pointers(self, is_middle, new_mtx_n)
    
  end subroutine verify_column
  function move_first_lr_pointers(self, row_hdr_n, new_mtx_n)
    class(matrix_t), intent(inout) :: self
    type(header_node), pointer :: row_hdr_n
    type(matrix_node), pointer :: new_mtx_n
    type(matrix_node), pointer :: current
    logical :: move_first_lr_pointers

    move_first_lr_pointers = .false.

    current => row_hdr_n%access

    if(.not. associated(current)) then ! If header node is empty
      row_hdr_n%access => new_mtx_n
      move_first_lr_pointers = .true.
    else if(new_mtx_n%col < current%col) then ! If new node will be on the left of the first node
      row_hdr_n%access%left => new_mtx_n
      new_mtx_n%right => row_hdr_n%access
      row_hdr_n%access => new_mtx_n
      move_first_lr_pointers = .true.
    end if
  end function move_first_lr_pointers
  function move_middle_lr_pointers(self, row_hdr_n, new_mtx_n)
    class(matrix_t), intent(inout) :: self
    type(header_node), pointer :: row_hdr_n
    type(matrix_node), pointer :: new_mtx_n
    type(matrix_node), pointer :: current
    type(matrix_node), pointer :: move_middle_lr_pointers

    current => row_hdr_n%access
    do while (associated(current%right))
      ! If the new node is in the middle of the row
      if(current%col < new_mtx_n%col .and. new_mtx_n%col < current%right%col) then
        new_mtx_n%right => current%right
        new_mtx_n%left => current
        current%right%left => new_mtx_n
        current%right => new_mtx_n
        nullify(move_middle_lr_pointers)
        return
      else if(current%col == new_mtx_n%col) then
        nullify(move_middle_lr_pointers)
        return
      end if
      current => current%right
    end do
    allocate(move_middle_lr_pointers)
    move_middle_lr_pointers => current
  end function move_middle_lr_pointers
  subroutine move_last_lr_pointers(self, las_mtx_n, new_mtx_n)
    class(matrix_t), intent(inout) :: self
    type(matrix_node), pointer :: las_mtx_n
    type(matrix_node), pointer :: new_mtx_n

    las_mtx_n%right => new_mtx_n
    new_mtx_n%left => las_mtx_n
  end subroutine move_last_lr_pointers

  subroutine verify_row(self, col_hdr_n, new_mtx_n)
    class(matrix_t), intent(inout) :: self
    type(header_node), pointer :: col_hdr_n
    type(matrix_node), pointer :: new_mtx_n
    type(matrix_node), pointer :: is_middle
    logical :: is_first

    is_first = move_first_ud_pointers(self, col_hdr_n, new_mtx_n)
    if(is_first) return

    is_middle => move_middle_ud_pointers(self, col_hdr_n, new_mtx_n)
    if(.not. associated(is_middle)) return

    ! Like the middle function returns a pointer of the last node
    ! that means that the new node is the last one
    call move_last_ud_pointers(self, is_middle, new_mtx_n)

    
  end subroutine verify_row
  function move_first_ud_pointers(self, col_hdr_n, new_mtx_n)
    class(matrix_t), intent(inout) :: self
    type(header_node), pointer :: col_hdr_n
    type(matrix_node), pointer :: new_mtx_n
    type(matrix_node), pointer :: current
    logical :: move_first_ud_pointers

    move_first_ud_pointers = .false.

    current => col_hdr_n%access
    if(.not. associated(current)) then ! If header node is empty
      col_hdr_n%access => new_mtx_n
      move_first_ud_pointers = .true.
    else if(new_mtx_n%row < current%row) then ! If new node is up the first node
      col_hdr_n%access%up => new_mtx_n
      new_mtx_n%down => col_hdr_n%access
      col_hdr_n%access => new_mtx_n
      move_first_ud_pointers = .true.
    end if
  end function move_first_ud_pointers
  function move_middle_ud_pointers(self, col_hdr_n, new_mtx_n)
    class(matrix_t), intent(inout) :: self
    type(header_node), pointer :: col_hdr_n
    type(matrix_node), pointer :: new_mtx_n
    type(matrix_node), pointer :: current
    type(matrix_node), pointer :: move_middle_ud_pointers

    current => col_hdr_n%access
    do while (associated(current%down))
      ! If the new node is in the middle of the row
      if(current%row < new_mtx_n%row .and. new_mtx_n%row < current%down%row) then
        new_mtx_n%down => current%down
        new_mtx_n%up => current
        current%down%up => new_mtx_n
        current%down => new_mtx_n
        nullify(move_middle_ud_pointers)
        return
      end if
      current => current%down
    end do
    allocate(move_middle_ud_pointers)
    move_middle_ud_pointers => current
  end function move_middle_ud_pointers
  subroutine move_last_ud_pointers(self, las_mtx_n, new_mtx_n)
    class(matrix_t), intent(inout) :: self
    type(matrix_node), pointer :: las_mtx_n
    type(matrix_node), pointer :: new_mtx_n

    las_mtx_n%down => new_mtx_n
    new_mtx_n%up => las_mtx_n
  end subroutine move_last_ud_pointers

  subroutine print(self)
    class(matrix_t), intent(inout) :: self

    type(header_node), pointer :: row_hdr_node, col_hdr_node
    type(header_t), pointer :: row_hdr, col_hdr
    type(matrix_node), pointer :: current_node
    integer :: row, col
    integer :: num_rows, num_cols
    
    row_hdr => self%row
    col_hdr => self%col
    
    ! Obtener el número total de filas y columnas
    num_rows = row_hdr%size
    num_cols = col_hdr%size

    ! Imprimir encabezado de columna
    col_hdr_node => col_hdr%first
    write(*, '(a)', advance='no') "       "
    do while (associated(col_hdr_node))
      write(*, '(I5)', advance='no') col_hdr_node%position
      col_hdr_node => col_hdr_node%next
    end do
    print *
    ! Imprimir separador
    write(*, '(a)', advance='no') "        ------------------------"
    print *

    ! Imprimir matriz
    row_hdr_node => row_hdr%first
    do row = 1, num_rows
      ! Imprimir encabezado de fila
      write(*, '(I5, a)', advance='no') row, " |"
      current_node => row_hdr_node%access
      do col = 1, num_cols
        if (associated(current_node)) then
          if(current_node%col == col) then
            write(*, '(I5)', advance='no') current_node%value
            current_node => current_node%right
          else
            write(*, '(I5)', advance='no') 0
          end if
        end if
      end do
      print *
      row_hdr_node => row_hdr_node%next
    end do
  end subroutine print

  subroutine create_dot(self)
    class(matrix_t), intent(inout) :: self
    character(:), allocatable :: code

    code = "digraph G{" // new_line('a')
    code = code // '  node[shape=box];' // new_line('a')
    code = code // '  MTX[ label = "Matrix", style = filled, fillcolor = firebrick1, group = 0 ];' // new_line('a')

    code = code // self%get_content()
    code = code // '}' // new_line('a')

    call self%write_dot(code)
        
  end subroutine create_dot

  function get_content(self) result(content)
    class(matrix_t), intent(inout) :: self
    type(header_node), pointer :: row_node
    type(matrix_node), pointer :: current_mtx_node
    character(:), allocatable :: alignCols
    character(:), allocatable :: alignRows
    character(:), allocatable :: createRowNodes
    character(:), allocatable :: createColNodes
    character(:), allocatable :: createMtxNodes
    character(:), allocatable :: linkNodes
    character(:), allocatable :: content
    character(len=10) :: row_pos
    character(len=10) :: aux2
    character(len=15) :: address
    character(len=15) :: address2
    content = ''
    alignRows = ''
    alignCols = ''
    createRowNodes = ''
    createColNodes = ''
    createMtxNodes = ''
    linkNodes = ''

    call self%align_col_nodes(createColNodes, alignCols, linkNodes)

    row_node => self%row%first
    ! link first row node to matrix node
    write(row_pos, '(I0)') row_node%position
    linkNodes = linkNodes // '  "MTX" -> "f' // trim(row_pos) // '";' // new_line('a')

    do while (associated(row_node))
      write(row_pos, '(I0)') row_node%position

      ! create row node
      createRowNodes = createRowNodes // '  "f' // trim(row_pos) // '" [label = "f' // trim(row_pos) 
      createRowNodes = createRowNodes // '"  style = filled, fillcolor = bisque1, group = 0 ];' // new_line('a')
      ! align row node
      alignRows = alignRows // '  { rank = same; ' // '"f' // trim(row_pos) // '";'
      ! link next and prev row nodes
      if(associated(row_node%next)) then
        write(aux2, '(I0)') row_node%next%position
        linkNodes = linkNodes // '  "f' // trim(row_pos) // '" -> "f' // trim(aux2) // '";' // new_line('a')
      else if(associated(row_node%prev)) then
        write(aux2, '(I0)') row_node%prev%position
        linkNodes = linkNodes // '  "f' // trim(row_pos) // '" -> "f' // trim(aux2) // '";' // new_line('a')
      end if

      ! link row node to first matrix node
      current_mtx_node => row_node%access
      address = self%get_address_memory(current_mtx_node)
      linkNodes = linkNodes // '  "f' // trim(row_pos) // '" -> "' // trim(address) // '";' // new_line('a')
      do while (associated(current_mtx_node))
        ! create matrix node
        address = self%get_address_memory(current_mtx_node)
        write(aux2, '(I0)') current_mtx_node%value
        createMtxNodes = createMtxNodes // '  "' // trim(address) // '" [label = "' // trim(aux2)
        write(aux2, '(I0)') current_mtx_node%col 
        createMtxNodes = createMtxNodes // '" group = ' // trim(aux2) // '];' // new_line('a')
        ! align matrix nodes
        alignRows = alignRows // '"' // trim(address) // '";'
        ! link matrix nodes RIGHT, LEFT, DOWN, UP
        if(associated(current_mtx_node%right)) then
          address2 = self%get_address_memory(current_mtx_node%right)
          linkNodes = linkNodes // '  "' // trim(address) // '" -> "' // trim(address2) // '";' // new_line('a')
        end if
        if(associated(current_mtx_node%left)) then
          address2 = self%get_address_memory(current_mtx_node%left)
          linkNodes = linkNodes // '  "' // trim(address) // '" -> "' // trim(address2) // '";' // new_line('a')
        end if
        if(associated(current_mtx_node%down)) then
          address2 = self%get_address_memory(current_mtx_node%down)
          linkNodes = linkNodes // '  "' // trim(address) // '" -> "' // trim(address2) // '";' // new_line('a')
        end if
        if(associated(current_mtx_node%up)) then
          address2 = self%get_address_memory(current_mtx_node%up)
          linkNodes = linkNodes // '  "' // trim(address) // '" -> "' // trim(address2) // '";' // new_line('a')
        else
          write(aux2, '(I0)') current_mtx_node%col 
          linkNodes = linkNodes // '  "c' // trim(aux2) // '" -> "' // trim(address) // '";' // new_line('a')
        end if

        current_mtx_node => current_mtx_node%right
      end do
      alignRows = alignRows // '};' // new_line('a')
      row_node => row_node%next
    end do
    content = createColNodes // createRowNodes // alignCols // alignRows // createMtxNodes // linkNodes
  end function get_content

  subroutine align_col_nodes(self, createNodes, align, linkNodes)
    class(matrix_t), intent(inout) :: self
    type(header_node), pointer :: col_hdr_node
    character(:), allocatable, intent(inout) :: align
    character(:), allocatable, intent(inout) :: createNodes
    character(:), allocatable, intent(inout) :: linkNodes
    character(len=10) :: aux
    character(len=10) :: aux2
    createNodes = ''
    align = ''
    linkNodes = ''

    align = '  { rank = same; "MTX";'
    
    col_hdr_node => self%col%first
    ! link first column node to matrix node
    write(aux, '(I0)') col_hdr_node%position
    linkNodes = linkNodes // '  "MTX" -> "c' // trim(aux) // '";' // new_line('a')

    do while (associated(col_hdr_node))
      write(aux, '(I0)') col_hdr_node%position

      ! create column node
      createNodes = createNodes // '  "c' // trim(aux) // '" [label = "c' // trim(aux) 
      createNodes = createNodes // '"  style = filled, fillcolor = bisque1, group = ' // trim(aux) // ' ];' // new_line('a')
      ! align column node
      align = align // '"c' // trim(aux) // '";'
      
      ! link next and prev nodes
      if(associated(col_hdr_node%next)) then
        write(aux2, '(I0)') col_hdr_node%next%position
        linkNodes = linkNodes // '  "c' // trim(aux) // '" -> "c' // trim(aux2) // '";' // new_line('a')
      end if
      if(associated(col_hdr_node%prev)) then
        write(aux2, '(I0)') col_hdr_node%prev%position
        linkNodes = linkNodes // '  "c' // trim(aux) // '" -> "c' // trim(aux2) // '";' // new_line('a')
      end if
      
      col_hdr_node => col_hdr_node%next
    end do
    align = align// '};' // new_line('a') 
  end subroutine align_col_nodes

  function get_address_memory(self, node) result(address)
    class(matrix_t), intent(in) :: self
    type(matrix_node), pointer :: node
    character(len=100) :: address
    ! integer 8
    integer*8 :: i

    i = loc(node) ! get the address of x
    ! convert the address to string
    write(address, 10) i 
    10 format(I0)

  end function get_address_memory

  subroutine write_dot(self, code)
    class(matrix_t), intent(in) :: self
    character(len=*), intent(in) :: code
    open(10, file='graph.dot', status='replace', action='write')
    write(10, '(A)') trim(code)
    close(10)
  end subroutine write_dot
end module matrix_m