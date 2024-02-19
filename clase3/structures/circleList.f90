module circleList

  type circle_list
    integer :: size = 0
    type(node), pointer :: head => null()
    type(node), pointer :: tail => null()

    contains
      procedure :: add
      procedure :: print
      procedure :: print_dot
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

  subroutine print_dot(self, filename)
    class(circle_list), intent(inout) :: self ! referencia a la lista
    character(len=*), intent(in) :: filename ! nombre del archivo
    type(node), pointer :: current ! puntero al nodo actual
    integer :: id ! id del nodo

    ! puntero al nodo actual
    current => self%head

    ! abrir el archivo
    open(10, file=filename, status='replace') 
    
    ! escribir el encabezado
    write(10, '(a)') "digraph G {" ! encabezado del archivo dot
    write(10, '(a)') "  node [shape=circle];"  ! forma de los nodos
    write(10, '(a)') "  rankdir=LR" ! orientación del grafo

    if ( .not. associated(current))  then ! si la lista está vacía
      write(10, '(a)') "  EmptyList;" ! escribir un nodo que diga que la lista está vacía
    else ! si la lista no está vacía
      write(10, '(a, i0, a)') '  Head -> Node', self%head%id, ';' ! escribir la arista de la cabeza al primer nodo
      write(10, '(a, i0, a)') '  Tail -> Node', self%tail%id, ';' ! escribir la arista de la cola al último nodo
      
      do ! recorrer la lista
        id = current%id
        ! crear el nodo
        write(10, '(a, i0, a, i0, a)') '  Node', id, ' [label="', id, '"];'
        ! escribir las aristas
        write(10, '(a, i0, a, i0)') '  Node', current%prev%id, ' -> Node', id, ';' ! arista del nodo anterior al nodo actual
        write(10, '(a, i0, a, i0)') '  Node', current%next%id, ' -> Node', id, ';' ! arista del nodo siguiente al nodo actual

        ! avanzar al siguiente nodo
        current => current%next
        ! si ya se recorrió toda la lista, salir del ciclo
        if (id == self%tail%id) exit
      end do
    end if
    ! escribir el pie del archivo
    write(10, '(a)') "}"
    ! cerrar el archivo
    close(10)
  end subroutine print_dot
end module circleList