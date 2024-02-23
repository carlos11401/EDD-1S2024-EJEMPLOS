program main
  use json_module
  implicit none
  ! Variables
  type(json_file) :: json ! JSON file object
  type(json_value), pointer :: list_p, client_p, attribute_p ! Pointers to JSON objects
  type(json_core) :: jsonc ! JSON core object
  character(:), allocatable :: nombre, id, img_g, img_p
  integer :: i, size ! Size of the list
  logical :: found ! Flag to check if the attribute was found

  call json%initialize()
  call json%load(filename='clients.json') ! Load the JSON file
  call json%info('', n_children=size) ! Get the number of children in the root object
  call json%get_core(jsonc) ! Get the core object, that will be used to get the children
  call json%get('', list_p, found) ! Get the list of children
  
  do i = 1, size
    ! Get the i-th child
    call jsonc%get_child(list_p, i, client_p, found=found)
    ! Get the attributes of the client
    call jsonc%get_child(client_p, 'id', attribute_p, found=found)
    if(found) call jsonc%get(attribute_p, id)

    call jsonc%get_child(client_p, 'nombre', attribute_p, found=found)
    if(found) call jsonc%get(attribute_p, nombre)

    call jsonc%get_child(client_p, 'img_g', attribute_p, found=found)
    if(found) call jsonc%get(attribute_p, img_g)

    call jsonc%get_child(client_p, 'img_p', attribute_p, found=found)
    if(found) call jsonc%get(attribute_p, img_p)

    ! Print the values
    print '(10a)', 'ID: ', trim(id), ' Nombre: ', trim(nombre), ' img_g: ', trim(img_g), ' img_p: ', trim(img_p)
  end do  
  ! Clean up
  call json%destroy()
  
end program main
