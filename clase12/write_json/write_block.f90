! To run the program, you need to have the json-fortran library installed
! gfortran -I./json-fortran/build/include -o main write_block.f90 bitsy.f90 ./json-fortran/build/lib/libjsonfortran.a

program main
  use json_module
  use bitsy
  implicit none

  type(json_core) :: json
  type(json_value), pointer :: myjson, data, vertex

  ! Initialize the JSON object
  call json%initialize()

  ! Creating a JSON object
  call json%create_object(myjson, '')

  ! Creating DATA attribute
  call json%create_array(data, 'DATA')

  ! Creating the vertex of node s1
  call json%create_object(vertex, '')
  call json%add(vertex, 'sucursal_o', 's1')
  call json%add(vertex, 'direccion_o', '6ta av 4 calle zon2 de villa nueva')
  call json%add(vertex, 'sucursal_d', 's2')
  call json%add(vertex, 'direccion_d', '3ta av 2 calle zon5 de villa nueva')
  call json%add(vertex, 'total', 200)

  ! Adding the vertex to the DATA array
  call json%add(data, vertex)

  ! Creating the vertex of node s2
  call json%create_object(vertex, '')
  call json%add(vertex, 'sucursal_o', 's2')
  call json%add(vertex, 'direccion_o', '3ta av 2 calle zon5 de villa nueva')
  call json%add(vertex, 'sucursal_d', 's3')
  call json%add(vertex, 'direccion_d', '7ma av 3 calle zon10 de villa nueva')
  call json%add(vertex, 'total', 150)

  ! Adding the vertex to the DATA array
  call json%add(data, vertex)

  ! Creating INDEX attribute
  call json%add(myjson, 'INDEX', 0)

  ! Adding NONCE attribute
  call json%add(myjson, 'NONCE', 0000434)
  
  ! Adding DATA attribute
  call json%add(myjson, data)

  ! Adding hashes attributes
  call json%add(myjson, 'ROOTMARKLE', sha256('Here goes the root Merkle hash'))
  call json%add(myjson, 'PREVIOUSHASH', 0000)
  call json%add(myjson, 'HASH', sha256('Here goes the hash of all elements in block'))

  ! Cleaning up
  nullify(data)
  nullify(vertex)
  ! Printing the JSON object
  call json%print(myjson, './myblock.json')
  call json%destroy(myjson)
end program main