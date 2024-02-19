program main
  use sortsMethods
  
  integer :: array(7) = [3,2,5,4,8,7,1] ! array desordenado
  integer :: arr2(7) ! array para almacenar el array ordenado
  integer :: found ! variable para almacenar el resultado de la busqueda

  ! imprimir array
  print '(10I1)', array
  ! ordenar array
  arr2 = bubleSort(array)
  print '(10I1)', arr2
  ! buscar un elemento en el array
  found = BinarySearch(arr2, 5)
  print '(I0)', found

end program main