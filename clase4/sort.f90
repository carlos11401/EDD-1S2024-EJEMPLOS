module sortsMethods
  implicit none

  contains

  function bubleSort(arr) result(sortedArr)
    integer, intent(inout) :: arr(:) ! Arreglo a ordenar
    integer :: sortedArr(size(arr)) ! Arreglo ordenado
    integer :: i, j, temp, arrSize ! Variables auxiliares
    logical :: swapped = .false. ! Indica si se realizó un intercambio

    sortedArr = arr ! Copiar el arreglo original
    arrSize = size(arr)   ! Tamaño del arreglo
  
    do i = 1, arrSize - 1 
      do j = 1, arrSize - i
        ! Si el elemento actual es mayor que el siguiente
        if (sortedArr(j) > sortedArr(j + 1)) then
          ! Indicar que se realizó un intercambio
          swapped = .true. 
          ! Intercambiar los elementos
          temp = sortedArr(j) 
          sortedArr(j) = sortedArr(j + 1) 
          sortedArr(j + 1) = temp
        end if
      end do
      ! Si no se realizó ningún intercambio, el arreglo ya está ordenado
      if (.not. swapped) then
        exit
      end if
    end do
  end function bubleSort

  integer function BinarySearch(arr, key)
    integer, intent(in) :: arr(:) ! Arreglo a buscar
    integer, intent(in) :: key ! Elemento a buscar
    integer :: low, high, mid ! Variables auxiliares

    ! Inicializar los límites del arreglo
    low = 1 
    high = size(arr)

    do while (low <= high) ! Mientras que el arreglo no esté vacío
      ! Calcular el punto medio
      mid = (low + high) / 2
      if (arr(mid) == key) then ! ¡Eureka
          ! Elemento encontrado
          BinarySearch = mid  
          return
      else if (key > arr(mid)) then ! Si el elemento está en la mitad superior
          ! Descartar la mitad inferior
          low = mid + 1 
      else ! Si el elemento está en la mitad inferior
          ! Descartar la mitad superior
          high = mid - 1 
      end if
    end do

    BinarySearch = -1  ! Elemento no encontrado
  end function BinarySearch

end module sortsMethods