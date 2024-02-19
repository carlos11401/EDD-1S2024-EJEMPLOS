! Para generar las imagenes de los grafos, se debe instalar graphviz
! El comando para generar la imagen es:
! dot -Tpng img1.dot -o img1.png
program myprogram
  use circleList
  implicit none

  type(circle_list) :: cl

  call cl%add(1)
  call cl%add(2)
  call cl%add(3)
  call cl%add(4)
  call cl%add(5)

  call cl%print_dot("img1.dot")

  call cl%print()

  call cl%remove(3)
  print *, "After removing 3"
  call cl%print()

  call cl%remove(5)
  print *, "After removing 5"
  call cl%print()

  call cl%remove(1)
  call cl%remove(2)
  print *, "After removing 1, 2"
  call cl%print()

  call cl%remove(4)
  print *, "After removing 1, 2, 4"
  call cl%print()
  call cl%print_dot("img2.dot")

  call cl%add(6)
  call cl%add(7)
  print *, "After adding 6, 7"
  call cl%print()
  call cl%print_dot("img3.dot")
  
end program myprogram