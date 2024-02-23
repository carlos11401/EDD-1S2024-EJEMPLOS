program myprogram
  use circleList
  implicit none
  
  type(circle_list) :: cl

  call cl%add(1, 50)
  call cl%add(2, 60)
  call cl%add(3, 70)
  call cl%add(4, 80)
  call cl%add(5, 90)

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

  call cl%add(6, 100)
  call cl%add(7, 110)
  call cl%add(8, 120)
  call cl%add(9, 130)
  call cl%add(7, 111)
  call cl%add(8, 121)
  call cl%add(9, 131)
  call cl%add(8, 122)
  call cl%add(9, 132)
  call cl%add(9, 133)
  print *, "After adding  more elements"
  call cl%print()

end program myprogram