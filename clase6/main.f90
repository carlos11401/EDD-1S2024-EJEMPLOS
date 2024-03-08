program main
  use matrix_m
  implicit none

  type(matrix_t) :: mtx
  integer :: i, count, size, midle1, midle2
  logical :: insert

  call mtx%init()

  size = 10
  midle1 = size/2
  midle2 = midle1 + 1
  
  ! first inclined line
  do i = 1, size
    call mtx%add(i, i, 1)
  end do
  ! second inclined line
  i = size
  count = 1
  do while (i > 0)
    call mtx%add(i, count, 2)
    i = i - 1
    count = count + 1
  end do
  ! vertical line
  do i = 1, size
    if( i /= midle1 .and.  i /= midle2) then
      if(insert) then
        call mtx%add(i, midle1, 3)
        insert = .false.
      else
        call mtx%add(i, midle2, 4)
        insert = .true.
      end if
    end if
  end do
  ! horizontal line
  do i = 1, size
    if( i /= midle1 .and.  i /= midle2) then
      if(insert) then
        call mtx%add(midle1, i, 5)
        insert = .false.
      else
        call mtx%add(midle2, i, 6)
        insert = .true.
      end if
    end if
  end do

  !call mtx%print()
  call mtx%create_dot()
  
end program main
