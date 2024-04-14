program main
    use hash_table_m
    implicit none

    type(HashTable) :: table
    integer :: num_list(9) = [5, 522, 16, 1, 18, 29, 42, 500, 60]
    integer :: i
    do i = 1, 9
        call table%insert(num_list(i))
    end do
    call table%print()
    call table%search(42)
    call table%search(29)
end program main