program main
    use IO_Toolbox
    implicit none

    real(pv), dimension(20,3) :: A
    real(pv), allocatable :: B(:,:)

    print*, "I/O Toolbox Test"

    A(:,1) = 1.0_pv
    A(:,2) = 2.0_pv
    A(:,3) = 3.0_pv


    call write_matrix(A,'test.csv')

    call read_matrix(B, 'test.csv')

    call print_real_matrix(B)


end program main