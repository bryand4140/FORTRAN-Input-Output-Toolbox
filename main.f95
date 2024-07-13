program main
    use IO_Toolbox
    implicit none

    real(pv), dimension(20,3) :: A
    real(pv), allocatable :: B(:,:)
    character(len=15), dimension(3) :: column_labels

    print*, "I/O Toolbox Test"


    column_labels(1) = 'Column 1'
    column_labels(2) = 'Column 2'
    column_labels(3) = 'Column 3'

    A(:,1) = 1.0_pv
    A(:,2) = 2.0_pv
    A(:,3) = 3.0_pv


    call write_matrix_with_labels(A, 'test.csv', column_labels)




end program main