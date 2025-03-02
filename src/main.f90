program main
    use MOD_Select_Kind, only: pv
    use IO_Toolbox
    implicit none

    real(pv), dimension(20,3) :: A
    real(pv), allocatable :: B(:,:)
    character(len=15), dimension(3) :: column_labels
    character(len=20), allocatable :: labels(:)
    character(len = 100) :: filename


    print*, "I/O Toolbox Test"

    filename = 'test.csv'


    A(:,1) = 1.0_pv
    A(:,2) = 2.0_pv
    A(:,3) = 3.0_pv


    call write_matrix(A, 'test.csv', scientific = .true.)


    !Test the function write_labeled_matrix
    filename = 'test2.csv'
    column_labels = ['Col 1', 'Col 2', 'Col 3']

    call write_labeled_matrix(A, column_labels, trim(filename), scientific = .true.)

    !Read the matrix back into 
    call read_matrix(B, "test.csv")

    !Print the matrix that was read into B.
    call prm(B)

    !Import the matrix from test2.csv
    call read_labeled_matrix(B, labels, "test2.csv")
    print*, "Matrix successfully read from file:", trim(filename)
    print*, "Column Labels:", labels

    call prm(B)




end program main