program main
    use MOD_Select_Kind, only: pv
    use IO_Toolbox
    implicit none

    real(pv), dimension(20,3) :: A
    real(pv), allocatable :: B(:,:)
    character(len=15), dimension(3) :: column_labels
    character(len=20), allocatable :: labels(:)
    character(len = 100) :: filename
    integer :: i
    real(pv), allocatable :: DOM(:,:)
    character(len = 250) :: path

    print*, "I/O Toolbox Test"

    filename = 'test.csv'
    path     = "/mnt/c/Users/bryan/OneDrive/CU/Code/FORTRAN-Input-Output-Toolbox/outputs/"


    A(:,1) = 1.0_pv
    A(:,2) = 2.0_pv
    A(:,3) = 3.0_pv


    call write_matrix(A, 'test.csv', scientific = .true., path = path, use_wsl_path = .true.)


    !Test the function write_labeled_matrix
    filename = 'test2.csv'
    column_labels = ['eta  ', 'theta', 's_i  ']

    call write_labeled_matrix(A, column_labels, trim(filename), scientific = .true., &
    path = path, use_wsl_path = .true.)

    !Read the matrix back into 
    call read_matrix(B, "test.csv", path = path)

    !Print the matrix that was read into B.
    call prm(B)

    ! Import labeled matrix from test2.csv
    call read_labeled_matrix(B, labels, "test2.csv", path = path)  
    print*, "Matrix successfully read from file:", trim(filename)
    print*, "Column Labels:", labels
    call prm(B)


    allocate(DOM(40, 2))

    do i = 1,size(DOM, DIM = 1)
        DOM(i, 1) = real(i, kind = pv) / 40.0_pv
        DOM(i, 2) = sin(2.0 * DOM(i, 1))
    end do

    call write_labeled_matrix(DOM, ['x', 'y'], 'test3.csv', scientific = .true., &
    path = path, use_wsl_path = .true.)

end program main