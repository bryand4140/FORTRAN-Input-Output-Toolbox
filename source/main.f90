program main
    use MOD_Select_Precision, only: pv
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
    integer :: status

    ! Derived type for standard output derived type (SODT) example variables
    type(std_output_file) :: SODT 


    !-------------------------------------------------------------------------------------
    print*, "I/O Toolbox Test"

    ! Set path to outputs directory
    path = "./outputs"

    ! Initialize A
    A(:,1) = 1.0_pv
    A(:,2) = 2.0_pv
    A(:,3) = 3.0_pv

    ! Write matrix A to CSV using scientific notation
    call write_matrix(A, 'test.csv', path = path, scientific = .true.)

    ! Test write_labeled_matrix
    filename = 'test2.csv'
    column_labels = ['eta  ', 'theta', 's_i  ']
    call write_labeled_matrix(A, column_labels, filename, path = path, scientific = .true.)

    ! Read matrix back into B
    call read_matrix(B, 'test.csv', status, path = path)
    if (status /= 0) then
        print *, "Error reading matrix from file, status =", status
    else
        call prm(B)
    end if

    ! Import labeled matrix from test2.csv
    call read_matrix(B, 'test2.csv', status, path=path, num_head_rows=1, column_labels=labels)
    print*, "Matrix successfully read from file: ", trim(filename)
    print*, "Column Labels:", labels
    call prm(B)

    ! Build DOM and write labeled matrix
    allocate(DOM(40, 2))
    do i = 1, size(DOM,1)
        DOM(i, 1) = real(i, kind=pv) / 40.0_pv
        DOM(i, 2) = sin(2.0 * DOM(i, 1))
    end do
    call write_labeled_matrix(DOM, ['x', 'y'], 'test3.csv', path = path, scientific = .true.)


    !----------------------------------------------------------------
    ! Example of using the standard output derived type
    SODT%filename = 'my_data_output_1.dat'
    SODT%path = path
    SODT%x_label = 'Time, s'
    SODT%y_label = 'Three random profiles'
    SODT%title = 'This is the title'
    SODT%legend_on = .true.
    SODT%data_labels = ['Time (s)', 'C1      ', 'R1      ', 'K1      ']

    !generate some random real numbers and put into the data matrix. 
    allocate(SODT%data(100, 4))

    SODT%data(:, 1) = [(i, i=1, 100)] ! Time
    call random_number(SODT%data(:, 2)) ! C1
    call random_number(SODT%data(:, 3)) ! R1
    call random_number(SODT%data(:, 4)) ! K1

    call write_std_output(SODT)


    

end program main