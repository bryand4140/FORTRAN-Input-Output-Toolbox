module IO_Toolbox
    use MOD_Select_Kind, only: pv
    implicit none

    private !Set all subroutines to private by default

    public :: write_matrix, read_matrix
    public :: write_labeled_matrix
    public :: prv, piv, prm
    public :: read_labeled_matrix


    !Input/Output Toolbox - Contains subroutines for reading and
    !writing matrices to CSV files and printing vectors and matrices.


contains

subroutine write_matrix(matrix, filename, path, scientific, use_wsl_path)
    ! This subroutine writes a matrix to a CSV file.
    !
    ! Input:
    !   matrix:       the matrix to write to the csv file
    !   filename:     the name of the csv file to write to. Must have .csv extension. 
    !   path:         optional argument specifying the directory path for the output file
    !   scientific:   optional argument to specify whether to use scientific notation
    !                 for the output values.
    !   use_wsl_path: optional logical argument. If .true., the subroutine treats the
    !                 provided path as a Linux/WSL path (using '/').
    !
    ! Note on Paths:
    !   - For Windows, use double backslashes in the path name (e.g., 'C:\\Users\\username\\Desktop')
    !   - For WSL (Linux), use forward slashes (e.g., '/home/username/Desktop')
    
    implicit none

    ! Declare input arguments
    real(pv), intent(in) :: matrix(:,:)
    character(len=*), intent(in) :: filename
    character(len=*), intent(in), optional :: path
    logical, intent(in), optional :: scientific
    logical, intent(in), optional :: use_wsl_path

    ! Declare local variables
    integer :: i, j, status
    character(len=30) :: value
    logical :: use_scientific, is_wsl
    character(len=256) :: full_path

    ! Determine if scientific notation should be used
    if (present(scientific)) then
        use_scientific = scientific
    else
        use_scientific = .false.
    end if

    ! Determine if WSL path should be used
    if (present(use_wsl_path)) then
        is_wsl = use_wsl_path
    else
        is_wsl = .false.
    end if

    ! Construct the full path
    if (present(path)) then
        if (is_wsl) then
            ! WSL/Linux uses '/'
            if (path(len_trim(path):len_trim(path)) == '/') then
                full_path = trim(path) // trim(filename)
            else
                full_path = trim(path) // '/' // trim(filename)
            end if
        else
            ! Windows: use backslash
            if (path(len_trim(path):len_trim(path)) == '\\') then
                full_path = trim(path) // trim(filename)
            else
                full_path = trim(path) // '\\' // trim(filename)
            end if
        end if
    else
        full_path = trim(filename)
    end if

    ! Open the file for writing
    open(unit=10, file=full_path, status='replace', action='write', &
         form='formatted', iostat=status)
    if (status /= 0) then
        print*, 'Error opening file ', trim(full_path)
        stop
    end if

    ! Write a spacer and log the action to the console
    print*, " " 
    write(*,"(A,A)") "Writing matrix to file ", trim(full_path)
    
    ! Write the matrix to the file in CSV format
    do i = 1, size(matrix, 1)
        do j = 1, size(matrix, 2)
            if (use_scientific) then
                write(value, '(E15.6)') matrix(i,j)
            else
                write(value, '(F15.6)') matrix(i,j)
            end if
            if (j < size(matrix, 2)) then
                write(10, '(A)', advance='no') trim(value) // ','
            else
                write(10, '(A)', advance='no') trim(value)
            end if
        end do
        write(10, *)
    end do

    ! Close the file and finish
    close(10)
    write(*,"(A)") "File write Complete."
    print*, " "

end subroutine write_matrix


subroutine read_matrix(matrix, filename, path)
    ! This subroutine reads a CSV file into a matrix.
    !
    ! Input:
    !   filename: the name of the CSV file to read from. Must have .csv extension.
    !   path:     (optional) directory path where the file is located.
    !
    ! Output:
    !   matrix: the matrix read from the CSV file (must be allocatable).
    !-------------------------------------------------------------------------------

    implicit none

    ! Declare the input arguments
    character(len=*), intent(in) :: filename
    real(pv), allocatable, intent(out) :: matrix(:,:)
    character(len=*), intent(in), optional :: path

    ! Declare local variables
    integer :: i, j, ios, nrows, ncols, pos
    character(len=1000) :: line
    character(len=15) :: token
    real, allocatable :: temp_matrix(:,:)
    character(len=256) :: full_path
    character(len=1) :: separator

    ! Determine path separator based on OS
    separator = '/'
    
    ! Construct the full file path
    if (present(path)) then
        if (path(len_trim(path):len_trim(path)) == separator) then
            full_path = trim(path) // trim(filename)
        else
            full_path = trim(path) // separator // trim(filename)
        end if
    else
        full_path = trim(filename)
    end if

    ! Open the file for reading
    open(unit=10, file=full_path, status='old', action='read', &
         form='formatted', iostat=ios)
    if (ios /= 0) then
        print*, 'Error: Could not open file ', trim(full_path)
        stop
    end if

    ! Determine the number of rows and columns
    nrows = 0
    ncols = 0
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (nrows == 0) then
            ncols = 1
            do pos = 1, len_trim(line)
                if (line(pos:pos) == ',') ncols = ncols + 1
            end do
        end if
        nrows = nrows + 1
    end do

    ! Allocate the matrix
    allocate(temp_matrix(nrows, ncols))

    ! Rewind the file to the beginning
    rewind(10)

    ! Read the data into the matrix
    i = 1
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        pos = 1
        do j = 1, ncols
            call get_token(line, pos, token)
            read(token, *) temp_matrix(i, j)
        end do
        i = i + 1
    end do

    ! Close the file
    close(10)

    ! Return the matrix
    matrix = temp_matrix

    contains

    subroutine get_token(input_line, input_pos, output_token)
        ! This subroutine extracts a token from a line starting at position pos
        character(len=*), intent(in) :: input_line
        integer, intent(inout) :: input_pos
        character(len=15), intent(out) :: output_token
        integer :: start, end

        start = input_pos
        end = index(input_line(start:), ',') - 1
        if (end == -1) then
            end = len_trim(input_line) - start + 1
        end if
        output_token = input_line(start:start+end-1)
        input_pos = start + end + 1
    end subroutine get_token

end subroutine read_matrix


subroutine write_labeled_matrix(matrix, column_labels, filename, path, scientific, use_wsl_path)
    ! General Description: This subroutine writes a matrix to a CSV file with column labels.
    !
    ! Input:
    !   matrix:         the matrix to write to the csv file
    !   column_labels:  an array of strings containing the labels for each column
    !   filename:       the name of the csv file to write to. Must have .csv extension.
    !Optional Inputs:
    !   path:           optional argument specifying the directory path for the output file
    !   scientific:     optional argument to specify whether to use scientific notation
    !                   for the output values.
    !   use_wsl_path:   optional logical argument. If .true., the subroutine treats the
    !                   provided path as a Linux/WSL path (using '/').
    !
    ! Note on Paths:
    !   - For Windows, use double backslashes in the path name (e.g., 'C:\\Users\\username\\Desktop')
    !   - For WSL (Linux), use forward slashes (e.g., '/home/username/Desktop')

    implicit none

    ! Declare input arguments
    real(pv), intent(in) :: matrix(:,:)
    character(len=*), intent(in) :: filename
    character(len=*), intent(in) :: column_labels(:)
    character(len=*), intent(in), optional :: path
    logical, intent(in), optional :: scientific
    logical, intent(in), optional :: use_wsl_path

    ! Declare local variables
    integer :: i, j, status, num_columns
    character(len=30) :: value
    logical :: use_scientific, is_wsl
    character(len=256) :: full_path

    ! Determine if scientific notation should be used
    if (present(scientific)) then
        use_scientific = scientific
    else
        use_scientific = .false.
    end if

    ! Determine if WSL path should be used
    if (present(use_wsl_path)) then
        is_wsl = use_wsl_path
    else
        is_wsl = .false.
    end if

    ! Construct the full path
    if (present(path)) then
        if (is_wsl) then
            if (path(len_trim(path):len_trim(path)) == '/') then
                full_path = trim(path) // trim(filename)
            else
                full_path = trim(path) // '/' // trim(filename)
            end if
        else
            if (path(len_trim(path):len_trim(path)) == '\\') then
                full_path = trim(path) // trim(filename)
            else
                full_path = trim(path) // '\\' // trim(filename)
            end if
        end if
    else
        full_path = trim(filename)
    end if

    ! Ensure the number of labels matches the number of matrix columns
    num_columns = size(matrix, 2)
    if (size(column_labels) /= num_columns) then
        print*, 'Error: Number of column labels does not match the number of columns in the matrix.'
        stop
    end if

    ! Open the file for writing
    open(unit=10, file=full_path, status='replace', action='write', form='formatted', iostat=status)
    if (status /= 0) then
        print*, 'Error opening file ', trim(full_path)
        stop
    end if

    ! Log the action to the console
    print*, " "
    write(*,"(A,A)") "Writing labeled matrix to file ", trim(full_path)

    ! Write column labels in fixed-width fields (15 characters) so they line up above the data columns.
    do j = 1, num_columns
        write(10, '(A15)', advance='no') adjustl(trim(column_labels(j)))
        if (j < num_columns) then
            write(10, '(A)', advance='no') ','
        else
            write(10, *)  ! Newline after header
        end if
    end do

    ! Write the matrix data in CSV format using the same field width
    do i = 1, size(matrix, 1)
        do j = 1, num_columns
            if (use_scientific) then
                write(value, '(E15.6)') matrix(i,j)
            else
                write(value, '(F15.6)') matrix(i,j)
            end if
            if (j < num_columns) then
                write(10, '(A)', advance='no') trim(value) // ','
            else
                write(10, '(A)') trim(value)
            end if
        end do
    end do

    ! Close the file and finish
    close(10)
    write(*,"(A)") "File write complete."
    print*, " "

end subroutine write_labeled_matrix


subroutine read_labeled_matrix(matrix, column_labels, filename, path)
    ! Reads a labeled CSV file into a matrix.
    
    implicit none

    ! Input/Output arguments
    character(len=*), intent(in) :: filename
    real(pv), allocatable, intent(out) :: matrix(:,:)
    character(len=*), allocatable, intent(out) :: column_labels(:)
    character(len=*), intent(in), optional :: path

    ! Local variables
    integer :: i, j, ios, nrows, ncols, pos
    character(len=1000) :: line
    character(len=15) :: token
    real, allocatable :: temp_matrix(:,:)
    character(len=256) :: full_path
    character(len=1) :: separator

    ! Set path separator
    separator = '/'

    ! Construct full file path
    if (present(path)) then
        if (path(len_trim(path):len_trim(path)) == separator) then
            full_path = trim(path) // trim(filename)
        else
            full_path = trim(path) // separator // trim(filename)
        end if
    else
        full_path = trim(filename)
    end if

    ! Open the file for reading
    open(unit=10, file=full_path, status='old', action='read', &
         form='formatted', iostat=ios)
    if (ios /= 0) then
        print*, 'Error: Could not open file ', trim(full_path)
        stop
    end if

    ! Read the first line to get column labels
    read(10, '(A)', iostat=ios) line
    if (ios /= 0) then
        print*, 'Error: Failed to read column labels from file.'
        stop
    end if

    ! Determine the number of columns
    ncols = 1
    do pos = 1, len_trim(line)
        if (line(pos:pos) == ',') ncols = ncols + 1
    end do

    ! Allocate column labels dynamically
    if (allocated(column_labels)) deallocate(column_labels)
    allocate(column_labels(ncols))

    ! Extract column labels
    pos = 1
    do j = 1, ncols
        call get_token(line, pos, column_labels(j))
    end do

    ! Determine number of rows (excluding the header)
    nrows = 0
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle  ! Skip empty lines
        nrows = nrows + 1
    end do

    ! Allocate the matrix dynamically
    if (allocated(matrix)) deallocate(matrix)
    allocate(matrix(nrows, ncols))

    ! Rewind to read numerical data (skip header)
    rewind(10)
    read(10, '(A)') line  ! Skip header row
    

    ! Read data into the matrix
    i = 1
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle  ! Skip empty lines, if any
        pos = 1
        do j = 1, ncols
            call get_token(line, pos, token)
            if (len_trim(token) == 0) then
                print*, "Warning: empty token encountered at row", i, "column", j
                matrix(i, j) = 0.0_pv  ! or choose an appropriate default value
            else
                read(token, *) matrix(i, j)
            end if
        end do
        i = i + 1
    end do
    

    ! Close file
    close(10)

    contains

    subroutine get_token(input_line, input_pos, output_token)
        character(len=*), intent(in) :: input_line
        integer, intent(inout) :: input_pos
        character(len=15), intent(out) :: output_token
        integer :: start, end

        start = input_pos
        end = index(input_line(start:), ',') - 1
        if (end == -1) then
            end = len_trim(input_line) - start + 1
        end if
        output_token = input_line(start:start+end-1)
        input_pos = start + end + 1
    end subroutine get_token

end subroutine read_labeled_matrix



!-----------------------------------------------------------------------
! Printing Tools

SUBROUTINE prv(A)
    ! This subroutine prints a one-dimensional array of real numbers
    ! prv = print real vector

    IMPLICIT NONE

    ! Declare A as a one-dimensional array of unknown size
    REAL(pv), INTENT(IN) :: A(:)
    INTEGER :: i, N

    ! Determine the size of the array A
    N = SIZE(A)

    ! Loop over the array and print each element
    DO i = 1, N
        WRITE(*, '(ES15.4)') A(i)
    END DO

END SUBROUTINE prv


SUBROUTINE piv(A)
    INTEGER, INTENT(IN) :: A(:)
    INTEGER :: i
    DO i = 1, SIZE(A)
        WRITE(*, '(I0)') A(i)
    END DO
END SUBROUTINE piv


SUBROUTINE prm(matrix)
    ! This subroutine prints a two-dimensional array of real numbers
    ! prm = print real matrix

    REAL(pv), INTENT(IN) :: matrix(:,:)
    INTEGER :: i, j

    DO i = 1, SIZE(matrix, 1)
        DO j = 1, SIZE(matrix, 2)
            WRITE(*, '(F15.4, " ")', ADVANCE='NO') matrix(i,j)
        END DO
        WRITE(*, *) ! newline
    END DO
END SUBROUTINE prm

end module IO_Toolbox