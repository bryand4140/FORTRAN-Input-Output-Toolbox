module IO_Toolbox
    implicit none

    !Input/Output Toolbox - Contains subroutines for reading and
    !writing matrices to CSV files and printing vectors and matrices.

    !Last Update: 5/25/2024

    !Author: BHD

    ! Define the kind for real numbers
    ! 15 = number of decimal digits
    ! 307 = number of bits
    integer, parameter :: pv = selected_real_kind(15, 307)



contains


subroutine write_matrix(matrix, filename, scientific)
    ! This subroutine writes a matrix to a CSV file.

    ! Input:
    !   matrix: the matrix to write to the csv file
    !   filename: the name of the csv file to write to
    !   scientific: optional argument to specify whether to use scientific notation

    implicit none

    ! Declare the input arguments
    real(pv), intent(in) :: matrix(:,:)
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: scientific

    ! Declare local variables
    integer :: i, j
    integer :: status
    character(len=30) :: value
    logical :: use_scientific

    ! Determine if scientific notation should be used
    if (present(scientific)) then
        use_scientific = scientific
    else
        use_scientific = .false.
    end if

    ! Open the file for writing
    open(unit=10, file=filename, status='replace', action='write', &
         form='formatted', iostat=status)
    if (status /= 0) then
        print*, 'Error opening file ', filename
        stop
    end if

    ! Write the matrix to the file
    print*, " " ! Spacer
    write(*,"(A)") "Writing matrix to file ", filename
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

    ! Close the file
    close(10)
    write(*,"(A)") "File write Complete."
    print*, " " ! Spacer

end subroutine write_matrix




subroutine read_matrix(matrix, filename)
    ! This subroutine reads a CSV file into a matrix
    implicit none

    ! Declare the input arguments
    character(len=*), intent(in) :: filename
    real(pv), allocatable, intent(out) :: matrix(:,:)

    ! Declare local variables
    integer :: i, j, ios, nrows, ncols, pos
    character(len=1000) :: line
    character(len=15) :: token
    real, allocatable :: temp_matrix(:,:)
    logical :: end_of_file

    ! Open the file for reading
    open(unit=10, file=filename, status='old', action='read', &
         form='formatted', iostat=ios)
    if (ios /= 0) then
        print*, 'Error opening file ', filename
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
        subroutine get_token(line, pos, token)
            ! This subroutine extracts a token from a line starting at position pos
            character(len=*), intent(in) :: line
            integer, intent(inout) :: pos
            character(len=15), intent(out) :: token
            integer :: start, end

            start = pos
            end = index(line(start:), ',') - 1
            if (end == -1) then
                end = len_trim(line) - start + 1
            end if
            token = line(start:start+end-1)
            pos = start + end + 1
        end subroutine get_token

end subroutine read_matrix


subroutine write_matrix_with_labels(matrix, filename, column_labels)
    ! This subroutine writes a matrix with column labels to a CSV file
    implicit none
    real(pv), intent(in) :: matrix(:,:)
    character(len=*), intent(in) :: filename
    character(len=15), intent(in) :: column_labels(:)
    integer :: i, j, status
    character(len=20) :: value
    character(len=20) :: label

    ! Open the file for writing
    open(unit=10, file=filename, status='replace', action='write', form='formatted', iostat=status)
    if (status /= 0) then
        print*, 'Error opening file ', filename
        stop
    end if

    ! Write the column labels to the file
    do j = 1, size(column_labels)
        write(label, '(A20)') trim(column_labels(j))
        if (j < size(column_labels)) then
            write(10, '(A)', advance='no') trim(label) // ','
        else
            write(10, '(A)', advance='no') trim(label)
        end if
    end do
    write(10, *)

    ! Write the matrix to the file
    do i = 1, size(matrix, 1)
        do j = 1, size(matrix, 2)
            write(value, '(F20.5)') matrix(i,j)
            if (j < size(matrix, 2)) then
                write(10, '(A)', advance='no') trim(value) // ','
            else
                write(10, '(A)', advance='no') trim(value)
            end if
        end do
        write(10, *)
    end do

    ! Close the file
    close(10)
end subroutine write_matrix_with_labels





subroutine read_matrix_with_labels(matrix, filename, column_labels)
    ! This subroutine reads a CSV file with column labels into a matrix
    implicit none

    ! Declare the input arguments
    character(len=*), intent(in) :: filename
    real(pv), allocatable, intent(out) :: matrix(:,:)
    character(len=15), allocatable, intent(out) :: column_labels(:)

    ! Declare local variables
    integer :: i, j, ios, nrows, ncols, pos
    character(len=1000) :: line
    character(len=15) :: token
    real, allocatable :: temp_matrix(:,:)
    logical :: end_of_file

    ! Open the file for reading
    open(unit=10, file=filename, status='old', action='read', &
         form='formatted', iostat=ios)
    if (ios /= 0) then
        print*, 'Error opening file ', filename
        stop
    end if

    ! Read the column labels
    read(10, '(A)', iostat=ios) line
    if (ios /= 0) then
        print*, 'Error reading column labels from file ', filename
        stop
    end if
    ncols = 1
    do pos = 1, len_trim(line)
        if (line(pos:pos) == ',') ncols = ncols + 1
    end do
    allocate(column_labels(ncols))
    pos = 1
    do j = 1, ncols
        call get_token(line, pos, column_labels(j))
    end do

    ! Determine the number of rows
    nrows = 0
    do
        read(10, '(A)', iostat=ios) line
        if (ios /= 0) exit
        nrows = nrows + 1
    end do

    ! Allocate the matrix
    allocate(temp_matrix(nrows, ncols))

    ! Rewind the file to the beginning and skip the first line (column labels)
    rewind(10)
    read(10, '(A)', iostat=ios) line

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

    ! Return the matrix and column labels
    matrix = temp_matrix

    contains
        subroutine get_token(line, pos, token)
            ! This subroutine extracts a token from a line starting at position pos
            character(len=*), intent(in) :: line
            integer, intent(inout) :: pos
            character(len=15), intent(out) :: token
            integer :: start, end

            start = pos
            end = index(line(start:), ',') - 1
            if (end == -1) then
                end = len_trim(line) - start + 1
            end if
            token = line(start:start+end-1)
            pos = start + end + 1
        end subroutine get_token

end subroutine read_matrix_with_labels




!-----------------------------------------------------------------------
! Printing Tools

SUBROUTINE print_real_vector(A)
    ! This subroutine prints a one-dimensional array of real numbers
    IMPLICIT NONE

    ! Declare A as a one-dimensional array of unknown size
    REAL(pv), INTENT(IN) :: A(:)
    INTEGER :: i, N

    ! Determine the size of the array A
    N = SIZE(A)

    ! Loop over the array and print each element
    DO i = 1, N
        WRITE(*, '(F0.4)') A(i)
    END DO

END SUBROUTINE print_real_vector


SUBROUTINE print_integer_vector(A)
    INTEGER, INTENT(IN) :: A(:)
    INTEGER :: i
    DO i = 1, SIZE(A)
        WRITE(*, '(I0)') A(i)
    END DO
END SUBROUTINE print_integer_vector


SUBROUTINE print_real_matrix(matrix)
  REAL(pv), INTENT(IN) :: matrix(:,:)
  INTEGER :: i, j

  DO i = 1, SIZE(matrix, 1)
    DO j = 1, SIZE(matrix, 2)
      WRITE(*, '(F8.4, " ")', ADVANCE='NO') matrix(i,j)
    END DO
    WRITE(*, *) ! newline
  END DO
END SUBROUTINE print_real_matrix


end module IO_Toolbox