module IO_Toolbox
    use MOD_Select_Precision, only: pv
    implicit none

    private !Set all subroutines to private by default

    public :: write_matrix
    public :: write_labeled_matrix
    public :: prv, piv, prm
    public :: read_matrix
    public :: write_std_output

    type, public :: std_output_file
        character(len = :), allocatable :: filename
        character(len = :), allocatable :: x_label
        character(len = :), allocatable :: y_label
        character(len = :), allocatable :: title
        character(len = :), allocatable :: path
        character(len = :), dimension(:), allocatable :: data_labels
        real(pv), allocatable :: data(:,:)
        logical :: grid_on
        logical :: legend_on
    end type std_output_file
        



    !Input/Output Toolbox - Contains subroutines for reading and
    !writing matrices to CSV files and printing vectors and matrices.


contains

subroutine write_matrix(matrix, filename, path, scientific)
    ! Writes a matrix to a data file. Auto-detects the path format.

    implicit none

    ! Input arguments
    real(pv), intent(in)           :: matrix(:,:)
    character(len=*), intent(in)   :: filename
    character(len=*), intent(in), optional :: path
    logical,    intent(in), optional :: scientific

    ! Local variables
    character(len=:), allocatable :: p
    logical              :: is_wsl, use_scientific
    character(len=1)     :: sep
    character(len=256)   :: full_path
    integer              :: unit, ios, i, j
    character(len=30)    :: value

    ! Determine if scientific notation should be used
    if (present(scientific)) then
        use_scientific = scientific
    else
        use_scientific = .false.
    end if

    ! Build full path with OS-specific separator
    if (present(path)) then
        p = trim(path)
        is_wsl = detect_wsl_path(p)
        if (is_wsl) then
            sep = '/'
        else
            sep = '\'
        end if
        if (p(len_trim(p):len_trim(p)) == sep) then
            full_path = p // trim(filename)
        else
            full_path = p // sep // trim(filename)
        end if
    else
        full_path = trim(filename)
    end if

    ! Open file for writing
    open(newunit=unit, file=full_path, status='replace', action='write', &
         form='formatted', iostat=ios)
    if (ios /= 0) then
        print*, 'Error opening file ', trim(full_path)
        stop
    end if

    ! Log to console
    print*, ' '
    write(*,'(A,A)') 'Writing matrix to file ', trim(full_path)

    ! Write matrix rows in CSV format
    do i = 1, size(matrix,1)
        do j = 1, size(matrix,2)
            if (use_scientific) then
                write(value,'(E15.6)') matrix(i,j)
            else
                write(value,'(F15.6)') matrix(i,j)
            end if
            if (j < size(matrix,2)) then
                write(unit,'(A)',advance='no') trim(value)//','
            else
                write(unit,'(A)')          trim(value)
            end if
        end do
    end do

    close(unit)
    write(*,'(A)') 'File write complete.'
    print*, ' '

end subroutine write_matrix


subroutine read_matrix(matrix, filename, status, path, num_head_rows, column_labels, delimiter)
    !---------------------------------------------------------------------------|
    ! Subroutine: Reads a data file into a 2D matrix, skipping a specified 
    !             number of header rows. Compatible with both Unix and Windows 
    !             file paths.The default delimiter is a comma (',').
    !
    ! Description:
    ! This subroutine processes a text file containing numerical data, ignoring 
    ! a given number of header lines, and stores the remaining data into a 
    ! two-dimensional matrix. It is designed to handle file paths from different 
    ! operating systems seamlessly.
    !
    ! Input:
    !   filename      - (Character, Intent(In)) The name of the data file to read.
    !
    ! Optional Inputs:
    !   path          - (Character, Intent(In), Optional) Directory path.
    !   num_head_rows - (Integer, Intent(In),   Optional) Number of header rows to skip.
    !   delimiter     - (Character, Intent(In), Optional) Delimiter character (default: ',').
    !
    ! Output:
    !   matrix        - (Real, Allocatable, Intent(Out)) 2D array of data.
    !   status        - (Integer, Optional) with the followin status flag options:
    !                   >> 0 = file read successfully
    !                   >> 1 - file does not exist
    !                   >> 2 - file is already opened
    !                   >> 3 - failed to read header rows
    !                   >> 4 - failed to read first data row
    !
    ! Optional Outputs:
    !   column_labels - (Character, Allocatable, Intent(Out), Optional) Column labels.
    !   status        - (Integer, Intent(Out), Optional) Status flag: 0=success, nonzero=error.
    !---------------------------------------------------------------------------|
    implicit none

    ! Input/Output arguments
    character(len=*), intent(in) :: filename
    real(pv), allocatable, intent(out) :: matrix(:,:)
    character(len=*), intent(in), optional :: path
    integer, intent(in), optional :: num_head_rows
    character(len=*), allocatable, intent(out), optional :: column_labels(:)
    integer, intent(out) :: status
    character(len=*), intent(in), optional :: delimiter

    ! Local variables
    integer :: i, j, ios, nrows, ncols, pos, skip_rows
    character(len=1000) :: line
    character(len=15) :: token
    character(len=256) :: full_path
    character(len=1) :: separator_unix, separator_win
    character(len=1) :: delim
    character(len=:), allocatable :: header_line
    logical :: is_windows, path_has_trailing_separator
    logical :: file_exists
    logical :: is_opened
    integer :: file_unit

    ! Initialize status
    status = -1

    ! Set default number of header rows to skip
    skip_rows = 0
    if (present(num_head_rows)) skip_rows = num_head_rows

    ! Set delimiter
    if (present(delimiter)) then
        delim = delimiter(1:1)
    else
        delim = ','
    end if

    ! Set path separators
    separator_unix = '/'
    separator_win = '\'

    ! Determine if we're on Windows by checking for environment variable
    is_windows = .false.
    call get_environment_variable("OS", line, status=ios)
    if (ios == 0) then
        if (index(line, "Windows") > 0) is_windows = .true.
    end if

    ! Construct full file path with proper handling for both OS types
    if (present(path)) then
        path_has_trailing_separator = .false.
        if (len_trim(path) > 0) then
            if (path(len_trim(path):len_trim(path)) == separator_unix .or. &
                path(len_trim(path):len_trim(path)) == separator_win) then
                path_has_trailing_separator = .true.
            end if
        end if
        if (path_has_trailing_separator) then
            full_path = trim(path) // trim(filename)
        else
            if (is_windows) then
                full_path = trim(path) // separator_win // trim(filename)
            else
                full_path = trim(path) // separator_unix // trim(filename)
            end if
        end if
    else
        full_path = trim(filename)
    end if

    ! Check if file exists and if it is already opened
    inquire(file=full_path, exist=file_exists)
    if (.not. file_exists) then
        print *, 'Error: File does not exist - ', trim(full_path)
        status = 1
        return
    end if

    inquire(file=full_path, opened=is_opened)
    if (is_opened) then
        print *, 'Warning: File is already opened.'
        ! Proceeding anyway
    end if

    ! Open the file for reading
    open(newunit=file_unit, file=full_path, status='old', action='read', &
         form='formatted', iostat=ios)
    if (ios /= 0) then
        status = 2
        print*, 'Error: Could not open file ', trim(full_path)
        return
    end if

    ! If column labels are requested and there's at least one header row
    if (present(column_labels) .and. skip_rows >= 1) then
        do i = 1, skip_rows
            read(file_unit, '(A)', iostat=ios) line
            if (ios /= 0) then
                status = 3
                print*, 'Error: Failed to read header rows from file.'
                close(file_unit)
                return
            end if
            ! Skip comment lines in header
            if (len_trim(line) > 0) then
                if (line(1:1) == '#' .or. line(1:1) == '!') cycle
            end if
            if (i == skip_rows) header_line = trim(line)
        end do

        ! Determine the number of columns
        ncols = 1
        do pos = 1, len_trim(header_line)
            if (header_line(pos:pos) == delim) ncols = ncols + 1
        end do

        if (allocated(column_labels)) deallocate(column_labels)
        allocate(column_labels(ncols))

        pos = 1
        do j = 1, ncols
            call get_token(header_line, pos, column_labels(j), delim)
            column_labels(j) = adjustl(column_labels(j)) ! Trim whitespace from label
        end do
    else
        do i = 1, skip_rows
            read(file_unit, '(A)', iostat=ios) line
            if (ios /= 0) then
                status = 4
                print*, 'Error: Failed to read header rows from file.'
                close(file_unit)
                return
            end if
            ! Skip comment lines in header
            if (len_trim(line) > 0) then
                if (line(1:1) == '#' .or. line(1:1) == '!') cycle
            end if
        end do

        ! Skip comment lines before first data row
        do
            read(file_unit, '(A)', iostat=ios) line
            if (ios /= 0) then
                status = 5
                print*, 'Error: Failed to read first data row.'
                close(file_unit)
                return
            end if
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '#' .or. line(1:1) == '!') cycle
            exit
        end do

        ncols = 1
        do pos = 1, len_trim(line)
            if (line(pos:pos) == delim) ncols = ncols + 1
        end do

        backspace(file_unit)
    end if

    ! Determine number of data rows
    nrows = 0
    do
        read(file_unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        if (line(1:1) == '#' .or. line(1:1) == '!') cycle
        nrows = nrows + 1
    end do

    if (allocated(matrix)) deallocate(matrix)
    allocate(matrix(nrows, ncols))

    rewind(file_unit)
    do i = 1, skip_rows
        read(file_unit, '(A)')
    end do

    i = 1
    do
        read(file_unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        if (line(1:1) == '#' .or. line(1:1) == '!') cycle
        pos = 1
        do j = 1, ncols
            call get_token(line, pos, token, delim)
            token = adjustl(token) ! Trim whitespace from token
            if (len_trim(token) == 0) then
                print*, "Warning: empty token encountered at row", i, "column", j
                matrix(i, j) = 0.0_pv
            else
                read(token, *, iostat=ios) matrix(i, j)
                if (ios /= 0) then
                    status = 6
                    print*, "Error: Failed to read value at row", i, "column", j
                    close(file_unit)
                    return
                end if
            end if
        end do
        i = i + 1
    end do

    close(file_unit)

    status = 0  ! <-- Add this line to indicate successful read

    contains

    subroutine get_token(input_line, input_pos, output_token, delim)
        character(len=*), intent(in) :: input_line
        integer, intent(inout) :: input_pos
        character(len=*), intent(out) :: output_token
        character(len=1), intent(in) :: delim
        integer :: start, end

        start = input_pos
        end = index(input_line(start:), delim) - 1
        if (end == -1) then
            end = len_trim(input_line) - start + 1
        end if
        output_token = input_line(start:start+end-1)
        output_token = adjustl(output_token) ! Trim whitespace from token
        input_pos = start + end + 1
    end subroutine get_token

end subroutine read_matrix


subroutine write_labeled_matrix(matrix, column_labels, filename, path, scientific)
    ! General Description: Writes a matrix with column labels to a CSV file.

    implicit none

    ! Input arguments
    real(pv), intent(in)           :: matrix(:,:)
    character(len=*), intent(in)   :: filename
    character(len=*), intent(in)   :: column_labels(:)
    character(len=*), intent(in), optional :: path
    logical,    intent(in), optional :: scientific

    ! Local variables
    integer              :: i, j, status, num_columns
    character(len=30)    :: value
    logical              :: use_scientific, is_wsl
    character(len=256)   :: full_path
    character(len=:), allocatable :: p

    ! Determine if scientific notation should be used
    if (present(scientific)) then
        use_scientific = scientific
    else
        use_scientific = .false.
    end if

    ! Construct the full path and detect format
    if (present(path)) then
        p = trim(path)
        is_wsl = detect_wsl_path(p)
        if (is_wsl) then
            ! Linux/WSL: use '/'
            if (p(len_trim(p):len_trim(p)) == '/') then
                full_path = p // trim(filename)
            else
                full_path = p // '/' // trim(filename)
            end if
        else
            ! Windows: use '\'
            if (p(len_trim(p):len_trim(p)) == '\\') then
                full_path = p // trim(filename)
            else
                full_path = p // '\\' // trim(filename)
            end if
        end if
    else
        full_path = trim(filename)
    end if

    ! Check labels length
    num_columns = size(matrix, 2)
    if (size(column_labels) /= num_columns) then
        print*, " " !Spacer
        print*, '-----------------------------------------------------------------------'
        print*, 'Error in IO_Toolbox --> write_labeled_matrix(): '
        print*, 'Number of column labels does not match number of matrix columns.'
        print*, ' >>> Number of columns in matrix: ', num_columns
        print*, ' >>> Number of column labels: ', size(column_labels)
        print*, '-----------------------------------------------------------------------'
        print*, " " !Spacer
        stop
    end if

    ! Open file for writing
    open(unit=10, file=full_path, status='replace', action='write', form='formatted', iostat=status)
    if (status /= 0) then
        print*, " " !Spacer
        print*, '-----------------------------------------------------------------------'
        print*, 'Error in IO_Toolbox --> write_labeled_matrix(): '
        print*, ' >>> Error opening file ', trim(full_path)
        print*, '-----------------------------------------------------------------------'
        print*, " " !Spacer
        stop
    end if

    ! Log
    print*, ' ' !Spacer
    write(*,'(A,A)') 'Writing labeled matrix to file ', trim(full_path)

    ! Write header labels
    do j = 1, num_columns
        write(10, '(A15)', advance='no') adjustl(trim(column_labels(j)))
        if (j < num_columns) then
            write(10, '(A)', advance='no') ','
        else
            write(10, *)
        end if
    end do

    ! Write matrix rows
    do i = 1, size(matrix, 1)
        do j = 1, num_columns
            if (use_scientific) then
                write(value, '(E15.6)') matrix(i, j)
            else
                write(value, '(F15.6)') matrix(i, j)
            end if
            if (j < num_columns) then
                write(10, '(A)', advance='no') trim(value)//','
            else
                write(10, '(A)') trim(value)
            end if
        end do
    end do

    close(10)
    write(*,'(A)') 'File write complete.'
    print*, ' '

end subroutine write_labeled_matrix


function detect_wsl_path(path_string) result(is_wsl)
    implicit none
    character(len=*), intent(in) :: path_string
    logical :: is_wsl
    
    is_wsl = .false.
    
    ! Check if path contains forward slashes (typical in WSL/Linux paths)
    if (index(path_string, '/') > 0) then
        is_wsl = .true.
        return
    end if
    
    ! Check for absolute Linux paths
    if (len_trim(path_string) >= 1) then
        if (path_string(1:1) == '/') then
            is_wsl = .true.
            return
        endif
    endif
    
    ! Check for common WSL mount points
    if (len_trim(path_string) >= 5) then
        if (path_string(1:5) == '/mnt/') then
            is_wsl = .true.
            return
        endif
    endif
    
    ! Check for Linux home directories
    if (len_trim(path_string) >= 6) then
        if (path_string(1:6) == '/home/') then
            is_wsl = .true.
            return
        endif
    endif
    
    ! If we're actually running in WSL and we have a Windows path, set to true
    if (system_is_wsl() .and. len_trim(path_string) >= 2) then
        if (path_string(2:2) == ':') then
            ! This is a Windows path being used in WSL, so we should treat it as a WSL path
            ! that needs conversion
            is_wsl = .true.
            return
        endif
    endif
end function detect_wsl_path


function system_is_wsl() result(is_wsl)
    implicit none
    logical :: is_wsl
    integer :: unit_num, ios
    character(256) :: line
    
    is_wsl = .false.
    
    ! Try to read /proc/version to check for WSL
    open(newunit=unit_num, file="/proc/version", status="old", action="read", iostat=ios)
    if (ios == 0) then
        read(unit_num, '(A)', iostat=ios) line
        close(unit_num)
        
        ! Check if the version contains "Microsoft" or "WSL"
        if (index(line, "Microsoft") > 0 .or. index(line, "WSL") > 0) then
            is_wsl = .true.
        endif
    endif
end function system_is_wsl


function strip_nonprintable(str) result(clean)
    character(len=*), intent(in) :: str
    character(len=len(str)) :: clean
    integer :: i, j
    j = 1
    do i = 1, len_trim(str)
        if (iachar(str(i:i)) >= 32 .and. iachar(str(i:i)) <= 126) then
            clean(j:j) = str(i:i)
            j = j + 1
        end if
    end do
    if (j <= len(clean)) clean(j:) = ' '
end function strip_nonprintable


subroutine write_std_output(S)
    implicit none
    type(std_output_file), intent(in) :: S

    integer           :: unit, ios, i, j
    integer           :: nrow, ncol
    logical           :: is_wsl
    character(len=256):: full_path
    character(len=512):: line
    character(len=30) :: tmp

    !— determine separator and build full_path
    is_wsl = detect_wsl_path(trim(S%path))
    if (len_trim(S%path) > 0) then
      if (is_wsl) then
        if ( S%path(len_trim(S%path):len_trim(S%path)) == '/' ) then
          full_path = trim(S%path)//trim(S%filename)
        else
          full_path = trim(S%path)//'/'//trim(S%filename)
        end if
      else
        if ( S%path(len_trim(S%path):len_trim(S%path)) == '\' ) then
          full_path = trim(S%path)//trim(S%filename)
        else
          full_path = trim(S%path)//'\'//'\'//trim(S%filename)
        end if
      end if
    else
      full_path = trim(S%filename)
    end if

    !— open file
    open(newunit=unit, file=full_path, status='replace', action='write', form='formatted', iostat=ios)
    if (ios /= 0) then
      print*, 'Error opening ', full_path
      stop
    end if

    !— write metadata as comment lines
    write(unit,'(A)') '# Title: '   // trim(S%title)
    write(unit,'(A)') '# X-Label: ' // trim(S%x_label)
    write(unit,'(A)') '# Y-Label: ' // trim(S%y_label)

    ! data_labels
    line = '# DataLabels:'
    do j = 1, size(S%data_labels)
      line = trim(line)//' '//adjustl(trim(S%data_labels(j)))
      if (j < size(S%data_labels)) line = trim(line)//','
    end do
    write(unit,'(A)') trim(line)

    ! grid_on / legend_on
    if (S%grid_on) then
        write(unit, '(A)') '# GridOn: true'
    else
        write(unit, '(A)') '# GridOn: false'
    end if

    if (S%legend_on) then
        write(unit, '(A)') '# LegendOn: true'
    else
        write(unit, '(A)') '# LegendOn: false'
    end if

    !Put a blank line in between the metadata and the data
    write(unit,'(A)') ' '

    !— write a plain header line with just the column labels
    ncol = size(S%data,2)
    line = ''
    do j = 1, ncol
      line = trim(line)//trim(S%data_labels(j))
      if (j < ncol) line = trim(line)//','
    end do
    write(unit,'(A)') trim(line)

    !— write the data block
    nrow = size(S%data,1)
    do i = 1, nrow
      do j = 1, ncol
        write(tmp,'(ES15.6)') S%data(i,j)
        if (j < ncol) then
          write(unit,'(A)',advance='no') trim(tmp)//','
        else
          write(unit,'(A)') trim(tmp)
        end if
      end do
    end do

    close(unit)
    print*, 'Std‑output file written to ', full_path
  end subroutine write_std_output

!-----------------------------------------------------------------------
! Printing Tools - used for testing purposes.

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