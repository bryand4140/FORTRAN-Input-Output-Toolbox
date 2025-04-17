module IO_Toolbox
    use MOD_Select_Precision, only: pv
    implicit none

    private !Set all subroutines to private by default

    public :: write_matrix, read_matrix
    public :: write_labeled_matrix
    public :: prv, piv, prm
    public :: read_labeled_matrix
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


subroutine read_matrix(matrix, filename, path)
    ! Reads a CSV file into a matrix, auto-detecting path format.

    implicit none

    ! Input/Output arguments
    character(len=*), intent(in)          :: filename
    real(pv), allocatable, intent(out)    :: matrix(:,:)
    character(len=*), intent(in), optional :: path

    ! Local variables
    character(len=:), allocatable :: p
    logical              :: is_wsl
    character(len=1)     :: sep
    character(len=256)   :: full_path
    integer              :: unit, ios, nrows, ncols, i, j, pos
    character(len=1000)  :: line
    character(len=15)    :: token
    real(pv), allocatable :: temp_matrix(:,:)

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

    ! Open the file for reading
    open(newunit=unit, file=full_path, status='old', action='read', &
         form='formatted', iostat=ios)
    if (ios /= 0) then
        print*, 'Error: Could not open file ', trim(full_path)
        stop
    end if

    ! Determine number of rows and columns
    nrows = 0
    ncols = 0
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (nrows == 0) then
            ncols = 1
            do pos = 1, len_trim(line)
                if (line(pos:pos) == ',') ncols = ncols + 1
            end do
        end if
        nrows = nrows + 1
    end do

    ! Allocate matrix
    allocate(temp_matrix(nrows, ncols))

    ! Rewind to beginning
    rewind(unit)

    ! Read data into matrix
    i = 1
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        pos = 1
        do j = 1, ncols
            call get_token(line, pos, token)
            read(token, *) temp_matrix(i, j)
        end do
        i = i + 1
    end do

    close(unit)
    matrix = temp_matrix

    contains

    subroutine get_token(input_line, input_pos, output_token)
        character(len=*), intent(in) :: input_line
        integer, intent(inout)       :: input_pos
        character(len=15), intent(out):: output_token
        integer                     :: start, lenend

        start = input_pos
        lenend = index(input_line(start:), ',') - 1
        if (lenend < 0) lenend = len_trim(input_line) - start + 1
        output_token = input_line(start:start+lenend-1)
        input_pos = start + lenend + 1
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


subroutine read_labeled_matrix(matrix, column_labels, filename, path)
    ! Reads a labeled CSV file into a matrix, auto-detecting path format.

    implicit none

    ! Input/Output arguments
    character(len=*), intent(in) :: filename
    real(pv), allocatable, intent(out) :: matrix(:,:)
    character(len=*), allocatable, intent(out) :: column_labels(:)
    character(len=*), intent(in), optional :: path

    ! Local variables
    character(len=:), allocatable :: p
    logical :: is_wsl
    character(len=1) :: sep
    character(len=256) :: full_path
    integer :: unit, ios, i, j, nrows, ncols, pos
    character(len=1000) :: line
    character(len=15) :: token
    real(pv), allocatable :: temp_matrix(:,:)

    ! Construct full file path with proper separator
    if (present(path)) then
        p = trim(path)
        is_wsl = detect_wsl_path(p)
        if (is_wsl) then
            sep = '/'
        else
            sep = '\\'
        end if
        if (p(len_trim(p):len_trim(p)) == sep) then
            full_path = p // trim(filename)
        else
            full_path = p // sep // trim(filename)
        end if
    else
        full_path = trim(filename)
    end if

    ! Open the file for reading
    open(newunit=unit, file=full_path, status='old', action='read', &
         form='formatted', iostat=ios)
    if (ios /= 0) then
        print*, 'Error: Could not open file ', trim(full_path)
        stop
    end if

    ! Read header line for column labels
    read(unit, '(A)', iostat=ios) line
    if (ios /= 0) then
        print*, 'Error: Failed to read column labels from file.'
        stop
    end if

    ! Determine number of columns from header
    ncols = 1
    do pos = 1, len_trim(line)
        if (line(pos:pos) == ',') ncols = ncols + 1
    end do

    ! Allocate and extract column labels
    allocate(column_labels(ncols))
    pos = 1
    do j = 1, ncols
        call get_token(line, pos, column_labels(j))
    end do

    ! Remove non-printable characters from the first label (fix BOM issue)
    column_labels(1) = adjustl(strip_nonprintable(column_labels(1)))

    ! Count data rows (excluding header)
    nrows = 0
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        nrows = nrows + 1
    end do

    ! Allocate temporary matrix and rewind to data
    allocate(temp_matrix(nrows, ncols))
    rewind(unit)
    read(unit, '(A)') line  ! skip header

    ! Read data into matrix
    i = 1
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        if (len_trim(line) == 0) cycle
        pos = 1
        do j = 1, ncols
            call get_token(line, pos, token)
            if (len_trim(token) == 0) then
                temp_matrix(i, j) = 0.0_pv
            else
                read(token, *) temp_matrix(i, j)
            end if
        end do
        i = i + 1
    end do

    close(unit)
    matrix = temp_matrix

    contains

    subroutine get_token(input_line, input_pos, output_token)
        character(len=*), intent(in) :: input_line
        integer, intent(inout) :: input_pos
        character(len=15), intent(out) :: output_token
        integer :: start, lenend

        start = input_pos
        lenend = index(input_line(start:), ',') - 1
        if (lenend < 0) lenend = len_trim(input_line) - start + 1
        output_token = input_line(start:start+lenend-1)
        input_pos = start + lenend + 1
    end subroutine get_token

end subroutine read_labeled_matrix


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