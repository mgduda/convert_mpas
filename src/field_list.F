module field_list

    use scan_input, only : input_field_type

    type field_list_type
        integer :: num_fields = 0
        character (len=64), dimension(:), pointer :: fieldNames => null()
    end type field_list_type


    contains


    integer function field_list_init(include_list, exclude_list) result(stat)

        implicit none

        type (field_list_type), intent(out) :: include_list
        type (field_list_type), intent(out) :: exclude_list

        logical :: exists
        integer :: nlines
        integer :: i, j
        integer :: iostatus


        stat = 0

        write(0,*) ' '
        inquire(file='include_fields', exist=exists)
        if (exists) then
            write(0,*) 'Reading list of fields to include in remapped output'
            open(21, file='include_fields', form='formatted')
            nlines = 0
            read(21,fmt=*,iostat=iostatus)
            do while (iostatus >= 0) 
                nlines = nlines + 1
                read(21,fmt=*,iostat=iostatus)
            end do
            rewind(21)
            write(0,*) 'Found ', nlines, ' lines in ''include_fields'' file'
            include_list % num_fields = nlines
            allocate(include_list % fieldNames(include_list % num_fields))
            i = 1
            do j=1,nlines
                read(21,fmt=*,iostat=iostatus) include_list % fieldNames(i)
                if (iostatus == 0) then
                    i = i + 1
                else
                    write(0,*) 'Error while reading line ', j, ' of file ''include_fields'''
                    include_list % num_fields = include_list % num_fields - 1
                end if
            end do
            close(21)
        else
            write(0,*) 'List of fields to be included in output (''include_fields'') not found'
        end if


        inquire(file='exclude_fields', exist=exists)
        if (exists) then
            write(0,*) 'Reading list of fields to exclude in remapped output'
            open(21, file='exclude_fields', form='formatted')
            nlines = 0
            read(21,fmt=*,iostat=iostatus)
            do while (iostatus >= 0) 
                nlines = nlines + 1
                read(21,fmt=*,iostat=iostatus)
            end do
            rewind(21)
            write(0,*) 'Found ', nlines, ' lines in ''exclude_fields'' file'
            exclude_list % num_fields = nlines
            allocate(exclude_list % fieldNames(exclude_list % num_fields))
            i = 1
            do j=1,nlines
                read(21,fmt=*,iostat=iostatus) exclude_list % fieldNames(i)
                if (iostatus == 0) then
                    i = i + 1
                else
                    write(0,*) 'Error while reading line ', j, ' of file ''exclude_fields'''
                    exclude_list % num_fields = exclude_list % num_fields - 1
                end if
            end do
            close(21)
        else
            write(0,*) 'List of fields to be excluded from output (''exclude_fields'') not found'
        end if

    end function field_list_init


    integer function field_list_finalize(include_list, exclude_list) result(stat)

        implicit none

        type (field_list_type), intent(inout) :: include_list
        type (field_list_type), intent(inout) :: exclude_list

        stat = 0

        include_list % num_fields = 0
        exclude_list % num_fields = 0

        if (associated(include_list % fieldNames)) then
            deallocate(include_list % fieldNames)
        end if
        if (associated(exclude_list % fieldNames)) then
            deallocate(exclude_list % fieldNames)
        end if

    end function field_list_finalize


    logical function should_remap_field(field, include_list, exclude_list)

        implicit none

        type (field_list_type), intent(in) :: include_list
        type (field_list_type), intent(in) :: exclude_list
        type (input_field_type), intent(in) :: field

        integer :: j


        should_remap_field = .true.

        !
        ! If no lists provided, we should remap every field
        !
        if (include_list % num_fields == 0 .and. &
            exclude_list % num_fields == 0) then
            return
        end if

        !
        ! If an include list was provided, look for the field name there
        !
        if (include_list % num_fields /= 0) then

            do j=1,include_list % num_fields
                if (trim(include_list % fieldNames(j)) == trim(field % name)) then
                    return
                end if
            end do
            should_remap_field = .false.

        !
        ! Otherwise, check whether we should NOT interpolate the field
        !
        else if (exclude_list % num_fields /= 0) then

            do j=1,exclude_list % num_fields
                if (trim(exclude_list % fieldNames(j)) == trim(field % name)) then
                    should_remap_field = .false.
                    return
                end if
            end do

        end if

    end function should_remap_field

end module field_list
