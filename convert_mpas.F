program convert_mpas

    use scan_input

    implicit none

   
    integer :: stat
    character (len=1024) :: filename
    type (input_handle_type) :: handle
    type (input_field_type) :: field


    if (command_argument_count() < 1) then
        write(0,*) ' '
        write(0,*) 'Usage: convert_mpas [options]'
        write(0,*) ' '
        stop 1
    end if

    call get_command_argument(1, filename)

    stat = scan_input_open(filename, handle)

    do while (scan_input_next_field(handle, field) == 0) 
       write(0,*) 'Field name = ', trim(field % name)
       write(0,*) '   ', field % isTimeDependent
       stat = scan_input_free_field(field)
    end do

write(0,*) 'Checking for cellsOnCell'
    if (scan_input_for_field(handle, 'cellsOnCell', field) /= 0) then
        write(0,*) 'Scan for cellsOnCell failed...'
    else
        write(0,*) 'success: '//trim(field % name)
    end if
    stat = scan_input_free_field(field)

    stat = scan_input_close(handle)

end program convert_mpas
