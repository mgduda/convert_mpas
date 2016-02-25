program convert_mpas

    use scan_input
    use mpas_mesh
    use target_mesh
    use remapper

    implicit none

   
    integer :: stat
    character (len=1024) :: filename
    type (mpas_mesh_type) :: source_mesh
    type (target_mesh_type) :: destination_mesh
    type (input_handle_type) :: handle
    type (input_field_type) :: field
    type (remap_info_type) :: remap_info


    if (command_argument_count() < 1) then
        write(0,*) ' '
        write(0,*) 'Usage: convert_mpas [options]'
        write(0,*) ' '
        stop 1
    end if

    call get_command_argument(1, filename)

    if (mpas_mesh_setup(filename, source_mesh) /= 0) then
        write(0,*) 'Error: Problems setting up MPAS mesh from file '//trim(filename)
        stop 2
    end if

    if (target_mesh_setup(destination_mesh) /= 0) then
        write(0,*) 'Error: Problems setting up target mesh'
        stop 3
    end if

    if (remap_info_setup(source_mesh, destination_mesh, remap_info) /= 0) then
        write(0,*) 'Error: Problems setting up remapping'
        stop 4
    end if

    stat = scan_input_open(filename, handle)

    do while (scan_input_next_field(handle, field) == 0) 
       write(0,*) 'Field name = ', trim(field % name)
       write(0,*) '   ', field % isTimeDependent
       stat = scan_input_free_field(field)
    end do

write(0,*) 'Checking for xCell'
    if (scan_input_for_field(handle, 'xCell', field) /= 0) then
        write(0,*) 'Scan for xCell failed...'
    else
        write(0,*) 'success: '//trim(field % name)
        stat = scan_input_read_field(field)
        if (stat /= 0) then
            write(0,*) 'Error while reading xCell'
        else
write(0,*) 'eh? ', size(field % array1r)
            write(0,*) field % array1r(1:10)
            write(0,*) field % array1r(field % dimlens(1)-10:field % dimlens(1))
        end if
    end if
write(0,*) 'Freeing field'
    stat = scan_input_free_field(field)

    stat = scan_input_close(handle)

    stat = mpas_mesh_free(source_mesh)

    stat = target_mesh_free(destination_mesh)

    stat = remap_info_free(remap_info)

    stop

end program convert_mpas
