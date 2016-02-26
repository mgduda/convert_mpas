program convert_mpas

    use scan_input
    use mpas_mesh
    use target_mesh
    use remapper
    use file_output

    implicit none

   
    integer :: stat
    integer :: idim
    character (len=1024) :: filename
    type (mpas_mesh_type) :: source_mesh
    type (target_mesh_type) :: destination_mesh
    type (input_handle_type) :: handle
    type (input_field_type) :: field
    type (remap_info_type) :: remap_info
    type (output_handle_type) :: output_handle
    type (target_field_type) :: target_field


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
       write(0,*) '   can remap? ', can_remap_field(field)
       write(0,*) '   ndims=', field % ndims
       do idim=1,field % ndims
           write(0,*) '         ', trim(field % dimnames(idim))
       end do
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

    stat = scan_input_close(handle)


    stat = file_output_open('blah.nc', output_handle)

    stat = remap_field(remap_info, field, target_field)
write(0,*) 'remap status=', stat
    stat = file_output_register_field(output_handle, target_field)

    stat = file_output_write_field(output_handle, target_field)


    stat = mpas_mesh_free(source_mesh)
    stat = target_mesh_free(destination_mesh)
    stat = scan_input_free_field(field)
    stat = free_target_field(target_field)
    stat = remap_info_free(remap_info)

    stat = file_output_close(output_handle)

    stop

end program convert_mpas
