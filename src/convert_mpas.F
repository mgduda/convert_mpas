program convert_mpas

    use scan_input
    use mpas_mesh
    use target_mesh
    use remapper
    use file_output

    implicit none

   
    integer :: stat
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

    if (target_mesh_setup(destination_mesh, nlat=180, nlon=360) /= 0) then
        write(0,*) 'Error: Problems setting up target mesh'
        stop 3
    end if

    if (remap_info_setup(source_mesh, destination_mesh, remap_info) /= 0) then
        write(0,*) 'Error: Problems setting up remapping'
        stop 4
    end if

    if (scan_input_open(filename, handle) /= 0) then
        write(0,*) 'Error: Problems opening input file'
        stop 5
    end if

    if (file_output_open('blah.nc', output_handle) /= 0) then
        write(0,*) 'Error: Problems opening output file'
        stop 6
    end if

    do while (scan_input_next_field(handle, field) == 0) 
        if (can_remap_field(field)) then
            write(0,*) 'Remapping field '//trim(field % name)
            stat = scan_input_read_field(field)    ! fix
            stat = remap_field(remap_info, field, target_field)    ! fix
            stat = file_output_register_field(output_handle, target_field)

            stat = free_target_field(target_field)
        end if
        stat = scan_input_free_field(field)
    end do

    stat = scan_input_rewind(handle)

    do while (scan_input_next_field(handle, field) == 0) 
        if (can_remap_field(field)) then
            write(0,*) 'Actually remapping field '//trim(field % name)
            stat = scan_input_read_field(field)
            stat = remap_field(remap_info, field, target_field)
            stat = file_output_write_field(output_handle, target_field)

            stat = free_target_field(target_field)
        end if
        stat = scan_input_free_field(field)
    end do

    stat = scan_input_close(handle)
    stat = file_output_close(output_handle)

    stat = mpas_mesh_free(source_mesh)
    stat = target_mesh_free(destination_mesh)
    stat = remap_info_free(remap_info)


    stop

end program convert_mpas
