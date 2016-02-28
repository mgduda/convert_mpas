program convert_mpas

    use scan_input
    use mpas_mesh
    use target_mesh
    use remapper
    use file_output
    use field_list

    implicit none

   
    integer :: stat
    character (len=1024) :: mesh_filename, data_filename
    type (mpas_mesh_type) :: source_mesh
    type (target_mesh_type) :: destination_mesh
    type (input_handle_type) :: handle
    type (input_field_type) :: field
    type (remap_info_type) :: remap_info
    type (output_handle_type) :: output_handle
    type (target_field_type) :: target_field
    type (field_list_type) :: include_field_list, exclude_field_list


    if (command_argument_count() < 1) then
        write(0,*) ' '
        write(0,*) 'Usage: convert_mpas [mesh-file] data-file'
        write(0,*) ' '
        write(0,*) 'If only one file argument is given, both the MPAS mesh information and'
        write(0,*) 'the fields will be read from the specified file.'
        write(0,*) 'If two file arguments are given, the MPAS mesh information will be read'
        write(0,*) 'from the first file and fields to be remapped will be read from'
        write(0,*) 'the second file.'
        stop 1
    end if

    if (command_argument_count() == 1) then
        call get_command_argument(1, mesh_filename)
        call get_command_argument(1, data_filename)
    else if (command_argument_count() == 2) then
        call get_command_argument(1, mesh_filename)
        call get_command_argument(2, data_filename)
    end if

    write(0,*) 'Reading MPAS mesh information from file '''//trim(mesh_filename)//''''
    write(0,*) 'Reading MPAS fields from file '''//trim(data_filename)//''''

    if (target_mesh_setup(destination_mesh) /= 0) then
        write(0,*) 'Error: Problems setting up target mesh'
        stop 2
    end if

    if (mpas_mesh_setup(mesh_filename, source_mesh) /= 0) then
        write(0,*) 'Error: Problems setting up MPAS mesh from file '//trim(mesh_filename)
        stop 3
    end if

    if (remap_info_setup(source_mesh, destination_mesh, remap_info) /= 0) then
        write(0,*) 'Error: Problems setting up remapping'
        stop 4
    end if

    if (scan_input_open(data_filename, handle) /= 0) then
        write(0,*) 'Error: Problems opening input file'
        stop 5
    end if

    if (file_output_open('latlon.nc', output_handle) /= 0) then
        write(0,*) 'Error: Problems opening output file'
        stop 6
    end if

    stat = field_list_init(include_field_list, exclude_field_list)

    do while (scan_input_next_field(handle, field) == 0) 
        if (can_remap_field(field) .and. &
            should_remap_field(field, include_field_list, exclude_field_list)) then
            write(0,*) 'Remapping field '//trim(field % name)
            stat = remap_field_dryrun(remap_info, field, target_field)
            stat = file_output_register_field(output_handle, target_field)

            stat = free_target_field(target_field)
        end if
        stat = scan_input_free_field(field)
    end do

    stat = scan_input_rewind(handle)

    do while (scan_input_next_field(handle, field) == 0) 
        if (can_remap_field(field) .and. &
            should_remap_field(field, include_field_list, exclude_field_list)) then
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
    stat = field_list_finalize(include_field_list, exclude_field_list)

    stop

end program convert_mpas
