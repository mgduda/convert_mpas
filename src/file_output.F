module file_output

    use netcdf
    use remapper, only : target_field_type
    use scan_input, only : FIELD_TYPE_REAL, FIELD_TYPE_DOUBLE, FIELD_TYPE_INTEGER, FIELD_TYPE_CHARACTER

    type output_handle_type
        integer :: ncid
        integer :: unlimitedID
        logical :: inDefineMode = .true.
    end type output_handle_type


    contains


    integer function file_output_open(filename, handle) result(stat)

        implicit none

        character (len=*), intent(in) :: filename
        type (output_handle_type), intent(out) :: handle

        stat = 0

        stat = nf90_create(trim(filename), NF90_NETCDF4, handle % ncid)
        if (stat /= NF90_NOERR) then
            stat = 1
            return
        end if

        handle % inDefineMode = .true.

        stat = nf90_def_dim(handle % ncid, 'Time', NF90_UNLIMITED, handle % unlimitedId)
        if (stat /= NF90_NOERR) then
            stat = 1
            return
        end if

    end function file_output_open


    integer function file_output_close(handle) result(stat)

        implicit none

        type (output_handle_type), intent(inout) :: handle

        stat = 0

        stat = nf90_close(handle % ncid)
        if (stat /= NF90_NOERR) then
            stat = 1
            return
        end if

        handle % inDefineMode = .true.

    end function file_output_close


    integer function file_output_register_field(handle, field) result(stat)

        implicit none

        type (output_handle_type), intent(inout) :: handle
        type (target_field_type), intent(in) :: field

        integer :: idim
        integer :: varid
        integer, dimension(5) :: dimids

        stat = 0

        if (.not. handle % inDefineMode) then
            stat = 1
            return
        end if

        do idim=1,field % ndims
            stat = nf90_inq_dimid(handle % ncid, trim(field % dimnames(idim)), dimids(idim))
            if (stat /= NF90_NOERR) then
                stat = nf90_def_dim(handle % ncid, trim(field % dimnames(idim)), field % dimlens(idim), dimids(idim))
            end if
        end do
        if (field % isTimeDependent) then
            dimids(idim) = handle % unlimitedID
        end if

        if (field % xtype == FIELD_TYPE_REAL) then
            if (field % ndims == 0) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:1), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:0), varid)
                end if
            else if (field % ndims == 1) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:1), varid)
                end if
            else if (field % ndims == 2) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:3), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:2), varid)
                end if
            else if (field % ndims == 3) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:4), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:3), varid)
                end if
            else if (field % ndims == 4) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:5), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:4), varid)
                end if
            end if
        else if (field % xtype == FIELD_TYPE_DOUBLE) then
            if (field % ndims == 0) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:1), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:0), varid)
                end if
            else if (field % ndims == 1) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:1), varid)
                end if
            else if (field % ndims == 2) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:3), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:2), varid)
                end if
            else if (field % ndims == 3) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:4), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:3), varid)
                end if
            else if (field % ndims == 4) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:5), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:4), varid)
                end if
            end if

        else if (field % xtype == FIELD_TYPE_INTEGER) then
            if (field % ndims == 0) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:1), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:0), varid)
                end if
            else if (field % ndims == 1) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:1), varid)
                end if
            else if (field % ndims == 2) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:3), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:2), varid)
                end if
            else if (field % ndims == 3) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:4), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:3), varid)
                end if
            end if

        else if (field % xtype == FIELD_TYPE_CHARACTER) then
            write(0,*) ' '
            write(0,*) 'Processing of character fields is not supported; skipping write of field '//trim(field % name)
            write(0,*) ' '

        else
            write(0,*) ' '
            write(0,*) 'Unsupported type; skipping write of field '//trim(field % name)
            write(0,*) ' '
        end if

    end function file_output_register_field


    integer function file_output_write_field(handle, field, frame) result(stat)

        implicit none

        type (output_handle_type), intent(inout) :: handle
        type (target_field_type), intent(inout) :: field
        integer, intent(in), optional :: frame

        integer :: varid
        integer :: local_frame
        integer, dimension(5) :: start
        integer, dimension(5) :: count

        stat = 0

        local_frame = 1
        if (present(frame)) then
            local_frame = frame
        end if

        if (handle % inDefineMode) then
            stat = nf90_enddef(handle % ncid)
            if (stat /= NF90_NOERR) then
                stat = 1
                return
            end if
            handle % inDefineMode = .false.
        end if

        stat = nf90_inq_varid(handle % ncid, trim(field % name), varid)
        if (stat /= NF90_NOERR) then
            stat = 1
            return
        end if

        if (field % xtype == FIELD_TYPE_REAL) then
            if (field % ndims == 0) then
                if (field % isTimeDependent) then
                    start(1) = local_frame
                    count(1) = 1
                    stat = nf90_put_var(handle % ncid, varid, (/field % array0r/), &
                                        start=start(1:1), count=count(1:1))
                else
                    stat = nf90_put_var(handle % ncid, varid, field % array0r)
                end if
            else if (field % ndims == 1) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = local_frame
                    count(2) = 1
                    stat = nf90_put_var(handle % ncid, varid, field % array1r, &
                                        start=start(1:2), count=count(1:2))
                else
                    stat = nf90_put_var(handle % ncid, varid, field % array1r)
                end if
            else if (field % ndims == 2) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = 1
                    count(2) = field % dimlens(2)
                    start(3) = local_frame
                    count(3) = 1
                    stat = nf90_put_var(handle % ncid, varid, field % array2r, &
                                        start=start(1:3), count=count(1:3))
                else
                    stat = nf90_put_var(handle % ncid, varid, field % array2r)
                end if
            else if (field % ndims == 3) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = 1
                    count(2) = field % dimlens(2)
                    start(3) = 1
                    count(3) = field % dimlens(3)
                    start(4) = local_frame
                    count(4) = 1
                    stat = nf90_put_var(handle % ncid, varid, field % array3r, &
                                        start=start(1:4), count=count(1:4))
                else
                    stat = nf90_put_var(handle % ncid, varid, field % array3r)
                end if
            else if (field % ndims == 4) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = 1
                    count(2) = field % dimlens(2)
                    start(3) = 1
                    count(3) = field % dimlens(3)
                    start(4) = 1
                    count(4) = field % dimlens(4)
                    start(5) = local_frame
                    count(5) = 1
                    stat = nf90_put_var(handle % ncid, varid, field % array4r, &
                                        start=start(1:5), count=count(1:5))
                else
                    stat = nf90_put_var(handle % ncid, varid, field % array4r)
                end if
            end if
        else if (field % xtype == FIELD_TYPE_DOUBLE) then
            if (field % ndims == 0) then
                if (field % isTimeDependent) then
                    start(1) = local_frame
                    count(1) = 1
                    stat = nf90_put_var(handle % ncid, varid, (/field % array0d/), &
                                        start=start(1:1), count=count(1:1))
                else
                    stat = nf90_put_var(handle % ncid, varid, field % array0d)
                end if
            else if (field % ndims == 1) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = local_frame
                    count(2) = 1
                    stat = nf90_put_var(handle % ncid, varid, field % array1d, &
                                        start=start(1:2), count=count(1:2))
                else
                    stat = nf90_put_var(handle % ncid, varid, field % array1d)
                end if
            else if (field % ndims == 2) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = 1
                    count(2) = field % dimlens(2)
                    start(3) = local_frame
                    count(3) = 1
                    stat = nf90_put_var(handle % ncid, varid, field % array2d, &
                                        start=start(1:3), count=count(1:3))
                else
                    stat = nf90_put_var(handle % ncid, varid, field % array2d)
                end if
            else if (field % ndims == 3) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = 1
                    count(2) = field % dimlens(2)
                    start(3) = 1
                    count(3) = field % dimlens(3)
                    start(4) = local_frame
                    count(4) = 1
                    stat = nf90_put_var(handle % ncid, varid, field % array3d, &
                                        start=start(1:4), count=count(1:4))
                else
                    stat = nf90_put_var(handle % ncid, varid, field % array3d)
                end if
            else if (field % ndims == 4) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = 1
                    count(2) = field % dimlens(2)
                    start(3) = 1
                    count(3) = field % dimlens(3)
                    start(4) = 1
                    count(4) = field % dimlens(4)
                    start(5) = local_frame
                    count(5) = 1
                    stat = nf90_put_var(handle % ncid, varid, field % array4d, &
                                        start=start(1:5), count=count(1:5))
                else
                    stat = nf90_put_var(handle % ncid, varid, field % array4d)
                end if
            end if

        else if (field % xtype == FIELD_TYPE_INTEGER) then
            if (field % ndims == 0) then
                if (field % isTimeDependent) then
                    start(1) = local_frame
                    count(1) = 1
                    stat = nf90_put_var(handle % ncid, varid, (/field % array0i/), &
                                        start=start(1:1), count=count(1:1))
                else
                    stat = nf90_put_var(handle % ncid, varid, field % array0i)
                end if
            else if (field % ndims == 1) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = local_frame
                    count(2) = 1
                    stat = nf90_put_var(handle % ncid, varid, field % array1i, &
                                        start=start(1:2), count=count(1:2))
                else
                    stat = nf90_put_var(handle % ncid, varid, field % array1i)
                end if
            else if (field % ndims == 2) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = 1
                    count(2) = field % dimlens(2)
                    start(3) = local_frame
                    count(3) = 1
                    stat = nf90_put_var(handle % ncid, varid, field % array2i, &
                                        start=start(1:3), count=count(1:3))
                else
                    stat = nf90_put_var(handle % ncid, varid, field % array2i)
                end if
            else if (field % ndims == 3) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = 1
                    count(2) = field % dimlens(2)
                    start(3) = 1
                    count(3) = field % dimlens(3)
                    start(4) = local_frame
                    count(4) = 1
                    stat = nf90_put_var(handle % ncid, varid, field % array3i, &
                                        start=start(1:4), count=count(1:4))
                else
                    stat = nf90_put_var(handle % ncid, varid, field % array3i)
                end if
            end if

        end if

    end function file_output_write_field

end module file_output
