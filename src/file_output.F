module file_output

    use netcdf
    use remapper, only : target_field_type
    use scan_input, only : FIELD_TYPE_REAL, FIELD_TYPE_DOUBLE, FIELD_TYPE_INTEGER, FIELD_TYPE_CHARACTER

    type output_handle_type
        integer :: ncid
        logical :: inDefineMode = .true.
    end type output_handle_type


    contains


    integer function file_output_open(filename, handle) result(stat)

        implicit none

        character (len=*), intent(in) :: filename
        type (output_handle_type), intent(out) :: handle

        stat = 0

        stat = nf90_create(trim(filename), NF90_64BIT_OFFSET, handle % ncid)
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

        integer :: varid
        integer, dimension(5) :: dimids

        stat = 0

        if (.not. handle % inDefineMode) then
            stat = 1
            return
        end if

        !
        ! NEED SOME LOGIC HERE TO FIGURE OUT WHICH DIMENSIONS HAVE ALREADY BEEN DEFINED, TO DEFINE
        ! THOSE THAT HAVE NOT, AND TO BUILD A LIST OF DIMIDS...
        !

        if (field % xtype == FIELD_TYPE_REAL) then
            if (field % ndims == 0 .or. (field % ndims == 1 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:2), varid)
                end if
            else if (field % ndims == 1 .or. (field % ndims == 2 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:2), varid)
                end if
            else if (field % ndims == 2 .or. (field % ndims == 3 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:2), varid)
                end if
            else if (field % ndims == 3 .or. (field % ndims == 4 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:2), varid)
                end if
            else if (field % ndims == 4 .or. (field % ndims == 5 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_FLOAT, dimids(1:2), varid)
                end if
            end if
        else if (field % xtype == FIELD_TYPE_DOUBLE) then
            if (field % ndims == 0 .or. (field % ndims == 1 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:2), varid)
                end if
            else if (field % ndims == 1 .or. (field % ndims == 2 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:2), varid)
                end if
            else if (field % ndims == 2 .or. (field % ndims == 3 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:2), varid)
                end if
            else if (field % ndims == 3 .or. (field % ndims == 4 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:2), varid)
                end if
            else if (field % ndims == 4 .or. (field % ndims == 5 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_DOUBLE, dimids(1:2), varid)
                end if
            end if

        else if (field % xtype == FIELD_TYPE_INTEGER) then
            if (field % ndims == 0 .or. (field % ndims == 1 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:2), varid)
                end if
            else if (field % ndims == 1 .or. (field % ndims == 2 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:2), varid)
                end if
            else if (field % ndims == 2 .or. (field % ndims == 3 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:2), varid)
                end if
            else if (field % ndims == 3 .or. (field % ndims == 4 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:2), varid)
                else
                    stat = nf90_def_var(handle % ncid, trim(field % name), NF90_INT, dimids(1:2), varid)
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


    integer function file_output_write_field(handle, field) result(stat)

        implicit none

        type (output_handle_type), intent(inout) :: handle
        type (target_field_type), intent(inout) :: field

        integer :: varid

        stat = 0

        if (handle % inDefineMode) then
            stat = nf90_enddef(handle % ncid)
            if (stat /= NF90_NOERR) then
                stat = 1
                return
            end if
        end if

        stat = nf90_inq_varid(handle % ncid, trim(field % name), varid)
#if 0

        ierr = nf90_inquire_variable(ncid_in, var_in, xtype=vartype)
        ierr = nf90_inquire_variable(ncid_in, var_in, ndims=varndims)
        ierr = nf90_inquire_variable(ncid_in, var_in, dimids=vardimids)
        do i=1,varndims
            ierr = nf90_inquire_dimension(ncid_in, vardimids(i), len=vardimlens(i))
        end do
        ierr = nf90_inquire_dimension(ncid_in, vardimids(varndims), name=dimname)

        if (field % xtype == FIELD_TYPE_REAL) then
            if (field % ndims == 0 .or. (field % ndims == 1 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    start(1) = local_frame
                    count(1) = 1
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, temp1r, &
                                        start=start(1:1), count=count(1:1))
                    field % array0r = temp1r(1)
                else
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array0r)
                end if
            else if (field % ndims == 1 .or. (field % ndims == 2 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = local_frame
                    count(2) = 1
                    allocate(field % array1r(count(1)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array1r, &
                                        start=start(1:2), count=count(1:2))
                else
                    allocate(field % array1r(field%dimlens(1)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array1r)
                end if
            else if (field % ndims == 2 .or. (field % ndims == 3 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = 1
                    count(2) = field % dimlens(2)
                    start(3) = local_frame
                    count(3) = 1
                    allocate(field % array2r(count(1),count(2)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array2r, &
                                        start=start(1:3), count=count(1:3))
                else
                    allocate(field % array2r(field%dimlens(1),field%dimlens(2)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array2r)
                end if
            else if (field % ndims == 3 .or. (field % ndims == 4 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = 1
                    count(2) = field % dimlens(2)
                    start(3) = 1
                    count(3) = field % dimlens(3)
                    start(4) = local_frame
                    count(4) = 1
                    allocate(field % array3r(count(1),count(2),count(3)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array3r, &
                                        start=start(1:4), count=count(1:4))
                else
                    allocate(field % array3r(field%dimlens(1),field%dimlens(2),field%dimlens(3)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array3r)
                end if
            else if (field % ndims == 4 .or. (field % ndims == 5 .and. field % isTimeDependent)) then
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
                    allocate(field % array4r(count(1),count(2),count(3),count(4)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array4r, &
                                        start=start(1:5), count=count(1:5))
                else
                    allocate(field % array4r(field%dimlens(1),field%dimlens(2),field%dimlens(3),field%dimlens(4)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array4r)
                end if
            end if
        else if (field % xtype == FIELD_TYPE_DOUBLE) then
            if (field % ndims == 0 .or. (field % ndims == 1 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    start(1) = local_frame
                    count(1) = 1
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, temp1d, &
                                        start=start(1:1), count=count(1:1))
                    field % array0d = temp1d(1)
                else
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array0d)
                end if
            else if (field % ndims == 1 .or. (field % ndims == 2 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = local_frame
                    count(2) = 1
                    allocate(field % array1d(count(1)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array1d, &
                                        start=start(1:2), count=count(1:2))
                else
                    allocate(field % array1d(field%dimlens(1)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array1d)
                end if
            else if (field % ndims == 2 .or. (field % ndims == 3 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = 1
                    count(2) = field % dimlens(2)
                    start(3) = local_frame
                    count(3) = 1
                    allocate(field % array2d(count(1),count(2)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array2d, &
                                        start=start(1:3), count=count(1:3))
                else
                    allocate(field % array2d(field%dimlens(1),field%dimlens(2)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array2d)
                end if
            else if (field % ndims == 3 .or. (field % ndims == 4 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = 1
                    count(2) = field % dimlens(2)
                    start(3) = 1
                    count(3) = field % dimlens(3)
                    start(4) = local_frame
                    count(4) = 1
                    allocate(field % array3d(count(1),count(2),count(3)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array3d, &
                                        start=start(1:4), count=count(1:4))
                else
                    allocate(field % array3d(field%dimlens(1),field%dimlens(2),field%dimlens(3)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array3d)
                end if
            else if (field % ndims == 4 .or. (field % ndims == 5 .and. field % isTimeDependent)) then
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
                    allocate(field % array4d(count(1),count(2),count(3),count(4)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array4d, &
                                        start=start(1:5), count=count(1:5))
                else
                    allocate(field % array4d(field%dimlens(1),field%dimlens(2),field%dimlens(3),field%dimlens(4)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array4d)
                end if
            end if

        else if (field % xtype == FIELD_TYPE_INTEGER) then
            if (field % ndims == 0 .or. (field % ndims == 1 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    start(1) = local_frame
                    count(1) = 1
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, temp1i, &
                                        start=start(1:1), count=count(1:1))
                    field % array0i = temp1i(1)
                else
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array0i)
                end if
            else if (field % ndims == 1 .or. (field % ndims == 2 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = local_frame
                    count(2) = 1
                    allocate(field % array1i(count(1)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array1i, &
                                        start=start(1:2), count=count(1:2))
                else
                    allocate(field % array1i(field%dimlens(1)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array1i)
                end if
            else if (field % ndims == 2 .or. (field % ndims == 3 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = 1
                    count(2) = field % dimlens(2)
                    start(3) = local_frame
                    count(3) = 1
                    allocate(field % array2i(count(1),count(2)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array2i, &
                                        start=start(1:3), count=count(1:3))
                else
                    allocate(field % array2i(field%dimlens(1),field%dimlens(2)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array2i)
                end if
            else if (field % ndims == 3 .or. (field % ndims == 4 .and. field % isTimeDependent)) then
                if (field % isTimeDependent) then
                    start(1) = 1
                    count(1) = field % dimlens(1)
                    start(2) = 1
                    count(2) = field % dimlens(2)
                    start(3) = 1
                    count(3) = field % dimlens(3)
                    start(4) = local_frame
                    count(4) = 1
                    allocate(field % array3i(count(1),count(2),count(3)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array3i, &
                                        start=start(1:4), count=count(1:4))
                else
                    allocate(field % array3i(field%dimlens(1),field%dimlens(2),field%dimlens(3)))
                    stat = nf90_put_var(field % file_handle % ncid, field % varid, field % array3i)
                end if
            end if

        end if
#endif

    end function file_output_write_field

end module file_output
