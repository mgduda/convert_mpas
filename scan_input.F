module scan_input

    use netcdf

    type input_handle_type
        integer :: ncid
        integer :: num_vars = 0
        integer :: current_var = 0
        integer, dimension(:), pointer :: varids => null()
        integer :: unlimited_dimid
    end type input_handle_type

    type input_field_type
        character (len=64) :: name
        logical :: isTimeDependent = .false.
        integer :: varid = -1
        integer :: xtype = -1
        integer :: ndims = -1
        character (len=64), dimension(:), pointer :: dimnames
        integer, dimension(:), pointer :: dimlens
        integer, dimension(:), pointer :: dimids
    end type input_field_type

    integer, parameter :: FIELD_TYPE_REAL      = 1, &
                          FIELD_TYPE_DOUBLE    = 2, &
                          FIELD_TYPE_INTEGER   = 3, &
                          FIELD_TYPE_CHARACTER = 4


    contains


    integer function scan_input_open(filename, handle) result(stat)

        implicit none

        character (len=*), intent(in) :: filename
        type (input_handle_type), intent(out) :: handle

 
        stat = 0

        stat = nf90_open(trim(filename), NF90_NOWRITE, handle % ncid)
        if (stat /= NF90_NOERR) then
            stat = 1
            return
        end if

        stat = nf90_inquire(handle % ncid, nVariables=handle % num_vars)
        if (stat /= NF90_NOERR) then
            stat = 1
            return
        end if

        allocate(handle % varids(handle % num_vars))

        stat = nf90_inq_varids(handle % ncid, handle % num_vars, handle % varids)
        if (stat /= NF90_NOERR) then
            stat = 1
            return
        end if

        stat = nf90_inquire(handle % ncid, unlimitedDimId=handle % unlimited_dimid)
        if (stat /= NF90_NOERR) then
            stat = 1
            return
        end if

        handle % current_var = 1

    end function scan_input_open


    integer function scan_input_close(handle) result(stat)

        implicit none

        type (input_handle_type), intent(inout) :: handle


        stat = 0

        stat = nf90_close(handle % ncid)
        if (stat /= NF90_NOERR) then
            stat = 1
        end if

        if (associated(handle % varids)) then
            deallocate(handle % varids)
        end if
        handle % current_var = 0

    end function scan_input_close


    integer function scan_input_next_field(handle, field) result(stat)

        implicit none

        type (input_handle_type), intent(inout) :: handle
        type (input_field_type), intent(out) :: field

        integer :: idim


        stat = 0

        if (handle % current_var < 1 .or. handle % current_var > handle % num_vars) then
            stat = 1
            return
        end if

        field % varid = handle % varids(handle % current_var)
        stat = nf90_inquire_variable(handle % ncid, field % varid, &
                                     name=field % name, &
                                     xtype=field % xtype, &
                                     ndims=field % ndims)
        if (stat /= NF90_NOERR) then
            stat = 1
            return
        end if

        if (field % xtype == NF90_FLOAT) then
            field % xtype = FIELD_TYPE_REAL
        else if (field % xtype == NF90_DOUBLE) then
            field % xtype = FIELD_TYPE_DOUBLE
        else if (field % xtype == NF90_INT) then
            field % xtype = FIELD_TYPE_INTEGER
        else if (field % xtype == NF90_CHAR) then
            field % xtype = FIELD_TYPE_CHARACTER
        end if

        allocate(field % dimids(field % ndims))

        stat = nf90_inquire_variable(handle % ncid, field % varid, &
                                     dimids=field % dimids)
        if (stat /= NF90_NOERR) then
            stat = 1
            deallocate(field % dimids)
            return
        end if

        allocate(field % dimlens(field % ndims))
        allocate(field % dimnames(field % ndims))

        do idim=1,field % ndims
            stat = nf90_inquire_dimension(handle % ncid, field % dimids(idim), &
                                          name=field % dimnames(idim), &
                                          len=field % dimlens(idim))
            if (field % dimids(idim) == handle % unlimited_dimid) then
                field % isTimeDependent = .true.
            end if
        end do

        handle % current_var = handle % current_var + 1

    end function scan_input_next_field


    integer function scan_input_for_field(handle, fieldname, field) result(stat)

        implicit none

        type (input_handle_type), intent(inout) :: handle
        character (len=*), intent(in) :: fieldname
        type (input_field_type), intent(out) :: field

        integer :: idim

        stat = 0

        stat = nf90_inq_varid(handle % ncid, trim(fieldname), field % varid)
        if (stat /= NF90_NOERR) then
            stat = 1
            return
        end if

        stat = nf90_inquire_variable(handle % ncid, field % varid, &
                                     name=field % name, &
                                     xtype=field % xtype, &
                                     ndims=field % ndims)
        if (stat /= NF90_NOERR) then
            stat = 1
            return
        end if

        allocate(field % dimids(field % ndims))

        stat = nf90_inquire_variable(handle % ncid, field % varid, &
                                     dimids=field % dimids)
        if (stat /= NF90_NOERR) then
            stat = 1
            deallocate(field % dimids)
            return
        end if

        allocate(field % dimlens(field % ndims))
        allocate(field % dimnames(field % ndims))

        do idim=1,field % ndims
            stat = nf90_inquire_dimension(handle % ncid, field % dimids(idim), &
                                          name=field % dimnames(idim), &
                                          len=field % dimlens(idim))
            if (field % dimids(idim) == handle % unlimited_dimid) then
                field % isTimeDependent = .true.
            end if
        end do

    end function scan_input_for_field


    integer function scan_input_free_field(field) result(stat)

        implicit none

        type (input_field_type), intent(inout) :: field


        stat = 0

        if (associated(field % dimids)) then
            deallocate(field % dimids)
        end if
        if (associated(field % dimlens)) then
            deallocate(field % dimlens)
        end if
        if (associated(field % dimnames)) then
            deallocate(field % dimnames)
        end if

    end function scan_input_free_field

end module scan_input
