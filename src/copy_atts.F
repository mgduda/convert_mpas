module copy_atts
    
    use netcdf
    use scan_input, only: input_handle_type, input_field_type
    use file_output, only: output_handle_type
    use remapper, only: target_field_type

    contains


    ! Copy attributes from field to target_field

    integer function copy_field_atts(handle, field, output_handle, target_field) result(stat)

        implicit none

        type (input_field_type)   :: field
        type (input_handle_type)  :: handle
        type (output_handle_type) :: output_handle
        type (target_field_type)  :: target_field

        character (len=256) :: attname
        integer :: iatt
        integer :: natts
        integer :: target_varid

        stat = 0

        ! Inquire number of attributes - natts
        stat = nf90_inquire_variable(handle%ncid, field%varid, natts=natts)
        if(stat /= NF90_NOERR) then
            WRITE(*,*) NF90_STRERROR(stat)
            stat = 1
            return
        end if

        ! Loop thru attributes
        do iatt=1,natts
            ! Inquire attribute name - attname
            stat = nf90_inq_attname(handle % ncid, field%varid, iatt, attname)
            if(stat /= NF90_NOERR) then
                WRITE(*,*) NF90_STRERROR(stat)
                stat = 1
                return
            end if
            ! Inquire variable ID of target_field - target_varid
            stat = NF90_INQ_VARID(output_handle%ncid, target_field%name, target_varid)
            if(stat /= NF90_NOERR) then
                WRITE(*,*) NF90_STRERROR(stat)
                stat = 1
                return
            end if
            ! Copy attribute from field to target_field
            stat = nf90_copy_att(handle % ncid, field % varid, attname, output_handle % ncid, target_varid)
            if(stat /= NF90_NOERR) then
                WRITE(*,*) NF90_STRERROR(stat)
                stat = 1
                return
            end if
        end do

    end function copy_field_atts

end module copy_atts
