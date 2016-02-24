module scan_input

    type input_field_type
        character (len=64) :: name
    end type input_field_type


    contains


    integer function scan_input_open(filename) result(handle)

        implicit none

        character (len=*), intent(in) :: filename

        handle = 42

    end function scan_input_open


    integer function scan_input_close(handle) result(stat)

        implicit none

        integer, intent(in) :: handle

        stat = 0

    end function scan_input_close


    integer function scan_input_next_field(handle, field) result(stat)

        implicit none

        integer, intent(in) :: handle
        type (input_field_type), intent(out) :: field

        field % name = 'theta'
        stat = 0

    end function scan_input_next_field

end module scan_input
