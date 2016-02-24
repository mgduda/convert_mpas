program convert_mpas

    use scan_input

    implicit none

   
    integer :: handle
    integer :: stat
    character (len=1024) :: filename
    type (input_field_type) :: field

    write(filename,'(a)') 'grid.nc'

    handle = scan_input_open(filename)

    do while (scan_input_next_field(handle, field) == 0) 
       write(0,*) 'Field name = ', trim(field % name)
    end do

    stat = scan_input_close(handle)

end program convert_mpas
