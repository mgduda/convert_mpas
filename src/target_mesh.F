module target_mesh

    type target_mesh_type
        integer :: nlat = 0
        integer :: nlon = 0
        real, dimension(:), pointer :: lats => null()
        real, dimension(:), pointer :: lons => null()
    end type target_mesh_type


    contains


    integer function target_mesh_setup(mesh, nlat, nlon) result(stat)

        implicit none

        type (target_mesh_type), intent(out) :: mesh
        integer, intent(in), optional :: nlat
        integer, intent(in), optional :: nlon

        integer :: i
        real :: delta
        real :: start_lat
        real :: start_lon
        real, parameter :: pi_const = 2.0 * asin(1.0)

        stat = 0

        if (present(nlat) .and. present(nlon)) then
            mesh % nlat = nlat
            mesh % nlon = nlon
        else
            mesh % nlat = 180
            mesh % nlon = 360
        end if

        allocate(mesh % lats(mesh % nlat))
        allocate(mesh % lons(mesh % nlon))

        delta = 180.0 / real(mesh % nlat)
        start_lat = -90.0
        do i=0,mesh % nlat-1
           mesh % lats(i+1) = start_lat + 0.5 * delta + real(i) * delta
           mesh % lats(i+1) = mesh % lats(i+1) * pi_const / 180.0
        end do

        delta = 360.0 / real(mesh % nlon)
        start_lon = -180.0
        do i=0,mesh % nlon-1
           mesh % lons(i+1) = start_lon + 0.5 * delta + real(i) * delta
           mesh % lons(i+1) = mesh % lons(i+1) * pi_const / 180.0
        end do

    end function target_mesh_setup


    integer function target_mesh_free(mesh) result(stat)

        implicit none

        type (target_mesh_type), intent(inout) :: mesh


        stat = 0

        mesh % nlat = 0
        mesh % nlon = 0

        if (associated(mesh % lats)) deallocate(mesh % lats)
        if (associated(mesh % lons)) deallocate(mesh % lons)

    end function target_mesh_free

end module target_mesh
