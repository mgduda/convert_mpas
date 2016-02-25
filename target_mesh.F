module target_mesh

    type target_mesh_type
        integer :: nlat = 0
        integer :: nlon = 0
    end type target_mesh_type


    contains


    integer function target_mesh_setup(mesh) result(stat)

        implicit none

        type (target_mesh_type), intent(out) :: mesh


        stat = 0

        mesh % nlat = 180
        mesh % nlon = 360

    end function target_mesh_setup


    integer function target_mesh_free(mesh) result(stat)

        implicit none

        type (target_mesh_type), intent(inout) :: mesh


        stat = 0

        mesh % nlat = 0
        mesh % nlon = 0

    end function target_mesh_free

end module target_mesh
