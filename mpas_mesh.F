module mpas_mesh

    type mpas_mesh_type
        integer :: nCells = 0
        integer :: nVertices = 0
        integer :: nEdges = 0
        integer :: maxEdges = 0
    end type mpas_mesh_type


    contains


    integer function mpas_mesh_setup(mesh_filename, mesh) result(stat)

        implicit none

        character (len=*), intent(in) :: mesh_filename
        type (mpas_mesh_type), intent(out) :: mesh


        stat = 0

        mesh % nCells = 40962
        mesh % nVertices = 2 * (mesh % nCells - 2)
        mesh % nEdges = 3 * (mesh % nCells - 2)

    end function mpas_mesh_setup


    integer function mpas_mesh_free(mesh) result(stat)

        implicit none

        type (mpas_mesh_type), intent(inout) :: mesh


        stat = 0

        mesh % nCells = 0
        mesh % nVertices = 0
        mesh % nEdges = 0

    end function mpas_mesh_free

end module mpas_mesh
