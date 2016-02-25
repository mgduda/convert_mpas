module remapper

    use mpas_mesh
    use target_mesh

    type remap_info_type
        integer :: method = -1
        type (mpas_mesh_type), pointer :: src_mesh
        type (target_mesh_type), pointer :: dst_mesh
    end type remap_info_type

    type target_field_type
        character (len=64) :: name
        integer :: ndims = -1
        integer :: xtype = -1
        logical :: isTimeDependent = .false.
    end type target_field_type


    contains


    integer function remap_info_setup(src_mesh, dst_mesh, remap_info) result(stat)

        implicit none

        type (mpas_mesh_type), intent(in) :: src_mesh
        type (target_mesh_type), intent(in) :: dst_mesh
        type (remap_info_type), intent(out) :: remap_info


        stat = 0

        remap_info % method = 1

    end function remap_info_setup


    integer function remap_info_free(remap_info) result(stat)

        implicit none

        type (remap_info_type), intent(inout) :: remap_info


        stat = 0

        remap_info % method = -1

    end function remap_info_free

end module remapper
