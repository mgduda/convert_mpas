module remapper

    use mpas_mesh
    use scan_input, only : input_field_type, FIELD_TYPE_REAL, FIELD_TYPE_DOUBLE, FIELD_TYPE_INTEGER
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
        integer, dimension(:), pointer :: dimlens => null()
        character (len=64), dimension(:), pointer :: dimnames => null()

        !  Members to store field data
        real :: array0r
        real, dimension(:), pointer :: array1r => null()
        real, dimension(:,:), pointer :: array2r => null()
        real, dimension(:,:,:), pointer :: array3r => null()
        real, dimension(:,:,:,:), pointer :: array4r => null()
        double precision :: array0d
        double precision, dimension(:), pointer :: array1d => null()
        double precision, dimension(:,:), pointer :: array2d => null()
        double precision, dimension(:,:,:), pointer :: array3d => null()
        double precision, dimension(:,:,:,:), pointer :: array4d => null()
        integer :: array0i
        integer, dimension(:), pointer :: array1i => null()
        integer, dimension(:,:), pointer :: array2i => null()
        integer, dimension(:,:,:), pointer :: array3i => null()
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


    integer function remap_field(remap_info, src_field, dst_field) result(stat)

        implicit none

        type (remap_info_type), intent(in) :: remap_info
        type (input_field_type), intent(in) :: src_field
        type (target_field_type), intent(out) :: dst_field

        stat = 0

        ! something
        dst_field % name = 'foo'
        dst_field % xtype = FIELD_TYPE_DOUBLE
        dst_field % ndims = 1
        dst_field % isTimeDependent = .true.
        allocate(dst_field % dimnames(1))
        allocate(dst_field % dimlens(1))
        dst_field % dimnames(1) = 'nLats'
        dst_field % dimlens(1) = 180
        allocate(dst_field % array1d(180))
        dst_field % array1d(:) = 42.14159265359

    end function remap_field


    integer function free_target_field(field) result(stat)

        implicit none

        type (target_field_type), intent(inout) :: field

        stat = 0

        if (associated(field % dimlens)) then
            deallocate(field % dimlens)
        end if
        if (associated(field % dimnames)) then
            deallocate(field % dimnames)
        end if

        if (associated(field % array1r)) then
            deallocate(field % array1r)
        end if
        if (associated(field % array2r)) then
            deallocate(field % array2r)
        end if
        if (associated(field % array3r)) then
            deallocate(field % array3r)
        end if
        if (associated(field % array4r)) then
            deallocate(field % array4r)
        end if

        if (associated(field % array1d)) then
            deallocate(field % array1d)
        end if
        if (associated(field % array2d)) then
            deallocate(field % array2d)
        end if
        if (associated(field % array3d)) then
            deallocate(field % array3d)
        end if
        if (associated(field % array4d)) then
            deallocate(field % array4d)
        end if

        if (associated(field % array1i)) then
            deallocate(field % array1i)
        end if
        if (associated(field % array2i)) then
            deallocate(field % array2i)
        end if
        if (associated(field % array3i)) then
            deallocate(field % array3i)
        end if

    end function free_target_field

end module remapper
