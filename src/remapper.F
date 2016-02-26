module remapper

    use mpas_mesh
    use scan_input, only : input_field_type, FIELD_TYPE_REAL, FIELD_TYPE_DOUBLE, FIELD_TYPE_INTEGER
    use target_mesh

    type remap_info_type
        integer :: method = -1
        type (mpas_mesh_type), pointer :: src_mesh
        type (target_mesh_type), pointer :: dst_mesh
        integer, dimension(:,:), pointer :: nearestCell
        integer, dimension(:,:), pointer :: nearestVertex
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
        integer, dimension(:,:,:,:), pointer :: array4i => null()
    end type target_field_type

    private :: nearest_cell, nearest_vertex, sphere_distance


    contains


    integer function remap_info_setup(src_mesh, dst_mesh, remap_info) result(stat)

        implicit none

        type (mpas_mesh_type), intent(in) :: src_mesh
        type (target_mesh_type), intent(in) :: dst_mesh
        type (remap_info_type), intent(out) :: remap_info

        integer :: idx
        integer :: ix, iy

        stat = 0

        remap_info % method = 1

        allocate(remap_info % nearestCell(dst_mesh % nlon, dst_mesh % nlat))
        allocate(remap_info % nearestVertex(dst_mesh % nlon, dst_mesh % nlat))

        idx = 1
        do iy=1,dst_mesh % nlat
        do ix=1,dst_mesh % nlon
            idx = nearest_cell(dst_mesh % lats(iy), dst_mesh % lons(ix), idx, src_mesh % nCells, src_mesh % maxEdges, &
                               src_mesh % nEdgesOnCell, src_mesh % cellsOnCell, src_mesh % latCell, src_mesh % lonCell)
            remap_info % nearestCell(ix, iy) = idx
        end do
        end do

        idx = 1
        do iy=1,dst_mesh % nlat
        do ix=1,dst_mesh % nlon
            idx = nearest_vertex(dst_mesh % lats(iy), dst_mesh % lons(ix), idx, &
                                 src_mesh % nCells, src_mesh % nVertices, src_mesh % maxEdges, &
                                 src_mesh % nEdgesOnCell, src_mesh % verticesOnCell, &
                                 src_mesh % cellsOnVertex, src_mesh % latCell, src_mesh % lonCell, &
                                 src_mesh % latVertex, src_mesh % lonVertex )
            remap_info % nearestVertex(ix, iy) = idx
        end do
        end do

    end function remap_info_setup


    integer function remap_info_free(remap_info) result(stat)

        implicit none

        type (remap_info_type), intent(inout) :: remap_info


        stat = 0

        remap_info % method = -1

        if (associated(remap_info % nearestCell)) then
            deallocate(remap_info % nearestCell)
        end if
        if (associated(remap_info % nearestVertex)) then
            deallocate(remap_info % nearestVertex)
        end if

    end function remap_info_free


    logical function can_remap_field(field)

        implicit none

        type (input_field_type), intent(in) :: field

        integer :: decompDim

        can_remap_field = .true.

        if (field % xtype /= FIELD_TYPE_INTEGER .and. &
            field % xtype /= FIELD_TYPE_REAL .and. &
            field % xtype /= FIELD_TYPE_DOUBLE) then
            can_remap_field = .false.
            return
        end if

        if (field % ndims == 0 .or. &
            (field % ndims == 1 .and. field % isTimeDependent)) then
            can_remap_field = .false.
            return
        end if

        decompDim = field % ndims
        if (field % isTimeDependent) then
            decompDim = decompDim - 1
        end if

        if (trim(field % dimnames(decompDim)) /= 'nCells' .and. &
            trim(field % dimnames(decompDim)) /= 'nVertices' .and. &
            trim(field % dimnames(decompDim)) /= 'nEdges') then

            can_remap_field = .false.
            return
        end if

    end function can_remap_field


    integer function remap_field(remap_info, src_field, dst_field, frame) result(stat)

        implicit none

        type (remap_info_type), intent(in) :: remap_info
        type (input_field_type), intent(in) :: src_field
        type (target_field_type), intent(out) :: dst_field
        integer, intent(in), optional :: frame

        integer :: local_frame
        integer :: decompDim
        integer :: idim
        integer :: iy, ix
        integer, dimension(:,:), pointer :: nearestIndex

        stat = 0

        local_frame = 1
        if (present(frame)) then
            local_frame = frame
        end if

        decompDim = src_field % ndims
        if (src_field % isTimeDependent) then
            decompDim = decompDim - 1
        end if

        if (trim(src_field % dimnames(decompDim)) == 'nCells') then
            nearestIndex => remap_info % nearestCell
        else if (trim(src_field % dimnames(decompDim)) == 'nVertices') then
            nearestIndex => remap_info % nearestVertex
        else if (trim(src_field % dimnames(decompDim)) == 'nEdges') then
            nearestIndex => remap_info % nearestCell
            write(0,*) 'Remap exception: edges not currently handled...'
        else
            write(0,*) 'Remap exception: unhandled decomposed dim'
        end if

        dst_field % name = src_field % name
        dst_field % xtype = src_field % xtype
        if (src_field % isTimeDependent) then
            ! Single horizontal dimension becomes two horizontal dimensions, nlat and nlon,
            ! but the time dimension is not counted in the target field
            dst_field % ndims = src_field % ndims
        else
            ! Single horizontal dimension becomes two horizontal dimensions, nlat and nlon
            dst_field % ndims = src_field % ndims + 1
        end if
        allocate(dst_field % dimnames(dst_field % ndims))
        allocate(dst_field % dimlens(dst_field % ndims))
        dst_field % isTimeDependent = src_field % isTimeDependent
        do idim=1,dst_field % ndims-2
            dst_field % dimlens(idim) = src_field % dimlens(idim)
            dst_field % dimnames(idim) = src_field % dimnames(idim)
        end do
        dst_field % dimlens(dst_field % ndims-1) = size(remap_info % nearestCell, dim=1)
        dst_field % dimnames(dst_field % ndims-1) = 'nLons'
        dst_field % dimlens(dst_field % ndims) = size(remap_info % nearestCell, dim=2)
        dst_field % dimnames(dst_field % ndims) = 'nLats'

        if (src_field % xtype == FIELD_TYPE_REAL) then
            if (dst_field % ndims == 2) then
                allocate(dst_field % array2r(dst_field % dimlens(1), &
                                             dst_field % dimlens(2)))
                if (dst_field % isTimeDependent) then
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array2r(ix,iy) = src_field % array2r(nearestIndex(ix,iy),local_frame)
                    end do
                    end do
                else
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array2r(ix,iy) = src_field % array1r(nearestIndex(ix,iy))
                    end do
                    end do
                end if
            else if (dst_field % ndims == 3) then
                allocate(dst_field % array3r(dst_field % dimlens(1), &
                                             dst_field % dimlens(2), &
                                             dst_field % dimlens(3)))
                if (dst_field % isTimeDependent) then
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array3r(:,ix,iy) = src_field % array3r(:,nearestIndex(ix,iy),local_frame)
                    end do
                    end do
                else
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array3r(:,ix,iy) = src_field % array2r(:,nearestIndex(ix,iy))
                    end do
                    end do
                end if
            else if (dst_field % ndims == 4) then
                allocate(dst_field % array4r(dst_field % dimlens(1), &
                                             dst_field % dimlens(2), &
                                             dst_field % dimlens(3), &
                                             dst_field % dimlens(4)))
                if (dst_field % isTimeDependent) then
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array4r(:,:,ix,iy) = src_field % array4r(:,:,nearestIndex(ix,iy),local_frame)
                    end do
                    end do
                else
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array4r(:,:,ix,iy) = src_field % array3r(:,:,nearestIndex(ix,iy))
                    end do
                    end do
                end if
            else
                write(0,*) 'Remap exception: unhandled dimension for real ', dst_field % ndims
            end if
        else if (src_field % xtype == FIELD_TYPE_DOUBLE) then
            if (dst_field % ndims == 2) then
                allocate(dst_field % array2d(dst_field % dimlens(1), &
                                             dst_field % dimlens(2)))
                if (dst_field % isTimeDependent) then
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array2d(ix,iy) = src_field % array2d(nearestIndex(ix,iy),local_frame)
                    end do
                    end do
                else
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array2d(ix,iy) = src_field % array1d(nearestIndex(ix,iy))
                    end do
                    end do
                end if
            else if (dst_field % ndims == 3) then
                allocate(dst_field % array3d(dst_field % dimlens(1), &
                                             dst_field % dimlens(2), &
                                             dst_field % dimlens(3)))
                if (dst_field % isTimeDependent) then
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array3d(:,ix,iy) = src_field % array3d(:,nearestIndex(ix,iy),local_frame)
                    end do
                    end do
                else
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array3d(:,ix,iy) = src_field % array2d(:,nearestIndex(ix,iy))
                    end do
                    end do
                end if
            else if (dst_field % ndims == 4) then
                allocate(dst_field % array4d(dst_field % dimlens(1), &
                                             dst_field % dimlens(2), &
                                             dst_field % dimlens(3), &
                                             dst_field % dimlens(4)))
                if (dst_field % isTimeDependent) then
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array4d(:,:,ix,iy) = src_field % array4d(:,:,nearestIndex(ix,iy),local_frame)
                    end do
                    end do
                else
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array4d(:,:,ix,iy) = src_field % array3d(:,:,nearestIndex(ix,iy))
                    end do
                    end do
                end if
            else
                write(0,*) 'Remap exception: unhandled dimension for dbl ', dst_field % ndims
            end if
        else if (src_field % xtype == FIELD_TYPE_INTEGER) then
            if (dst_field % ndims == 2) then
                allocate(dst_field % array2i(dst_field % dimlens(1), &
                                             dst_field % dimlens(2)))
                if (dst_field % isTimeDependent) then
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array2i(ix,iy) = src_field % array2i(nearestIndex(ix,iy),local_frame)
                    end do
                    end do
                else
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array2i(ix,iy) = src_field % array1i(nearestIndex(ix,iy))
                    end do
                    end do
                end if
            else if (dst_field % ndims == 3) then
                allocate(dst_field % array3i(dst_field % dimlens(1), &
                                             dst_field % dimlens(2), &
                                             dst_field % dimlens(3)))
                if (dst_field % isTimeDependent) then
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array3i(:,ix,iy) = src_field % array3i(:,nearestIndex(ix,iy),local_frame)
                    end do
                    end do
                else
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array3i(:,ix,iy) = src_field % array2i(:,nearestIndex(ix,iy))
                    end do
                    end do
                end if
            else if (dst_field % ndims == 4) then
                allocate(dst_field % array4i(dst_field % dimlens(1), &
                                             dst_field % dimlens(2), &
                                             dst_field % dimlens(3), &
                                             dst_field % dimlens(4)))
                if (dst_field % isTimeDependent) then
!                    do iy=1,size(nearestIndex, dim=2)
!                    do ix=1,size(nearestIndex, dim=1)
!                        dst_field % array4i(:,:,ix,iy) = src_field % array4i(:,:,nearestIndex(ix,iy),local_frame)
!                    end do
!                    end do
                     write(0,*) 'Remap exception: no support for time-dependent 3-d integer fields'
                else
                    do iy=1,size(nearestIndex, dim=2)
                    do ix=1,size(nearestIndex, dim=1)
                        dst_field % array4i(:,:,ix,iy) = src_field % array3i(:,:,nearestIndex(ix,iy))
                    end do
                    end do
                end if
            else
                write(0,*) 'Remap exception: unhandled dimension for int ', dst_field % ndims
            end if
        else
            write(0,*) 'Remap exception: unhandled type'
        end if

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
        if (associated(field % array4i)) then
            deallocate(field % array4i)
        end if

    end function free_target_field


    integer function nearest_cell(target_lat, target_lon, start_cell, nCells, maxEdges, &
                                  nEdgesOnCell, cellsOnCell, latCell, lonCell)

        implicit none

        real, intent(in) :: target_lat, target_lon
        integer, intent(in) :: start_cell
        integer, intent(in) :: nCells, maxEdges
        integer, dimension(nCells), intent(in) :: nEdgesOnCell
        integer, dimension(maxEdges,nCells), intent(in) :: cellsOnCell
        real, dimension(nCells), intent(in) :: latCell, lonCell

        integer :: i
        integer :: iCell
        integer :: current_cell
        real :: current_distance, d
        real :: nearest_distance

        nearest_cell = start_cell
        current_cell = -1

        do while (nearest_cell /= current_cell)
            current_cell = nearest_cell
            current_distance = sphere_distance(latCell(current_cell), lonCell(current_cell), target_lat, &
                                               target_lon, 1.0)
            nearest_cell = current_cell
            nearest_distance = current_distance
            do i = 1, nEdgesOnCell(current_cell)
                iCell = cellsOnCell(i,current_cell)
                if (iCell <= nCells) then
                    d = sphere_distance(latCell(iCell), lonCell(iCell), target_lat, target_lon, 1.0)
                    if (d < nearest_distance) then
                        nearest_cell = iCell
                        nearest_distance = d
                    end if
                end if
            end do
        end do

    end function nearest_cell


    integer function nearest_vertex( target_lat, target_lon, &
                                     start_vertex, &
                                     nCells, nVertices, maxEdges, &
                                     nEdgesOnCell, verticesOnCell, &
                                     cellsOnVertex, latCell, lonCell, &
                                     latVertex, lonVertex )

        implicit none

        real, intent(in) :: target_lat, target_lon
        integer, intent(in) :: start_vertex
        integer, intent(in) :: nCells, nVertices, maxEdges
        integer, dimension(nCells), intent(in) :: nEdgesOnCell
        integer, dimension(maxEdges,nCells), intent(in) :: verticesOnCell
        integer, dimension(3,nVertices), intent(in) :: cellsOnVertex
        real, dimension(nCells), intent(in) :: latCell, lonCell
        real, dimension(nVertices), intent(in) :: latVertex, lonVertex


        integer :: i, cell1, cell2, cell3, iCell
        integer :: iVtx
        integer :: current_vertex
        real :: cell1_dist, cell2_dist, cell3_dist
        real :: current_distance, d
        real :: nearest_distance

        nearest_vertex = start_vertex
        current_vertex = -1

        do while (nearest_vertex /= current_vertex)
            current_vertex = nearest_vertex
            current_distance = sphere_distance(latVertex(current_vertex), lonVertex(current_vertex), &
                                               target_lat,                target_lon,                1.0)
            nearest_vertex = current_vertex
            nearest_distance = current_distance
            cell1 = cellsOnVertex(1,current_vertex)
            cell2 = cellsOnVertex(2,current_vertex)
            cell3 = cellsOnVertex(3,current_vertex)
            cell1_dist = sphere_distance(latCell(cell1), lonCell(cell1), target_lat, target_lon, 1.0)
            cell2_dist = sphere_distance(latCell(cell2), lonCell(cell2), target_lat, target_lon, 1.0)
            cell3_dist = sphere_distance(latCell(cell3), lonCell(cell3), target_lat, target_lon, 1.0)
            if (cell1_dist < cell2_dist) then
                if (cell1_dist < cell3_dist) then
                    iCell = cell1
                else
                    iCell = cell3
                end if
            else
                if (cell2_dist < cell3_dist) then
                    iCell = cell2
                else
                    iCell = cell3
                end if
            end if
            do i = 1, nEdgesOnCell(iCell)
                iVtx = verticesOnCell(i,iCell)
                d = sphere_distance(latVertex(iVtx), lonVertex(iVtx), target_lat, target_lon, 1.0)
                if (d < nearest_distance) then
                    nearest_vertex = iVtx
                    nearest_distance = d
                end if
            end do
        end do

    end function nearest_vertex


    real function sphere_distance(lat1, lon1, lat2, lon2, radius)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Compute the great-circle distance between (lat1, lon1) and (lat2, lon2) on a
    !   sphere with given radius.
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        implicit none

        real, intent(in) :: lat1, lon1, lat2, lon2, radius

        real :: arg1

        arg1 = sqrt( sin(0.5*(lat2-lat1))**2 +  &
                     cos(lat1)*cos(lat2)*sin(0.5*(lon2-lon1))**2 )
        sphere_distance = 2.*radius*asin(arg1)

    end function sphere_distance

end module remapper
