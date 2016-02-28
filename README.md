convert_mpas
============

The 'convert_mpas' project aims to develop a general framework for mapping 
native MPAS output to other meshes.

## Compiling:

After selecting a Fortran compiler and associated compiler flags with the FC and FFLAGS
variables in the top-level Makefile, the 'convert_mpas' program may be compiled by 
simply running 'make'.


## Running:

The 'convert_mpas' program takes either one or two command-line arguments:
- If only one file argument is given, both the MPAS mesh information and 
  the fields will be read from the specified file.
- If two file arguments are given, the MPAS mesh information will be read from 
  the first file and fields to be remapped will be read from the second file.

Running 'convert_mpas' with no command-line arguments will print a usage summary.

By default, the 'convert_mpas' will remap all integer, real, or double-precision
fields that it finds in the input data file. However, by creating a list of
fields in a file named 'include_fields' in the run directory, with one field name 
per line, the 'convert_mpas' program will remap only those fields listed in 
the file. Alternatively, one can create a list of fields to be excluded from 
the output file; this list should be written to a file named 'exclude_fields'.
If both an 'include_fields' file and an 'exclude_fields' file are present in 
the run directory, only fields listed in the 'include_fields' file will be 
remapped, and the contents of the 'exclude_fields' file are ignored.

The target domain defaults to a 0.5x0.5-degree global lat-lon grid. However, one
may specify an alternate target domain using a file named 'target_domain' in 
the run directory. This file may contain lines assigning values to keywords, i.e.,

keyword = value

The following are available keywords for describing the target domain:
 - nlat : the number of latitude points in the grid (default value 360)
 - nlon : the number of longitude points in the grid (default value 720)
 - startlat : the starting latitude (default value -90.0)
 - startlon : the starting longitude (default value -180.0)
 - endlat : the ending latitude (default value 90.0)
 - endlon : the ending longitude (default value 180.0)

The actual points to which fields are interpolated are determined by dividing
the latitude and longitude ranges into the specified number of intervals, then
locating the interpolation points at the center of these intervals. For example,
specifying startlat=0, endlat=10, and nlat=10 would result in target latitudes
of 0.5, 1.5, ..., 8.5, and 9.5.

## Interpolation methodology:

Integer fields are remapped to the target grid using a nearest-neighbor scheme.
For all real-valued (single- or double-precision) fields, the 'convert_mpas' program
employs a barycentric interpolation, the output of which is C0 continuous. Cell-based
fields in the MPAS mesh are sampled from the three cell centers that form the vertices
of the Delaunay triangle containing the target point. Vertex- and edge-based fields
are sampled from the corners or faces, respectively, of the Voronoi cell containing
the target point.

## To-do:
- Experiment with OpenMP directives to speed up interpolation
- Clean up print statements and possibly add timing information to output
- Ensure that, for cell fields, the interpolation location lies within the triangle 
  used for interpolation
- Handle time dimension in both input and output files; for now, we can just keep 
  one time period per file
