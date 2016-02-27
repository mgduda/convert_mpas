convert_mpas
============

The 'convert_mpas' project aims to develop a general framework for mapping 
native MPAS output to other meshes.

### To-do:
- Add command-line arguments to specify nLat, nLon, startLat, startLon
- Add basic information on compiling and running to this README
- Experiment with OpenMP directives to speed up interpolation
- Clean up print statements and possibly add timing information to output
- Ensure that, for cell fields, the interpolation location lies within the triangle used for interpolation
- Handle time dimension in both input and output files; for now, we can just keep one time period per file
