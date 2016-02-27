convert_mpas
============

The 'convert_mpas' project aims to develop a general framework for mapping 
native MPAS output to other meshes.

### To-do:
- Ensure that, for cell fields, the interpolation location lies within the triangle used for interpolation
- Enable lists of fields to be specified; at present, we can just restrict what we write to output in MPAS
- Handle time dimension in both input and output files; for now, we can just keep one time period per file
