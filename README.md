convert_mpas
============

The 'convert_mpas' project aims to develop a general framework for mapping 
native MPAS output to other meshes.

### To-do:
- Handle time dimension in both input and output files; for now, we can just keep one time period per file
- Provide linear interpolation for cells, vertices, and edges; initially, cells and vertices may be sufficient
- Enable lists of fields to be specified; at present, we can just restrict what we write to output in MPAS
