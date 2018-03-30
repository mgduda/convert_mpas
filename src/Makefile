.SUFFIXES: .F .o

OBJS = \
	scan_input.o \
	mpas_mesh.o \
	target_mesh.o \
	remapper.o \
	file_output.o \
	copy_atts.o \
	field_list.o \
	timer.o \
	convert_mpas.o

all: $(OBJS)
	$(FC) -o convert_mpas $(OBJS) $(FCLIBS)

convert_mpas.o: scan_input.o mpas_mesh.o target_mesh.o file_output.o copy_atts.o remapper.o field_list.o timer.o

remapper.o: mpas_mesh.o target_mesh.o scan_input.o

file_output.o: remapper.o scan_input.o

copy_atts.o: scan_input.o file_output.o remapper.o

mpas_mesh.o: scan_input.o

field_list.o: scan_input.o

scan_input.o:

target_mesh.o:

clean:
	rm -f *.mod *.o convert_mpas

.F.o:
	rm -f $@ $*.mod
	$(FC) $(FFLAGS) -c $*.F $(FCINCLUDES)
