FC = gfortran
FFLAGS = -O2 -ffree-form
FCINCLUDES = -I${NETCDF}/include
FCLIBS = -L${NETCDF}/lib -lnetcdf

.SUFFIXES: .F .o

OBJS = \
	scan_input.o \
	convert_mpas.o

all: $(OBJS)
	$(FC) -o convert_mpas convert_mpas.o scan_input.o $(FCLIBS)

convert_mpas.o: scan_input.o

scan_input.o:

clean:
	rm -f *.mod *.o convert_mpas

.F.o:
	rm -f $@ $*.mod
	$(FC) $(FFLAGS) -c $*.F $(FCINCLUDES)
