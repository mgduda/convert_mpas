FC = gfortran
FFLAGS = -O2 -ffree-form -Wall
FCINCLUDES = -I${NETCDF}/include
FCLIBS = -L${NETCDF}/lib -lnetcdff -lnetcdf

all:
	( cd src; $(MAKE) FC="$(FC)" FFLAGS="$(FFLAGS)" FCINCLUDES="$(FCINCLUDES)" FCLIBS="$(FCLIBS)" )
	if [ -e src/convert_mpas ] ; then \
	   ( cp src/convert_mpas . ) \
	fi;
clean:
	( cd src; $(MAKE) clean )
	rm -f convert_mpas
