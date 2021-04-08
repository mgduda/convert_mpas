FC = $(shell nc-config --fc)
FFLAGS = -O2 -ffree-form -Wall -DHAVE_NF90_INQ_VARIDS
FCINCLUDES = $(shell nc-config --fflags)
FCLIBS = $(shell nc-config --flibs)

all:
	( cd src; $(MAKE) FC="$(FC)" FFLAGS="$(FFLAGS)" FCINCLUDES="$(FCINCLUDES)" FCLIBS="$(FCLIBS)" )
	if [ -e src/convert_mpas ] ; then \
	   ( cp src/convert_mpas . ) \
	fi;
clean:
	( cd src; $(MAKE) clean )
	rm -f convert_mpas
