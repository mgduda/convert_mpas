FC = $(shell nc-config --fc)
FFLAGS = -O3 -DHAVE_NF90_INQ_VARIDS
FCINCLUDES = $(shell nc-config --fflags)
FCLIBS = $(shell nc-config --flibs)


CHECKS = config_check
.PHONY: $(CHECKS)

all: $(CHECKS)
	( cd src; $(MAKE) FC="$(FC)" FFLAGS="$(FFLAGS)" FCINCLUDES="$(FCINCLUDES)" FCLIBS="$(FCLIBS)" )
	if [ -f src/convert_mpas ] ; then \
	   ( cp src/convert_mpas . ) \
	fi;

clean:
	( cd src; $(MAKE) clean )
	rm -f convert_mpas



#-------------------------------------------------------------------------------#
#-------- Targets for checking various aspects of the build environment --------#
#-------------------------------------------------------------------------------#


# Check that the nc-config program is available
config_check:
	@ printf "Checking for nc-config... "
	@ nc-config --fc > /dev/null 2>&1; \
	if [ $$? -ne 0 ]; then \
		printf "\n\nError: nc-config not found in \$$PATH, or it is not working correctly.\n"; \
		printf "       The nc-config program is typically found in a bin/ sub-directory\n"; \
		printf "       of the installation path of the NetCDF library. If you have set\n"; \
		printf "       the \$$NETCDF environment variable to the NetCDF installation path,\n"; \
		printf "       adding \$${NETCDF}/bin to your \$$PATH is usually sufficient.\n\n"; \
		exit 1; \
	fi
	@ printf "OK\n"
