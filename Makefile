FC = $(shell nf-config --fc)
FFLAGS = -O3
FCINCLUDES = $(shell nf-config --fflags)
RPATH = $(shell nf-config --flibs | grep -o -e '-L\S\+\( \|$$\)' | sed 's/^-L/-Wl,-rpath,/' | tr -d '\n')
RPATH += $(shell nc-config --libs | grep -o -e '-L\S\+\( \|$$\)' | sed 's/^-L/-Wl,-rpath,/' | tr -d '\n')
FCLIBS = -L$(shell nc-config --libdir) $(shell nf-config --flibs) $(RPATH)


CHECKS = config_check netcdf_check netcdf4_check inq_varids_check
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


# Check that the nf-config program is available
config_check:
	@ printf "Checking for nf-config... "
	@ nf-config --fc > /dev/null 2>&1; \
	if [ $$? -ne 0 ]; then \
		printf "\n\nError: nf-config not found in \$$PATH, or it is not working correctly.\n"; \
		printf "       The nf-config program is typically found in a bin/ sub-directory\n"; \
		printf "       of the installation path of the NetCDF library. If you have set\n"; \
		printf "       the \$$NETCDF environment variable to the NetCDF installation path,\n"; \
		printf "       adding \$${NETCDF}/bin to your \$$PATH is usually sufficient.\n\n"; \
		exit 1; \
	fi
	@ printf "OK\n"

# Check whether the NetCDF Fortran 90 library interface is available
netcdf_check: config_check
	@ printf "Checking for NetCDF Fortran 90 library interface... "
	@ fname=$$(mktemp varidXXXX.f90); \
	printf "program foo; use netcdf, only : nf90_open; end program foo\n" >> $$fname; \
	$(FC) $(FFLAGS) $(FCINCLUDES) $$fname $(FCLIBS) > /dev/null 2>&1; \
	if [ $$? -ne 0 ]; then \
		printf "\n\nError: Could not compile a test program using the NetCDF Fortran library.\n"; \
		printf "       Failed compilation command was:\n\n"; \
		printf "       $(FC) $(FFLAGS) $(FCINCLUDES) $$fname $(FCLIBS)\n\n"; \
		printf "       where $$fname contained the following program:\n\n"; \
		cat $$fname; \
		printf "\n"; \
		printf "       Compilation produced the following error:\n\n"; \
		$(FC) $(FFLAGS) $(FCINCLUDES) $$fname $(FCLIBS) 2>&1; \
		printf "\n"; \
		rm $$fname; \
		exit 1; \
	else \
		rm $$fname a.out; \
	fi
	@ printf "OK\n"

# Check whether the NetCDF library supports NetCDF4/HDF5 format
netcdf4_check: netcdf_check
	@ printf "Checking for NetCDF4 support... "
	$(eval FFLAGS_VARID := $(shell $\
		fname=$$(mktemp varidXXXX.f90); $\
		printf "program foo; use netcdf, only : nf90_netcdf4; end program foo\n" >> $$fname; $\
		$(FC) $(FFLAGS) $(FCINCLUDES) -c $$fname > /dev/null 2>&1; $\
		if [ $$? -eq 0 ]; then $\
			printf -- "-DHAVE_NETCDF4_SUPPORT"; $\
			rm $${fname%.f90}.o; $\
		fi; $\
		rm $$fname $\
	))
	@ printf $(if $(FFLAGS_VARID), "OK (Adding -DHAVE_NETCDF4_SUPPORT to FFLAGS)\n", "Not available\n")
	$(eval FFLAGS += $(FFLAGS_VARID))

# Check whether the NetCDF library supports nf90_inq_varids
inq_varids_check: netcdf4_check
	@ printf "Checking for nf90_inq_varids... "
	$(eval FFLAGS_VARID := $(shell $\
		fname=$$(mktemp varidXXXX.f90); $\
		printf "program foo; use netcdf, only : nf90_inq_varids; end program foo\n" >> $$fname; $\
		$(FC) $(FFLAGS) $(FCINCLUDES) -c $$fname > /dev/null 2>&1; $\
		if [ $$? -eq 0 ]; then $\
			printf -- "-DHAVE_NF90_INQ_VARIDS"; $\
			rm $${fname%.f90}.o; $\
		fi; $\
		rm $$fname $\
	))
	@ printf $(if $(FFLAGS_VARID), "OK (Adding -DHAVE_NF90_INQ_VARIDS to FFLAGS)\n", "Not available\n")
	$(eval FFLAGS += $(FFLAGS_VARID))
