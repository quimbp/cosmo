include ../../path.inc
include ../../make.inc

BINDIR = $(COSMO)/bin
LIBDIR = $(COSMO)/lib
INCDIR = $(COSMO)/include

TARGET = $(LIBDIR)/libcosmo.a
INC = $(CDFINC) -I$(INCDIR)

#FFLAGS = -fno-range-check  -fbounds-check
all: cosmo
OBJ = types.o constants.o utils.o lineargs.o help.o dates.o cdf.o geocdf.o \
      runge_kutta.o math.o lorenz.o ocean.o geojson.o roms.o cosmo.o

%.o: %.f90
	$(FC) $(FFLAGS) -c $< $(INC)

clean:
	echo $(COSMO)
	rm -f *.mod *.o $(TARGET)
	rm -f $(INCDIR)/*.mod
	rm -f $(LIBDIR)/*.a

cosmo: $(OBJ)
	echo $(COSMO)
	/bin/rm -f $(TARGET)
	$(INSTALL) *.mod $(INCDIR)
	$(AR) $(TARGET) *.o

