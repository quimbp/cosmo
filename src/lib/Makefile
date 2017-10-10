include ../../make.inc

TARGET = $(LIBDIR)/libcosmo.a
INC = $(CDFINC) -I$(INCDIR)

all: cosmo
OBJ = types.o constants.o utils.o lineargs.o help.o dates.o cdf.o geocdf.o \
      runge_kutta.o math.o lorenz.o grids.o tokens.o ocean.o cosmo.o

%.o: %.f90
	$(FC) $(FFLAGS) -c $< $(INC)

clean:
	rm -f *.mod *.o $(TARGET)
	rm -f $(INCDIR)/*
	rm -f $(LIBDIR)/*

cosmo: $(OBJ)
	/bin/rm -f $(TARGET)
	$(INSTALL) *.mod $(INCDIR)
	$(AR) $(TARGET) *.o

