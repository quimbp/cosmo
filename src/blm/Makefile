include ../../path.inc
include ../../make.inc

BINDIR = $(COSMO)/bin
LIBDIR = $(COSMO)/lib
INCDIR = $(COSMO)/include

#FFLAGS = -fno-range-check -fbounds-check


TARGET = blm

OBJ = hinterpol.o floats.o grids.o \
      fillcoast.o mod_out.o rhs.o \
      rk5.o clm.o help.o udf.o main.o

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -I../../include $(CDFINC)

all: $(OBJ)
	$(FC) $(FFLAGS) -o $(TARGET) $(OBJ) -L../../lib -lcosmo $(CDFLIB)
	$(INSTALL) $(TARGET) ../../bin

clean:
	rm -f $(OBJ) *.mod $(TARGET) ../../bin/$(TARGET)
