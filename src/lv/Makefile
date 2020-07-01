include ../../path.inc
include ../../make.inc

BINDIR = $(COSMO)/bin
LIBDIR = $(COSMO)/lib
INCDIR = $(COSMO)/include

FFLAGS = -fbounds-check
TARGET = lpv

OBJ = hinterpol.o floats.o \
      fillcoast.o out.o clm.o help.o udf.o interpol3d.o main.o

%.o: %.f90
	$(FC) $(FFLAGS) -c $< -I../../include $(CDFINC)

all: $(OBJ)
	$(FC) $(FFLAGS) -o $(TARGET) $(OBJ) -L../../lib -lcosmo $(CDFLIB)
	$(INSTALL) $(TARGET) ../../bin

clean:
	rm -f $(OBJ) *.mod $(TARGET) ../../bin/$(TARGET)
