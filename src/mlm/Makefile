include ../../path.inc
include ../../make.inc

BINDIR = $(COSMO)/bin
LIBDIR = $(COSMO)/lib
INCDIR = $(COSMO)/include

TARGET = mlm

OBJ = mlm.o 

%.o: %.f90
	$(FC) $(FFLAGS) -c $< $(CDFINC)

all: $(OBJ)
	$(FC) $(FFLAGS) -o $(TARGET) $(OBJ) $(CDFLIB)
	$(INSTALL) $(TARGET) ../../bin

clean:
	rm -f $(OBJ) $(TARGET) ../../bin/$(TARGET)
