include ../../path.inc
include ../../make.inc

BINDIR = $(COSMO)/bin
LIBDIR = $(COSMO)/lib
INCDIR = $(COSMO)/include

INC = $(CDFINC) -I$(INCDIR)
LIB = -L$(LIBDIR) -lcosmo -L$(CDFLIB) -lnetcdff
TARGET = clm
#FFLAGS = -fbounds-check

OBJ = options.o forcing.o float.o model.o out.o rk5.o help.o

all: main.f90 $(OBJ)
	$(FC) $(FFLAGS) -o $(TARGET) $(OBJ) $< $(INC) $(LIB)
	$(INSTALL) $(TARGET) ../../bin


clean:
	rm -f *.mod *.o $(TARGET)

options.o: options.f90 
	$(FC) $(FFLAGS) -c $< $(INC)

forcing.o: forcing.f90 
	$(FC) $(FFLAGS) -c $< $(INC)

model.o: model.f90 out.o options.o float.o
	$(FC) $(FFLAGS) -c $< $(INC)

out.o: out.f90 options.o
	$(FC) $(FFLAGS) -c $< $(INC)

float.o: float.f90 forcing.o options.o
	$(FC) $(FFLAGS) -c $< $(INC)

help.o: help.f90 
	$(FC) $(FFLAGS) -c $< $(INC)

rk5.o: rk5.f90 model.o
	$(FC) $(FFLAGS) -c $< $(INC)
