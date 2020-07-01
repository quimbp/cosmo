include ../../path.inc
include ../../make.inc

.SUFFIXES:.f90

BINDIR = $(COSMO)/bin
LIBDIR = $(COSMO)/lib
INCDIR = $(COSMO)/include

LIB = -L$(LIBDIR) -lcosmo $(CDFLIB)
INC = -I$(INCDIR) $(CDFINC)

TARGETS = floatpos streamline

.f90: 
	$(FC) $(FFLAGS) -o $@ $@.f90 $(INC) $(LIB)
	$(INSTALL) $@ $(BINDIR)


all: $(TARGETS) 

clean:
	rm -f $(TARGETS) *.o *.mod
	cd $(BINDIR); rm -f $(TARGETS)
