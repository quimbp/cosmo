include ../../path.inc
include ../../make.inc
#
BINDIR = $(COSMO)/bin
LIBDIR = $(COSMO)/lib
INCDIR = $(COSMO)/include

TARGET = $(LIBDIR)/libcosmo.a
INC = $(CDFINC) -I$(INCDIR)

all: $(TARGET)
OBJ = types.o constants.o status.o random.o utils.o math.o lineargs.o help.o rungekutta.o ascii.o datetime.o \
      grid.o json.o plot.o cosmo.o

%.o: %.f90
	$(FC) $(FFLAGS) -c $< $(INC)

clean:
	rm -f $(INCDIR)/*.mod
	rm -f *.mod *.o $(TARGET)

$(TARGET): $(OBJ)
	rm -f $(TARGET)
	$(INSTALL) *.mod $(INCDIR)
	$(AR) $(TARGET) $(OBJ)

types.o: types.f90
	$(FC) $(FFLAGS) -c $< $(INC)

constants.o: constants.f90 types.o
	$(FC) $(FFLAGS) -c $< $(INC)

status.o: status.f90
	$(FC) $(FFLAGS) -c $< $(INC)

random.o: random.f90 status.o constants.o types.o
	$(FC) $(FFLAGS) -c $< $(INC)

utils.o: utils.f90 random.o types.o
	$(FC) $(FFLAGS) -c $< $(INC)

math.o: math.f90 status.o constants.o types.o utils.o
	$(FC) $(FFLAGS) -c $< $(INC)

lineargs.o: lineargs.f90 utils.o constants.o types.o
	$(FC) $(FFLAGS) -c $< $(INC)

help.o: help.f90 utils.o
	$(FC) $(FFLAGS) -c $< $(INC)

rungekutta.o: rungekutta.f90 types.o
	$(FC) $(FFLAGS) -c $< $(INC)

ascii.o: ascii.f90 utils.o types.o
	$(FC) $(FFLAGS) -c $< $(INC)

datetime.o: datetime.f90 status.o utils.o types.o
	$(FC) $(FFLAGS) -c $< $(INC)

grid.o: grid.f90 datetime.o status.o utils.o math.o constants.o types.o
	$(FC) $(FFLAGS) -c $< $(INC)

json.o: json.f90 types.o
	$(FC) $(FFLAGS) -c $< $(INC)

plot.o: plot.f90 types.o
	$(FC) $(FFLAGS) -c $< $(INC)

cosmo.o: cosmo.f90 types.o constants.o status.o ascii.o utils.o datetime.o math.o grid.o rungekutta.o json.o
	$(FC) $(FFLAGS) -c $< $(INC)

