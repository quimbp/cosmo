include ../../make.inc

TARGET = $(LIBDIR)/libjsonfortran.a
INC = $(CDFINC) -I$(INCDIR)

all: cosmo
OBJ = json_kinds.o json_parameters.o json_string_utilities.o \
      json_value_module.o json_file_module.o json_module.o 

%.o: %.F90
	$(FC) $(FFLAGS) -c $< $(INC)

clean:
	rm -f *.mod *.o $(TARGET)
	rm -f $(INCDIR)/*
	rm -f $(LIBDIR)/*

cosmo: $(OBJ)
	/bin/rm -f $(TARGET)
	$(INSTALL) json_module.mod $(INCDIR)
	$(AR) $(TARGET) *.o

