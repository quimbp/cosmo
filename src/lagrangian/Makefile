include ../../make.inc

TARGET = lagrangian

OBJ = hinterpol.o floats.o \
      fillcoast.o mod_out.o clm.o help.o udf.o main.o

#FFLAGS = -fbounds-check
#FFLAGS = -O4
%.o: %.f90
	$(FC) $(FFLAGS) -c $< -I../../include $(CDFINC)

all: $(OBJ)
	$(FC) $(FFLAGS) -o $(TARGET) $(OBJ) -L../../lib -lcosmo $(CDFLIB)
	$(INSTALL) $(TARGET) ../../bin

clean:
	rm -f $(OBJ) *.mod $(TARGET) ../../bin/$(TARGET)
