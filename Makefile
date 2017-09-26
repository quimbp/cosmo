include make.inc

all: include lib bin LIB LAGRANGIAN
	@echo "Done"

include:
	mkdir $@

lib:
	mkdir $@

bin:
	mkdir $@

LIB:
	@echo 
	@echo "=============================================="
	@echo "Compiling the LIB library"
	@echo "=============================================="
	@echo 
	(cd src/lib/; make clean; make all)

LAGRANGIAN:
	@echo 
	@echo "=============================================="
	@echo "Compiling LAGRANGIAN"
	@echo "=============================================="
	@echo 
	(cd src/lagrangian/; make clean; make all)

clean:
	(cd src/lib; make clean)
	(cd src/lagrangian; make clean)

