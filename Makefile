include make.inc

all: include lib bin COSMO LAGRANGIAN TOOLS
	@echo "Done"

include:
	mkdir $@

lib:
	mkdir $@

bin:
	mkdir $@

COSMO:
	@echo 
	@echo "=============================================="
	@echo "Compiling the COSMO library"
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

TOOLS:
	@echo 
	@echo "=============================================="
	@echo "Compiling TOOLS"
	@echo "=============================================="
	@echo 
	(cd src/tools/; make clean; make all)

clean:
	(cd src/lib; make clean)
	(cd src/lagrangian; make clean)
	(cd src/tools; make clean)

