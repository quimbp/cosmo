include make.inc

all: LIB LAGRANGIAN
	@echo "Done"

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
	(cd src/lib/; make clean)
	(cd src/tools/; make clean)
	(cd lib/; rm -f *)
	(cd include/; rm -f *)
	(cd bin/; rm -f *)

