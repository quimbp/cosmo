#include make.inc

BINDIR=${PWD}/bin

all: path include lib bin default COSMO CLM COSMO_VIEW CFORT
	@echo "Done"

path:
	rm -f path.inc
	echo "COSMO = ${PWD}" > path.inc

include:
	@echo creating folder $@
	mkdir -p $@

lib:
	@echo creating folder $@
	mkdir -p $@

bin:
	@echo creating folder $@
	mkdir -p $@

default:
	@echo creating folder conf/default
	mkdir -p conf/default 

COSMO:
	@echo 
	@echo "=============================================="
	@echo "Compiling the COSMO library"
	@echo "=============================================="
	@echo 
	(cd src/lib/; make clean; make all)

BLM:
	@echo 
	@echo "=============================================="
	@echo "Compiling BLM"
	@echo "=============================================="
	@echo 
	(cd src/blm/; make clean; make all)

MLM:
	@echo 
	@echo "=============================================="
	@echo "Compiling MLM"
	@echo "=============================================="
	@echo 
	(cd src/mlm/; make clean; make all)

CLM:
	@echo 
	@echo "=============================================="
	@echo "Compiling CLM"
	@echo "=============================================="
	@echo 
	(cd src/clm/; make clean; make all)

TOOLS:
	@echo 
	@echo "=============================================="
	@echo "Compiling TOOLS"
	@echo "=============================================="
	@echo 
	(cd src/tools/; make clean; make all)

COSMO_VIEW:
	@echo $(COSMO)
	@echo ${PWD}
	@echo ${BINDIR}
	@echo 
	@echo "=============================================="
	@echo "COSMO-VIEW"
	@echo "=============================================="
	@echo 
	rm -f modules/cosmo/__init__.py
	sed 's@#COSMO_ROOT.*@COSMO_ROOT="'"${PWD}"'"@' modules/cosmo/__init__.template > modules/cosmo/__init__.py
	(cd conf/default/; rm -f *.conf)
	(cd modules; python3 setup.py build; python3 setup.py install)
	(cp cosmo-view/cosmo-view $(BINDIR))
	(cd $(BINDIR); chmod +x cosmo-view)

CFORT:
	@echo 
	@echo "=============================================="
	@echo "CFORT"
	@echo "=============================================="
	@echo 
	sed 's@TARGET_COSMO=.*@COSMO='${PWD}'@' cosmo-view/cfort.template > cosmo-view/cfort.1
	sed 's@TARGET_FC=.*@FC='${FC}'@' cosmo-view/cfort.1 > cosmo-view/cfort.2
	sed 's@TARGET_FFLAGS=.*@FFLAGS='"${FFLAGS}"'@' cosmo-view/cfort.2 > cosmo-view/cfort.3
	sed 's@TARGET_NF90_INC=.*@NF90_INC='${NF90_INC}'@' cosmo-view/cfort.3 > cosmo-view/cfort.4
	sed 's@TARGET_NF90_LIB=.*@NF90_LIB='${NF90_LIB}'@' cosmo-view/cfort.4 > cosmo-view/cfort
	mv cosmo-view/cfort bin/
	chmod +x bin/cfort
	rm -f cosmo-view/cfort.?

clean:
	(cd src/lib; make clean)
	(cd src/blm; make clean)
	(cd src/mlm; make clean)
	(cd src/tools; make clean)
	(cd bin/; rm -f cosmo-view)
	(cd conf/default/; rm -f *.conf)
	(cd modules; rm -fr build)
	(cd modules/cosmo; rm -f __init__.py)
	(rm -f path.inc)

