#include make.inc

BINDIR=${PWD}/bin

all: path include lib bin default COSMO CLM COSMO_VIEW
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
	(cd modules; python setup.py build; python setup.py install)
	(cp cosmo-view/cosmo-view $(BINDIR))
	(cd $(BINDIR); chmod +x cosmo-view)

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

