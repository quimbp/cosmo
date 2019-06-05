include make.inc

all: include lib bin COSMO BLM MLM TOOLS COSMO_VIEW
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

TOOLS:
	@echo 
	@echo "=============================================="
	@echo "Compiling TOOLS"
	@echo "=============================================="
	@echo 
	(cd src/tools/; make clean; make all)

COSMO_VIEW:
	@echo 
	@echo "=============================================="
	@echo "COSMO-VIEW"
	@echo "=============================================="
	@echo 
	rm -f modules/cosmo/__init__.py
	sed 's@#COSMO_ROOT.*@COSMO_ROOT="'"${PWD}"'"@' modules/cosmo/__init__.template > modules/cosmo/__init__.py
	(cd conf/default/; rm -f *.conf)
	(cd modules; sudo python3 setup.py build; sudo python3 setup.py install)
	(cp cosmo-view/cosmo-view $(BINDIR))
	(cd $(BINDIR); chmod +x cosmo-view)

clean:
	(cd src/lib; make clean)
	(cd src/blm; make clean)
	(cd src/mlm; make clean)
	(cd src/tools; make clean)
	(cd bin/; rm -f cosmo-view)
	(cd conf/default/; rm -f *.conf)
	(cd modules; sudo rm -fr build)
	(cd modules/cosmo; rm -f __init__.py)

