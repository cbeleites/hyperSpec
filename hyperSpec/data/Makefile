all: barbiturates.rda paracetamol.rda flu.rda laser.rda 

barbiturates.rda:
	cd ../../Vignettes/fileio && $(MAKE)
	cp ../../Vignettes/fileio/barbiturates.rda $@

paracetamol.rda:
	cd ../../Vignettes/fileio && $(MAKE)
	cp ../../Vignettes/fileio/paracetamol.rda $@

flu.rda:
	cd ../../Vignettes/flu && $(MAKE)
	cp ../../Vignettes/flu/flu.rda $@

laser.rda:
	cd ../../Vignettes/laser && $(MAKE)
	cp ../../Vignettes/laser/laser.rda $@

# for some reason, internal data needs to be in R/sysdata.rda
../R/sysdata.rda:
	cd ../../Vignettes/chondro && $(MAKE)
	cp ../../Vignettes/chondro/chondro-internal.rda $@
