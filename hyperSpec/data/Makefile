all: barbiturates.rda paracetamol.rda flu.rda laser.rda ../R/sysdata.rda

barbiturates.rda:
	$(MAKE) -C ../../Vignettes/fileio 
	cp ../../Vignettes/fileio/barbiturates.rda $@

paracetamol.rda:
	$(MAKE) -C ../../Vignettes/fileio 
	cp ../../Vignettes/fileio/paracetamol.rda $@

flu.rda:
	$(MAKE) -C ../../Vignettes/flu 
	cp ../../Vignettes/flu/flu.rda $@

laser.rda:
	$(MAKE) -C ../../Vignettes/laser 
	cp ../../Vignettes/laser/laser.rda $@

# for some reason, internal data needs to be in R/sysdata.rda
../R/sysdata.rda:
	$(MAKE) -C ../../Vignettes/chondro 
	cp ../../Vignettes/chondro/chondro-internal.rda $@
