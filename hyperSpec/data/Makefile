all: barbiturates.rda paracetamol.rda flu.rda laser.rda ../R/sysdata.rda

barbiturates.rda: ../../Vignettes/fileio/barbiturates.rda
	cp ../../Vignettes/fileio/barbiturates.rda $@
../../Vignettes/fileio/barbiturates.rda:
	$(MAKE) -C ../../Vignettes/fileio 

paracetamol.rda: ../../Vignettes/fileio/paracetamol.rda
	cp ../../Vignettes/fileio/paracetamol.rda $@
../../Vignettes/fileio/paracetamol.rda:
	$(MAKE) -C ../../Vignettes/fileio 

flu.rda: ../../Vignettes/flu/flu.rda
	cp ../../Vignettes/flu/flu.rda $@
../../Vignettes/flu/flu.rda:
	$(MAKE) -C ../../Vignettes/flu 

laser.rda: ../../Vignettes/laser/laser.rda
	cp ../../Vignettes/laser/laser.rda $@
../../Vignettes/laser/laser.rda:
	$(MAKE) -C ../../Vignettes/laser

# for some reason, internal data needs to be in R/sysdata.rda
../R/sysdata.rda: ../../Vignettes/chondro/chondro-internal.rda
	cp ../../Vignettes/chondro/chondro-internal.rda $@
../../Vignettes/chondro/chondro-internal.rda:
	$(MAKE) -C ../../Vignettes/chondro 
