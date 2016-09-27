all: vignettes.defs fileio.pdf chondro.pdf 

vignettes.defs: ../../../Vignettes/vignettes.defs
	cp -av $< $@

chondro.pdf: 
	cd ../../../Vignettes/chondro/ && $(MAKE) chondro.pdf
	cp -av ../../../Vignettes/chondro/chondro.pdf $@

fileio.pdf: 
	cd ../../../Vignettes/fileio && $(MAKE) fileio.pdf
	cp -av ../../../Vignettes/fileio/fileio.pdf $@

