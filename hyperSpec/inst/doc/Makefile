all: vignettes.defs fileio.pdf chondro.pdf 

vignettes.defs: ../../../Vignettes/vignettes.defs
	cp -av $< $@

chondro.pdf: 
	$(MAKE) -C ../../../Vignettes/chondro/ chondro.pdf
	cp -av ../../../Vignettes/chondro/chondro.pdf $@

fileio.pdf: 
	$(MAKE) -C ../../../Vignettes/fileio fileio.pdf
	cp -av ../../../Vignettes/fileio/fileio.pdf $@

