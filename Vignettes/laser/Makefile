all: laser.pdf

laser.pdf: laser.Rnw vignettes.defs fig-3D.png | rawdata rawdata/laser.txt.gz
	R CMD Sweave laser.Rnw --clean --pdf --compact="both" 

vignettes.defs: ../vignettes.defs
	cp -av $< $@

fig-3D.png:
	R CMD Sweave laser.Rnw --clean --pdf --compact="both" 

rawdata:
	mkdir rawdata

rawdata/laser.txt.gz: 
	$(MAKE) -C ../fileio txt.Renishaw/laser.txt.gz
	cp ../fileio/txt.Renishaw/laser.txt.gz $@ 
