all: laser.pdf

laser.pdf: laser.Rnw vignettes.defs  rawdata/laser.txt.gz fig-3D.png
	R CMD Sweave laser.Rnw --clean --pdf --compact="both" 

vignettes.defs: ../vignettes.defs
	cp -av $< $@

fig-3D.png: rawdata/laser.txt.gz vignettes.defs
	R CMD Sweave laser.Rnw --pdf 

rawdata:
	mkdir rawdata

rawdata/laser.txt.gz: | rawdata
	$(MAKE) -C ../fileio txt.Renishaw/laser.txt.gz
	cp ../fileio/txt.Renishaw/laser.txt.gz $@ 
