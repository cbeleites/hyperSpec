all: chondro.pdf

chondro.pdf: chondro.Rnw vignettes.defs rawdata/chondro.txt
	R CMD Sweave chondro.Rnw --pdf --compact="both" 

vignettes.defs: ../vignettes.defs
	cp -av $< $@

rawdata/chondro.txt: ../fileio/txt.Renishaw/chondro.txt | rawdata 
	$(MAKE) -C ../fileio txt.Renishaw/chondro.txt
	cp -av $< $@

rawdata:
	mkdir rawdata


