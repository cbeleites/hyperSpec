all: plotting.pdf

plotting.pdf: plotting.Rnw vignettes.defs
	R CMD Sweave plotting.Rnw --clean --pdf --compact="both" 

vignettes.defs: ../vignettes.defs
	cp -av $< $@

