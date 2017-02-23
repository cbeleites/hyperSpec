all: baseline.pdf

baseline.pdf: baseline.Rnw vignettes.defs
	R CMD Sweave baseline.Rnw --pdf --compact="both" 

vignettes.defs: ../vignettes.defs
	cp -av $< $@
