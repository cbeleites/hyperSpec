all: introduction.pdf

introduction.pdf: introduction.Rnw vignettes.defs strukturhyperspec.pdf
	R CMD Sweave introduction.Rnw --clean --pdf --compact="both" 

strukturhyperspec.pdf: strukturhyperspec.tex
	pdflatex $<

vignettes.defs: ../vignettes.defs
	cp -av $< $@
