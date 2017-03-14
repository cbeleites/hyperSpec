all: hyperspec.pdf

clean:
	rm -f *-fig-*.pdf *.aux *.bbl *.blg *.out *.log *-pkg.bib *.toc *.idx *.ilg *.ind *.rda
	rm -f *~ .*~ \#*\#

hyperspec.pdf: hyperspec.Rnw vignettes.defs strukturhyperspec.pdf
	R CMD Sweave hyperspec.Rnw --clean --pdf --compact="both" 

strukturhyperspec.pdf: strukturhyperspec.tex
	pdflatex $<

vignettes.defs: ../vignettes.defs
	cp -av $< $@
