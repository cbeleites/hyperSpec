all: introduction.pdf

clean:
	rm -f *-fig-*.pdf *.aux *.bbl *.blg *.out *.log *-pkg.bib *.toc *.idx *.ilg *.ind *.rda
	rm -f *~ .*~ \#*\#

introduction.pdf: introduction.Rnw vignettes.defs strukturhyperspec.pdf
	R CMD Sweave introduction.Rnw --clean --pdf --compact="both" 

strukturhyperspec.pdf: strukturhyperspec.tex
	pdflatex $<

vignettes.defs: ../vignettes.defs
	cp -av $< $@
