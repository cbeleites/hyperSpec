all: baseline.pdf

clean:
	rm -f *-fig-*.pdf *.aux *.bbl *.blg *.out *.log *-pkg.bib *.toc *.idx *.ilg *.ind *.rda
	rm -f *~ .*~ \#*\#

baseline.pdf: baseline.Rnw vignettes.defs
	R CMD Sweave baseline.Rnw --pdf --compact="both" 

vignettes.defs: ../vignettes.defs
	cp -av $< $@
