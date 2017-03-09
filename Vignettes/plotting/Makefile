all: plotting.pdf

clean:
	rm -f *-fig-*.pdf *.aux *.bbl *.blg *.out *.log *-pkg.bib *.toc *.idx *.ilg *.ind *.rda 
	rm -f *~ .*~ \#*\#

plotting.pdf: plotting.Rnw vignettes.defs
	R CMD Sweave plotting.Rnw --clean --pdf --compact="both" 

vignettes.defs: ../vignettes.defs
	cp -av $< $@

