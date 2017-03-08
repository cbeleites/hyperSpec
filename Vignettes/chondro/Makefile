all: chondro.pdf

clean:
	rm -f *-fig-*.pdf *.aux *.bbl *.blg *.out *.log *-pkg.bib *.toc *.idx *.ilg *.ind *.rda
	rm -f *~ .*~ \#*\#

chondro.pdf: chondro.Rnw vignettes.defs rawdata/chondro.txt
	R CMD Sweave chondro.Rnw --pdf --compact="both" 

vignettes.defs: ../vignettes.defs
	cp -av $< $@

rawdata/chondro.txt: ../fileio/txt.Renishaw/chondro.txt | rawdata 
	$(MAKE) -C ../fileio txt.Renishaw/chondro.txt
	cp -av $< $@

rawdata:
	mkdir rawdata


