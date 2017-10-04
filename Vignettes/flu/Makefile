all: flu.pdf

clean:
	rm -f *-fig-*.pdf *.aux *.bbl *.blg *.out *.log *-pkg.bib *.toc *.idx *.ilg *.ind *.rda
	rm -f *~ .*~ \#*\#

flu.pdf: flu.Rnw vignettes.defs read.txt.PerkinElmer.R fludata
	R CMD Sweave flu.Rnw --pdf --compact="both" 

vignettes.defs: ../vignettes.defs
	cp -av $< $@

read.txt.PerkinElmer.R: ../fileio/read.txt.PerkinElmer.R
	cp -av $< $@

FLU = rawdata/flu1.txt rawdata/flu2.txt rawdata/flu3.txt rawdata/flu4.txt rawdata/flu5.txt \
          rawdata/flu6.txt

fludata: $(FLU) | rawdata 


$(FLU):
	$(MAKE) -C ../fileio txt.PerkinElmer/$(notdir $@)
	cp -av ../fileio/txt.PerkinElmer/$(notdir $@) $@

rawdata:
	mkdir rawdata


