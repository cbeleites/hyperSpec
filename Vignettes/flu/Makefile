all: flu.pdf

flu.pdf: flu.Rnw vignettes.defs scan.txt.PerkinElmer.R fludata
	R CMD Sweave flu.Rnw --clean --pdf --compact="both" 

vignettes.defs: ../vignettes.defs
	cp -av $< $@

scan.txt.PerkinElmer.R: ../fileio/scan.txt.PerkinElmer.R
	cp -av $< $@

FLU = rawdata/flu1.txt rawdata/flu2.txt rawdata/flu3.txt rawdata/flu4.txt rawdata/flu5.txt \
          rawdata/flu6.txt

fludata: $(FLU) | rawdata 


$(FLU):
	cd ../fileio/ && $(MAKE) txt.PerkinElmer/$(notdir $@)
	cp -av ../fileio/txt.PerkinElmer/$(notdir $@) $@

rawdata:
	mkdir rawdata


