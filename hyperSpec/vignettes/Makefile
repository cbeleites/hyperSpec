all: vignettes.defs baseline.Rnw flu.Rnw introduction.Rnw chondro.pdf fileio.pdf

vignettes.defs: ../../Vignettes/vignettes.defs 
	cp -av $< $@

# baseline

baseline.Rnw: ../../Vignettes/baseline/baseline.Rnw
	cd ../../Vignettes/baseline && $(MAKE)
	cp -av $< $@

# chondro
# for chondro, the vignette is not built as the rawdata is too big for distributing.
# The .Rnw is a stub and the .pdf is provided in inst/doc
chondro.pdf: 
	cd ../inst/doc/ && $(MAKE) chondro.pdf

# fileio
# for fileio, the vignette is not built as the rawdata is too big for distributing.
# The .Rnw is a stub and the .pdf is provided in inst/doc
fileio.pdf: 
	cd ../inst/doc/ && $(MAKE) fileio.pdf

# flu

flu.Rnw:  ../../Vignettes/flu/flu.Rnw scan.txt.PerkinElmer.R fludata | rawdata 
	cd ../../Vignettes/flu && $(MAKE)
	cp -av $< $@

scan.txt.PerkinElmer.R: ../../Vignettes/fileio/scan.txt.PerkinElmer.R
	cp -av $< $@

fludata: ../../Vignettes/flu/rawdata/flu*.txt | rawdata
	cp -av $^ rawdata/

rawdata:
	mkdir $@

# introduction

introduction.Rnw: ../../Vignettes/introduction/introduction.Rnw ../../Vignettes/introduction/strukturhyperspec.pdf
	cd ../../Vignettes/introduction && $(MAKE)
	cp -av $^ ./

# laser

laser.Rnw: ../../Vignettes/laser/laser.Rnw rawdata/laser.txt.gz 
	cd ../../Vignettes/laser && $(MAKE)
	cp -av $< $@

rawdata/laser.txt.gz: ../../Vignettes/fileio/txt.Renishaw/laser.txt.gz | rawdata
	cd ../../Vignettes/fileio && $(MAKE) txt.Renishaw/laser.txt.gz
	cp $< $@ 

# plotting

plotting.Rnw: ../../Vignettes/plotting/plotting.Rnw 
	cd ../../Vignettes/plotting && $(MAKE)
	cp -av $^ ./

