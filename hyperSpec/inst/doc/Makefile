all: vignettes.defs fileio.pdf chondro.pdf fludata laserdata

clean:
	rm -f *~ .*~ \#*\#

vignettes.defs: ../../../Vignettes/vignettes.defs
	cp -av $< $@

chondro.pdf:
	$(MAKE) -C ../../../Vignettes/chondro/ chondro.pdf
	cp -av ../../../Vignettes/chondro/chondro.pdf $@

fileio.pdf: ../../../Vignettes/fileio/fileio.pdf
	$(MAKE) -C ../../../Vignettes/fileio fileio.pdf
	cp -av ../../../Vignettes/fileio/fileio.pdf $@

fludata: ../../../Vignettes/flu/rawdata | rawdata
	cp -av $< .
	rm -f rawdata/Makefile

laserdata: ../../../Vignettes/fileio/txt.Renishaw/laser.txt.gz | rawdata
	cp $< rawdata/

rawdata:
	mkdir $@
