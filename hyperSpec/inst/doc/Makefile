# NOTE: This file is automatically and erroneously deleted when running the package check from RStudio (instead of R CMD check from command line).
# REVERT ANY SUCH DELETION

all: vignettes.defs fludata laserdata

clean:
	rm -f *~ .*~ \#*\#

vignettes.defs: ../../../Vignettes/vignettes.defs
	cp -av $< $@

fludata: ../../../Vignettes/flu/rawdata | rawdata
	cp -av $< .
	rm -f rawdata/Makefile

laserdata: ../../../Vignettes/fileio/txt.Renishaw/laser.txt.gz | rawdata
	cp $< rawdata/

rawdata:
	mkdir $@
