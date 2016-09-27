all: roxygenize vignettes pkg-data

superclean: .FORCE
	git clean -q -f

install: roxygenize installdev vignettes 

installdev: roxygenize pkg-vignettes
	R CMD INSTALL hyperSpec --with.keep-source --fake --no-docs --no-build-vignettes

# Code building

roxygenize: hyperSpec/DESCRIPTION hyperSpec/R/*.R 
	Rscript --vanilla -e "library (roxygen2); roxygenize ('hyperSpec')" 
# TODO: DESCRIPTION target 
# add dependency clean

# VIGNETTES ########################################################################################

vignettes: baseline chondro flu laser plotting 
# fileio introduction laser plotting

# in subdirs ---------------------------------------------------------------------------------------

# baseline .........................................................................................

baseline:
	cd Vignettes/baseline && $(MAKE)
	cd hyperSpec/vignettes && $(MAKE) baseline.Rnw

# chondro ..........................................................................................

chondro:
	cd Vignettes/chondro && $(MAKE)
	cd hyperSpec/inst/doc && $(MAKE) chondro.pdf

#	cd $(dir $<) &&	R CMD Sweave chondro.Rnw --clean --pdf --compact="both" --quiet

# fileio ...........................................................................................

fileio:
	cd Vignettes/fileio && $(MAKE)
	cd hyperSpec/inst/doc && $(MAKE) fileio.pdf

# flu ..............................................................................................

flu:
	cd Vignettes/flu && $(MAKE)
	cd hyperSpec/vignettes && $(MAKE) flu.Rnw

# introduction .....................................................................................

introduction:
	cd Vignettes/introduction && $(MAKE)
	cd hyperSpec/vignettes && $(MAKE) introduction.Rnw

# laser ............................................................................................

laser:
	cd Vignettes/laser && $(MAKE)
	cd hyperSpec/vignettes && $(MAKE) laser.Rnw

# plotting .........................................................................................

plotting:
	cd Vignettes/plotting && $(MAKE)
	cd hyperSpec/vignettes && $(MAKE) plotting.Rnw

# vignettes in package folder ----------------------------------------------------------------------

pkg-vignettes: 
	cd hyperSpec/inst/doc && $(MAKE) # for fileio.pdf and chondro.pdf
	cd hyperSpec/vignettes && $(MAKE)


# package data --------------------------------------------------------------------------------------

pkg-data:
	cd hyperSpec/data && $(MAKE)

# Vignette zips -------------------------------------------------------------------------------------

chondro.zip:
	$(error TODO)

fileio.zip:
	$(error TODO)
