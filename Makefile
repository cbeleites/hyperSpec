all: roxygenize vignettes pkg-data

superclean: .FORCE
	git clean -q -f

install: roxygenize installdev vignettes 

installdev: roxygenize pkg-vignettes
	R CMD INSTALL hyperSpec --with.keep-source --fake --no-docs --no-build-vignettes

# Code building
build: roxygenize pkg-vignettes
	R CMD build hyperSpec

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
	cd hyperSpec/vignettes && $(MAKE) -f Makefile-localbaseline.Rnw

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
	cd hyperSpec/vignettes && $(MAKE) -f Makefile-local flu.Rnw

# introduction .....................................................................................

introduction:
	cd Vignettes/introduction && $(MAKE)
	cd hyperSpec/vignettes && $(MAKE) -f Makefile-local introduction.Rnw

# laser ............................................................................................

laser:
	cd Vignettes/laser && $(MAKE)
	cd hyperSpec/vignettes && $(MAKE) -f Makefile-local laser.Rnw

# plotting .........................................................................................

plotting:
	cd Vignettes/plotting && $(MAKE)
	cd hyperSpec/vignettes && $(MAKE) -f Makefile-local plotting.Rnw

# vignettes in package folder ----------------------------------------------------------------------

pkg-vignettes: 
	cd hyperSpec/inst/doc && $(MAKE) # for fileio.pdf and chondro.pdf
	cd hyperSpec/vignettes && $(MAKE) -f Makefile-local # do not use Makefile here as 
	                                                    # tools::buildVignettes will attempt to use it.
	                                                    # (even if .Rbuildignore lists the Makefile!)

# package data --------------------------------------------------------------------------------------

pkg-data:
	cd hyperSpec/data && $(MAKE)

# Vignette zips -------------------------------------------------------------------------------------

chondro.zip:
	$(error TODO)

fileio.zip:
	$(error TODO)
