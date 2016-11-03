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
	$(MAKE) -C Vignettes/baseline 
	$(MAKE) -C hyperSpec/vignettes -f Makefile-local baseline.Rnw

# chondro ..........................................................................................

chondro:
	$(MAKE) -C Vignettes/chondro 
	$(MAKE) -C hyperSpec/inst/doc  chondro.pdf

#	cd $(dir $<) &&	R CMD Sweave chondro.Rnw --clean --pdf --compact="both" --quiet

# fileio ...........................................................................................

fileio:
	$(MAKE) -C Vignettes/fileio 
	$(MAKE) -C hyperSpec/inst/doc  fileio.pdf

# flu ..............................................................................................

flu:
	$(MAKE) -C Vignettes/flu 
	$(MAKE) -C hyperSpec/vignettes -f Makefile-local flu.Rnw

# introduction .....................................................................................

introduction:
	$(MAKE) -C Vignettes/introduction 
	$(MAKE) -C hyperSpec/vignettes -f Makefile-local introduction.Rnw

# laser ............................................................................................

laser:
	$(MAKE) -C Vignettes/laser 
	$(MAKE) -C hyperSpec/vignettes -f Makefile-local laser.Rnw

# plotting .........................................................................................

plotting:
	$(MAKE) -C Vignettes/plotting 
	$(MAKE) -C hyperSpec/vignettes -f Makefile-local plotting.Rnw

# vignettes in package folder ----------------------------------------------------------------------

pkg-vignettes: 
	$(MAKE) -C hyperSpec/inst/doc  # for fileio.pdf and chondro.pdf
	$(MAKE) -C hyperSpec/vignettes -f Makefile-local # do not use Makefile here as 
	                                                 # tools::buildVignettes will attempt to use it.
	                                                 # (even if .Rbuildignore lists the Makefile!)

# package data --------------------------------------------------------------------------------------

pkg-data:
	$(MAKE) -C hyperSpec/data 

# Vignette zips -------------------------------------------------------------------------------------

chondro.zip:
	$(error TODO)

fileio.zip:
	$(error TODO)
