all: roxygenize vignettes pkg-data pkg-doc

superclean: .FORCE
	git clean -q -f

install: roxygenize installdev vignettes 

installdev: roxygenize pkg-vignettes
	R CMD INSTALL hyperSpec --with.keep-source --fake --no-docs --no-build-vignettes

# Code building
build: roxygenize pkg-vignettes
	R CMD build hyperSpec

roxygenize: DESCRIPTION hyperSpec/R/*.R 
	Rscript --vanilla -e "library (roxygen2); roxygenize ('hyperSpec')" 
# TODO: DESCRIPTION target 
# add dependency clean

DESCRIPTION: $(shell find hyperSpec -maxdepth 1 -daystart -not -ctime 0 -name "DESCRIPTION") #only if not modified today
	@echo update DESCRIPTION
	sed "s/\(^Version: .*-\)20[0-9][0-9][0-1][0-9][0-3][0-9]\(.*\)$$/\1`date +%Y%m%d`\2/" hyperSpec/DESCRIPTION > .DESCRIPTION
	sed "s/\(^Date: .*\)20[0-9][0-9]-[0-1][0-9]-[0-3][0-9]\(.*\)$$/\1`date +%F`\2/" .DESCRIPTION > hyperSpec/DESCRIPTION
	rm .DESCRIPTION

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

# package inst/doc ----------------------------------------------------------------------------------

pkg-doc:
	$(MAKE) -C hyperSpec/inst/doc 

# Vignette zips -------------------------------------------------------------------------------------

chondro.zip:
	$(error TODO)

fileio.zip:
	$(error TODO)
