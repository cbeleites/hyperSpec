all: roxygenize pkg-data pkg-doc vignettes pkg-vignettes | fileio-tests

DATE = $(shell date +%y%m%d)

clean:
	@rm -f *~ .*~ \#*\#
	@rm -f hyperSpec_*.tar.gz
	@rm -rf hyperSpec.Rcheck
	$(MAKE) -C Vignettes/baseline     clean
	$(MAKE) -C Vignettes/chondro      clean
	$(MAKE) -C Vignettes/fileio       clean
	$(MAKE) -C Vignettes/flu          clean
	$(MAKE) -C Vignettes/hyperspec clean
	$(MAKE) -C Vignettes/laser        clean
	$(MAKE) -C Vignettes/plotting     clean
	$(MAKE) -C hyperSpec/inst/doc     clean
	$(MAKE) -C hyperSpec/vignettes -f Makefile-local clean

superclean:
	@git clean -q -f -x -d

# TODO: add dependency `clean`

## bootstrap target does the required processing immediately after cloning, superclean, or
## if the installed version of hyperSpec is too old for building the vignettes

bootstrap: installdeps bootstrapI chondro flu laser pkg-data | fileio-tests
	@R CMD build --no-build-vignettes hyperSpec/
	@R CMD INSTALL hyperSpec_*-$(DATE).tar.gz

bootstrapI: roxygenize
	@R CMD build --no-build-vignettes hyperSpec/
	@R CMD INSTALL hyperSpec_*-$(DATE).tar.gz

installdeps:
	@echo -n "checking required & suggested packages ... "
	@Rscript --vanilla -e 'pkgs <- packageDescription("hyperSpec", lib.loc = "./", fields = c ("Depends", "Suggests", "Imports"))'\
                           -e 'pkgs <- gsub (pattern = "\n", replacement = "", pkgs)'\
	                   -e 'pkgs <- gsub (pattern = "[ ]?[(][^)]*[)]", "", pkgs) '\
	                   -e 'pkgs <- strsplit(pkgs, ",")'\
	                   -e 'pkgs <- unlist (pkgs)'\
	                   -e 'pkgs <- setdiff (pkgs, c (installed.packages()[, 1], "R"))'\
	                   -e 'if (length (pkgs) == 0L){'\
	                   -e 'cat ("OK\n")'\
	                   -e '} else {'\
	                   -e 'cat ("\n   installing: ", pkgs, "\n")'\
	                   -e 'install.packages(pkgs, repos = "https://cran.rstudio.com/")  '\
	                   -e '}'

fileio-tests: 
	$(MAKE) -C  hyperSpec/tests/testthat fileio

## installation targets

install: build
	@R CMD INSTALL hyperSpec_*-$(DATE).tar.gz

installdev: roxygenize pkg-vignettes
	@R CMD INSTALL hyperSpec --with.keep-source --fake --no-docs --no-build-vignettes

# Code building
build: all
	R CMD build hyperSpec

roxygenize: DESCRIPTION hyperSpec/R/*.R
	@echo "Roxygenize"
	@Rscript --vanilla -e "library (methods, quietly=TRUE, verbose = FALSE); library (devtools, quietly=TRUE, verbose = FALSE); document ('hyperSpec')"

DESCRIPTION: $(shell find hyperSpec -maxdepth 1 -daystart -not -ctime 0 -name "DESCRIPTION") #only if not modified today
	@echo update DESCRIPTION
	@sed "s/\(^Version: .*-\)[0-9][0-9][0-1][0-9][0-3][0-9]\(.*\)$$/\1$(DATE)\2/" hyperSpec/DESCRIPTION > .DESCRIPTION
	@sed "s/\(^Date: .*\)20[0-9][0-9]-[0-1][0-9]-[0-3][0-9]\(.*\)$$/\1`date +%F`\2/" .DESCRIPTION > hyperSpec/DESCRIPTION
	@rm .DESCRIPTION

# VIGNETTES ########################################################################################

vignettes: chondro flu laser plotting hyperspec fileio laser plotting baseline

# in subdirs ---------------------------------------------------------------------------------------

# baseline .........................................................................................

baseline:
	$(MAKE) -C Vignettes/baseline
	$(MAKE) -C hyperSpec/vignettes -f Makefile-local baseline.Rnw

# chondro ..........................................................................................

chondro:
	$(MAKE) -C Vignettes/chondro
	$(MAKE) -C hyperSpec/vignettes  -f Makefile-local chondro.pdf
	$(MAKE) -C hyperSpec/vignettes  -f Makefile-local chondro.pdf.asis

#	cd $(dir $<) &&	R CMD Sweave chondro.Rnw --clean --pdf --compact="both" --quiet

# fileio ...........................................................................................

fileio:
	$(MAKE) -C Vignettes/fileio
	$(MAKE) -C hyperSpec/vignettes  -f Makefile-local fileio.pdf
	$(MAKE) -C hyperSpec/vignettes  -f Makefile-local fileio.pdf.asis

# flu ..............................................................................................

flu:
	$(MAKE) -C Vignettes/flu
	$(MAKE) -C hyperSpec/vignettes -f Makefile-local flu.Rnw

# hyperspec vignette (aka introduction) ............................................................

hyperspec:
	$(MAKE) -C Vignettes/hyperspec
	$(MAKE) -C hyperSpec/vignettes -f Makefile-local hyperspec.Rnw

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
