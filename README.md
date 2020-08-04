
<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version-last-release/hyperSpec)](https://cran.r-project.org/package=hyperSpec)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/hyperSpec)](https://cran.r-project.org/package=hyperSpec)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/hyperSpec)](https://cran.r-project.org/package=hyperSpec)
[![R-CMD-check](https://github.com/cbeleites/hyperSpec/workflows/R-CMD-check/badge.svg?branch=develop)](https://github.com/cbeleites/hyperSpec/actions)
[![Codecov](https://codecov.io/gh/cbeleites/hyperSpec/branch/develop/graph/badge.svg)](https://codecov.io/gh/cbeleites/hyperSpec?branch=develop)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- badges: end -->

<!-- ---------------------------------------------------------------------- -->


# R package **hyperSpec**

[**R**](https://www.r-project.org/) package **hyperSpec** is the main package in the [**`r-hyperspec`**](https://r-hyperspec.github.io/) family of packages.
The goal of **hyperSpec** (and whole **`r-hyperspec`**) is to make the work with hyperspectral data sets, (i.e. spatially or time-resolved spectra, or spectra with any other kind of information associated with each of the spectra) more comfortable.
The spectra can be data as obtained during 
[XRF](https://en.wikipedia.org/wiki/X-ray_fluorescence),
[UV/VIS](https://en.wikipedia.org/wiki/Ultraviolet%E2%80%93visible_spectroscopy), 
[Fluorescence](https://en.wikipedia.org/wiki/Fluorescence_spectroscopy),
[AES](https://en.wikipedia.org/wiki/Auger_electron_spectroscopy),
[NIR](https://en.wikipedia.org/wiki/Near-infrared_spectroscopy),
[IR](https://en.wikipedia.org/wiki/Infrared_spectroscopy), 
[Raman](https://en.wikipedia.org/wiki/Raman_spectroscopy), 
[NMR](https://en.wikipedia.org/wiki/Nuclear_magnetic_resonance_spectroscopy), 
[MS](https://en.wikipedia.org/wiki/Mass_spectrometry),
etc. spectroscopy measurements.
More generally, any data that is recorded over a discretized variable, e.g. `absorbance = f(wavelength)`, stored as a vector of absorbance values for discrete wavelengths is suitable.


<!-- ---------------------------------------------------------------------- -->

## Documentation

There are two versions of **hyperSpec** online documentation:

a. for the [released version](https://cbeleites.github.io/hyperSpec/) and
b. for the [development version](https://cbeleites.github.io/hyperSpec/dev/) of **hyperSpec**.

The documentation of other **`r-hyperspec`** family packages can be found [here](https://r-hyperspec.github.io/).

<!-- ---------------------------------------------------------------------- -->

## Issues, bug repotrs and feature requests

Issues, bug repotrs and feature requests should go [here](https://github.com/cbeleites/hyperSpec/issues)!
<!-- ---------------------------------------------------------------------- -->

## Installation

### Install from CRAN

You can install the released version of **hyperSpec** from [CRAN](https://cran.r-project.org/package=hyperSpec) with:

```r
install.packages("hyperSpec")
```

### Install from GitHub

You can install the development version of the package from [GitHub](https://github.com/cbeleites/hyperSpec):

```r 
if (!require(remotes)) {install.packages("remotes")}
remotes::install_github("cbeleites/hyperSpec/hyperSpec")
```

> **NOTE:** 
> This method will not install package's documentation (help pages and vignettes) into your computer.
> So you can either use the [online documentation](https://cbeleites.github.io/hyperSpec/dev/) or build the package from source (see the next section).


## How to build from source

**NOTE:** Windows users need to download, install and properly configure **Rtools** (see [these instructions](https://cran.r-project.org/bin/windows/Rtools/)). These are required by a number of dependencies of hyperSpec. RStudio will offer to automatically install them if necessary.

We assume that your current working directory is in the root of repository, i.e. the directory with `README.md` file.

In order to build the package from source, first install packages listed under the section `Suggests:` in file `hyperSpec/DESCRIPTION`. At the very minimum at least the following packages are necessary:

```
install.packages(c("roxygen2", "devtools", "knitr", "rmarkdown", "bookdown", "R.matlab", "kableExtra", "mvtnorm", "plotrix", "pls", "baseline", "deldir", "tripack"))
```

You can use package `remotes` to automatically install all dependencies from the `Suggests` list:

```
# Do not abort installation even if some packages are not available
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")

remotes::install_deps('hyperSpec', dependencies = 'Suggests')
```

If using RStudio, go to menu *Build/configure build tools*, use 'Package' build system, set `hyperSpec` as the package directory, and make sure that the checkboxes for 'Use devtools package' and 'Generate documentation with Roxygen' are checked. Under the *Roxygen configuration* dialog set all checkboxes except of 'install and restart'.

Once everything is configured, you would need to invoke 'document' and then 'build' in RStudio, or the following in the terminal:

```
devtools::document('hyperSpec')
devtools::build('hyperSpec')
```
