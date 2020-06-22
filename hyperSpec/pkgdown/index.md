# R package **hyperSpec**

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version-last-release/hyperSpec)](https://cran.r-project.org/package=hyperSpec)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/grand-total/hyperSpec)](https://cran.r-project.org/package=hyperSpec)
[![metacran downloads](https://cranlogs.r-pkg.org/badges/hyperSpec)](https://cran.r-project.org/package=hyperSpec)
[![R build status](https://github.com/cbeleites/hyperSpec/workflows/R-CMD-check/badge.svg)](https://github.com/cbeleites/hyperSpec/actions)
[![Codecov test coverage](https://codecov.io/gh/cbeleites/hyperSpec/branch/develop/graph/badge.svg)](https://codecov.io/gh/cbeleites/hyperSpec?branch=develop)
[![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
<!-- badges: end -->

<!-- ---------------------------------------------------------------------- -->

Package **hyperSpec** is an [**R**](https://www.r-project.org/) package.
The goal of **hyperSpec** is to makethe work with hyperspectral data sets, (i.e. spatially or time-resolved spectra, or spectra with any other kind of information associated with each of the spectra) more confortable.
The spectra can be data as obtained during XRF, UV/VIS, Fluorescence, AES, NIR, IR, Raman, NMR, MS, etc. spectroscopy measurements.
More generally, any data that is recorded over a discretized variable, e.g. `absorbance = f(wavelength)`, stored as a vector of absorbance values for discrete wavelengths is suitable.

Issues and feature requests should go [here](https://github.com/cbeleites/hyperSpec/issues)!

<!-- ---------------------------------------------------------------------- -->

## Documentation

There are two versions of online documentation:

a) for the [released version](https://cbeleites.github.io/hyperSpec/) and
b) for the [development version](https://cbeleites.github.io/hyperSpec/dev/) of **hyperSpec**.

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
remotes::install_github("cbeleites/hyperSpec/hyperSpec", ref = "develop")
```

**NOTE:** Windows users need to download, install and properly configure **Rtools** (see [these instructions](https://cran.r-project.org/bin/windows/Rtools/)). 

