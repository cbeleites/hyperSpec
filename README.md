
# hyperSpec

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/hyperSpec)](https://cran.r-project.org/package=hyperSpec)
[![R-CMD-check](https://github.com/GegznaV/hyperSpec/workflows/R-CMD-check/badge.svg?branch=change-package-structure)](https://github.com/GegznaV/hyperSpec/actions)
<!-- badges: end -->

The goal of **hyperSpec** package is to work confortably with hyperspectral data sets, i.e. spatially or time-resolved spectra, or spectra with any other kind of information associated with each of the spectra.
The spectra can be data as obtained in XRF, UV/VIS, Fluorescence, AES, NIR, IR, Raman, NMR, MS, etc.
More generally, any data that is recorded over a discretized variable, e.g. absorbance = f (wavelength), stored as a vector of absorbance values for discrete wavelengths is suitable.

Issues and feature requests should go [here](https://github.com/cbeleites/hyperSpec/issues)!

## Installation

You can install the released version of **hyperSpec** from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("hyperSpec")
```

This branch of the package could be installed with:

``` r 
if (!require(remotes)) {install.packages("remotes")}
remotes::install_github("GegznaV/hyperSpec", ref = "change-package-structure")
```

