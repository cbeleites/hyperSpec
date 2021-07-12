# hyperSpec 1.0.0 2021-xx-xx

## Acknowledgements

* The `hyperSpec` team gratefully acknowledges support from the Google Summer of Code program, which sponsored student Erick Oduniyi during summer 2020. Erick and the team carried out a significant overhaul of `hyperSpec` which led to this major release.


## User-Facing Changes from 0.99 Series

* `NEWS.md` (this file) added so that users can readily see changes that may affect the use of the package.
* Documentation now available in `pkgdown` sites (https://r-hyperspec.github.io/).
* Introductory vignette reorganized and enhanced, thanks to Bryan Hanson.
* Dataset `faux_cell` and function `generate_faux_cell()` replace `chondro` dataset (#125, #156, #180, #229).
* Portions of `hyperSpec` were spun out into their own packages for ease of maintenance. 
**--- ELABORATE ---**
    - Dataset `chondro` was moved to package **hySpc.chondro** (https://r-hyperspec.github.io/hySpc.chondro/).
    - Functions `qplotspc()`, `qplotmap()`, `qplotc()`, `qplotmixmap()`, `legendright()`,  `qmixtile()`, `normalize.colrange()`, `normalize.range()`, `normalize.null()`, `normalize.minmax()`, `qmixlegend()`, `colmix.rgb()` were deprecated due analogous functionality in package **hySpc.ggplot2** (https://r-hyperspec.github.io/hySpc.ggplot2/).
    - Functions `read.ENVI()`, `read.ENVI.HySpex()`, `read.ENVI.Nicolet()` were deprecated due to analogous functionality in package **hySpc.read.ENVI** (https://r-hyperspec.github.io/hySpc.read.ENVI/).
    - Functions `read.spc()`, `read.spc.Kaiser()`, `read.spc.KaiserMap()`, `read.spc.KaiserLowHigh()` were deprecated due to analogous functionality in package **hySpc.read.spc** (https://r-hyperspec.github.io/hySpc.read.spc/).
    - Functions `read.spe()`, `spe.showcalpoints()` were deprecated due to analogous functionality in package **hySpc.read.spe** (https://r-hyperspec.github.io/hySpc.read.spe/).
    - Functions `read.mat.Cytospec()`, `read.mat.Witec()` were deprecated due to analogous functionality in package **hySpc.read.mat** (https://r-hyperspec.github.io/hySpc.read.mat/).
    - Function `read.jdx()` was deprecated due to analogous functionality in package **hySpc.read.jdx** (https://r-hyperspec.github.io/hySpc.read.jdx/).
    - Functions `read.asc.Andor()`, `read.asc.PerkinElmer()`, `read.txt.Horiba()`, `read.txt.Horiba.xy()`, `read.txt.Horiba.t()`, `read.txt.long()`, `read.txt.Renishaw()`,  `read.zip.Renishaw()`, `read.txt.Shimadzu()`, `read.txt.wide()`, `read.txt.Witec()`, `read.txt.Witec.Graph()`, `read.dat.Witec()`, `wc()`, `count_lines()` were deprecated due analogous functionality in package **hySpc.read.txt** (https://r-hyperspec.github.io/hySpc.read.txt/).
    - The following functions were renamed or replaced by new ones (see table below).
    
     Deprecated function     | New (replacement) function       | Related issues
    -------------------------|-------------------------------   | ----------------
     `.fileio.optional()`    | `.spc_io_postprocess_optional()` |  #208, #302
     `.fix_spc_colnames()`   | `.spc_fix_colnames()`            |  #208, #301
     `alois.palette()`       | `palette_alois()`                |  #208, #299, @sangttruong 
     `guess.wavelength()`    | `extract_numbers()`              |  #208, #309
     `matlab.dark.palette()` | `palette_matlab_dark()`          |  #299, #299, @sangttruong
     `matlab.palette()`      | `palette_matlab()`               |  #208, #299, @sangttruong
     `mergeextra()`          | `merge_data()`                   |  #208, #302
     `orderwl()`             | `wl_sort()`                      |  #208, #309
     `spc.bin()`             | `spc_bin()`                      |  #208, #301
     `spc.fit.poly()`        | `spc_fit_poly()`                 |  #208, #301
     `spc.fit.poly.below()`  | `spc_fit_poly_below()`           |  #208, #301
     `spc.loess()`           | `spc_loess()`                    |  #208, #301
     `spc.NA.approx()`       | `spc_na_approx()`                |  #208, #301
     `spc.rubberband()`      | `spc_rubberband()`               |  #208, #301
     `spc.smooth.spline()`   | `spc_smooth_spline()`            |  #208, #301
     `wl.eval()`             | `wl_eval()`                      |  #208, #309
     `wlconv()`              | `wl_convert_units()`             |  #208, #309
    - Wavelength unit conversion functions `ev2freq()`, `ev2invcm()`, `ev2nm()`, `ev2raman()`, `freq2ev()`, `freq2invcm()`, `freq2nm()`, `freq2raman()`, `invcm2ev()`, `invcm2freq()`, `invcm2nm()`, `invcm2raman()`, `nm2ev()`, `nm2freq()`, `nm2invcm()`, `nm2raman()`, `raman2ev()`, `raman2freq()`, `raman2invcm()`, `raman2nm()` are deprecated in favor of `wl_convert_units()` (#300).
* Function `spc.NA.linapprox()`, which was deprecated for long time, is now completely removed (#239).
* Column names in spectra matrix (`$spc` column of `hyperSpec` object) are now returned correctly by functions `spc.bin()` (#237), and `spc.loess()` (#245).
* New function `hy_list_available_hySpc_packages()` lists packages, that are available in GitHub organization `r-hyperSpec`.
* New function `hy_browse_homepage()` opens the homepage of *R hyperSpec* in a web browser.
* New function `hy_list_installed_hySpc_packages()` lists and function `hy_attach()` conveniently loads and attaches all installed **`r-hyperspec`** family packages (@cbeleites, @GegznaV, #219).
* Changes related to function `as.hyperSpec()`:
    - New method `as.hyperSpec(<hyperSpec>)` was created (#282).
    - The default value of argument `wl` is now set to `wl = NULL` (#297).
    - `wl = NULL` now means that the default values of wavelengths should be calculated inside the methods of `as.hyperSpec()` (#297).
* Possibility to initialize `hyperSpec` object by providing wavelengths only (#288).
* Function `wl.eval()` is converted into S3 generic. Methods `wl.eval(<hyperSpec>)` and `wl.eval(<numeric>)` for numeric vectors were added (#287).
* New function `new_hyperSpec()` that initializes `hyperSpec` object in a similar way as `new("hyperSpec")` does but has autocompletion possibilities in RStudio (#283).
* Functions `show()` and `print()` give more concise default output now (@GegznaV, #211).
* The default output of function `summary()` was changed (@GegznaV, #211).
* New color palette `palette_colorblind` introduced (@bryanhanson).



## Non-User-Facing Changes from 0.99 Series

* Note: this listing is for the benefit of developers, and should summarize significant infrastructure changes.
* Vignettes converted to `.Rmd` and formatted consistently, thanks to @GegznaV.
* Package **hySpc.testthat** is now used for unit testing (#228).
