# hyperSpec 1.0.0 2020-xx-xx

## Acknowledgements

* The `hyperSpec` team gratefully acknowledges support from the Google Summer of Code program, which sponsored student Erick Oduniyi during summer 2020. Erick and the team carried out a significant overhaul of `hyperSpec` which led to this major release.


## User-Facing Changes from 0.99 Series

* `NEWS.md` (this file) added so that users can readily see changes that may affect the use of the package.
* Documentation now available in `pkgdown` sites (https://r-hyperspec.github.io/).
* Introductory vignette reorganized and enhanced, thanks to Bryan Hanson.
* Portions of `hyperSpec` were spun out into their own packages for ease of maintenance. 
**--- ELABORATE ---**
    - Dataset `chondro` was moved to package **hySpc.chondro** (https://r-hyperspec.github.io/hySpc.chondro/).
    - Functions `qplotspc()`, `qplotmap()`, `qplotc()`, `qplotmixmap()`, `legendright()`,  `qmixtile()`, `normalize.colrange()`, `normalize.range()`, `normalize.null()`, `normalize.minmax()`, `qmixlegend()`, `colmix.rgb()` were deprecated due analogous functionality in package **hySpc.ggplot2** (https://r-hyperspec.github.io/hySpc.ggplot2/).
    - Functions `read.asc.Andor()`, `read.asc.PerkinElmer()`, `read.txt.Horiba()`, `read.txt.Horiba.xy()`, `read.txt.Horiba.t()`, `read.txt.long()`, `read.txt.Renishaw()`,  `read.zip.Renishaw()`, `read.txt.Shimadzu()`, `read.txt.wide()`, `read.txt.Witec()`, `read.txt.Witec.Graph()`, `read.dat.Witec()`, `wc()`, `count_lines()` were deprecated due analogous functionality in package **hySpc.read.txt** (https://r-hyperspec.github.io/hySpc.read.txt/).
    - Functions `read.mat.Witec()`, **--- ELABORATE ---**  were deprecated due analogous functionality in package **hySpc.read.mat** (https://r-hyperspec.github.io/hySpc.read.mat/).
* Dataset `faux_cell` and function `generate_faux_cell` replace `chondro` dataset (#125, #156, #180, #229).
* Function `spc.NA.linapprox()`, which was deprecated for long time, is now completely removed (#239).
* Column names in spectra matrix (`$spc` column of `hyperSpec` object) are now returned correctly by functions `spc.bin()` (#237), and `spc.loess()` (#245).
* New function `hy_list_available_hySpc_packages()` lists packages, that are available in GitHub organization `r-hyperSpec`.
* New function `hy_browse_homepage()` opens the homepage of *R hyperSpec* in a web browser.
* New method `as.hyperSpec(<hyperSpec>)` (#282).


## Non-User-Facing Changes from 0.99 Series

* Note: this listing is for the benefit of developers, and should summarize significant infrastructure changes.
* Vignettes converted to `.Rmd` and formatted consistently, thanks to Vilmantas Gegzna.
* Package **hySpc.testthat** is now used for unit testing (#228).
