# hyperSpec

[![CRAN status](https://www.r-pkg.org/badges/version/hyperSpec)](https://cran.r-project.org/package=hyperSpec)

https://github.com/r-hyperspec/hyperSpec is the new home for R package hyperSpec.

Issues and feature requests should go [here](https://github.com/r-hyperspec/hyperSpec/issues)!

# Installation instructions

## Option A, simple: Installation from CRAN

open `R` and use command `install.packages("hyperSpec")`. This installs the latest version available on CRAN.
[![CRAN status](https://www.r-pkg.org/badges/version/hyperSpec)](https://cran.r-project.org/package=hyperSpec)

## Option B – build it yourself from source in this repo

### Preparations
1. You will need [git](https://git-scm.com/), [git-lfs](https://packagecloud.io/github/git-lfs/install) and [GNU make (>= **3.80**)](https://www.gnu.org/software/make/)

   **NOTE:** Windows users need to download and install Rtools(see this [instructions](https://cran.r-project.org/bin/windows/Rtools/)). ***`GNU make` is included to Rtools, so you don't need to install it.*** See Troubleshooting below to check version of `GNU make` and update it if needed. 
1. Clone or download [hyperSpec source code from github](https://github.com/cbeleites/hyperSpec)  
   `git clone git@github.com:cbeleites/hyperSpec.git`

### Building *hyperSpec*
1. If you build hyperSpec for the first time, or run into trouble as the installed version on your computer is too old, run `make bootstrap`
  This will also check whether all dependencies and suggested packages (which are in fact dependencies for building) are installed and install them if not.
1. Execute `make install`. It creates a new tar.gz package with everything inside and installs it via `R CMD INSTALL hyperSpec_vv-date`.
1. Restart **R** to avoid trouble due to changes in the documentation data base.

**NOTE:** Windows users can run `make` commands in **R** using `system`: for example, `system('make install')`. But be sure that your wd is set to the root directory containing `Makefile`.

#### Troubleshooting

* Vignette building fails and you suspect that this is due to the *installed* version of hyperSpec being too old.

   ***Solution:*** Run `make bootstrap` to fix it.
* Vignette *fileio* needs lots of example files for testing import functions. These are managed by `git lfs`.

   ***Typical error:*** e.g. when building vignette `chondro`, import function complains `Error in scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings,  :   scan() expected 'a real', found 'version'`  
   or in vignette `flu`: `Error in .spc.filehdr(f) :
  Wrong spc file format version (or no spc file at all).
Only 'new' spc files (1996 file format) with LSB word order are supported.`


   ***Solution:*** Execute `git lfs pull` to get them.
* Old versions of `GNU make` don't support order-only prerequisites (|).

   ***Typical errors:*** `No rule to make target '|', needed by 'fileio.pdf'. Stop.`

   ***Solution:*** Update version of `GNU make`. *For Linux*, just download last version from the link above. *For Windows*, run `system('where make')` and replace that file by make.exe from [here] (http://www.equation.com/servlet/equation.cmd?fa=make). To check version of `GNU make` use `make -v`.
* Default language of your R environment is not English.

   ***Typical error:*** `inputenc Error: Unicode char \u8:árq not set up for use with LaTeX`

   ***Solution:*** Set language of the session to English as suggested [here](http://stackoverflow.com/questions/13575180/how-to-change-language-settings-in-r).

* Issues of versions 5.0.1 - 6.0.0(?) of roxygen2.

   ***Typical error:*** `unable to find required package 'roxygen_devtest'`

   ***Solution:*** In order to avoid it update roxygen2 (version 6.0.1 works) or downgrade to version 5.0.1: run in **R** `devtools::install_version(package = 'roxygen2', version = '5.0.1')`.

