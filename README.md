# hyperSpec
This is the new home for R package hyperSpec.

Issues and feature requests should go [here](https://github.com/cbeleites/hyperSpec/issues)!

# Installation instructions

## Option A, simple: Installation from CRAN

open `R` and use command `install.packages("hyperSpec")`. This installs the latest version available on CRAN.

## Option B – build it yourself from source in this repo

### Preparations
1. You will need [git](https://git-scm.com/), [git-lfs](https://packagecloud.io/github/git-lfs/install) and [GNU make (>= **3.80**)](https://www.gnu.org/software/make/)

   **NOTE:** Windows users need to dowload and install Rtools(see this [instructions](https://cran.r-project.org/bin/windows/Rtools/)). ***`GNU make` is included to Rtools, so you don't need to install it.*** See Troubleshooting below to check version of `GNU make` and update it if needed. 
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
   
   ***Solution:*** Execute `git lfs pull` to get them.
* Old versions of `GNU make` don't support order-only prerequisites (|).
   
   ***Typical erros:*** `No rule to make target '|', needed by 'fileio.pdf'. Stop.`

   ***Solution:*** Update version of `GNU make`. *For Linux*, just downloand last version from the link above. *For Windows*, run `system('where make')` and replace that file by make.exe from [here] (http://www.equation.com/servlet/equation.cmd?fa=make). To check version of `GNU make` use `make -v`.
* Issues of new versions(> 5.0.1) of roxygen2.

   ***Typical error:*** `unable to find required package 'roxygen_devtest'`
   
   ***Solution:*** In order to avoid it use roxygen2 of version 5.0.1: run in **R** `devtools::install_version(package = 'roxygen2', version = '5.0.1')`.
* Default language of your R enviroment is not English.

   ***Typical error:*** `inputenc Error: Unicode char \u8:árq not set up for use with LaTeX`
   
   ***Solution:*** Set language of the session to English as suggested [here](http://stackoverflow.com/questions/13575180/how-to-change-language-settings-in-r).

* After update to `R 3.4.0` building fails, typically on the `flu` vignette.

   ***Typical error:*** Command `make` successfully buils a vignette and then simply stops with something like `make[1]: *** [flu.pdf] Error 1`
   
   ***Solution:*** This is a bug in new R. Rollback to R 3.3.x, this is the only option right now. See Issue #45 for more details
