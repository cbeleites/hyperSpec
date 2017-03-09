# hyperSpec
This is the new home for R package hyperSpec.

Issues and feature requests should go [here](https://github.com/cbeleites/hyperSpec/issues)!

# Installation instructions

## Option A, simple: Installation from CRAN

open `R` and use command `install.packages("hyperSpec")`. This installs the latest version available on CRAN.

## Option B – build it yourself from source in this repo

### Preparations
1. You will need [git](https://git-scm.com/), [git-lfs](https://packagecloud.io/github/git-lfs/install) and [GNU make (>= **3.80**)](https://www.gnu.org/software/make/)
1. Clone [hyperSpec source code from github](https://github.com/cbeleites/hyperSpec)  
   `git clone git@github.com:cbeleites/hyperSpec.git`

### Building *hyperSpec*
1. If you build hyperSpec for the first time, or run into trouble as the installed version on your computer is too old, run `make bootstrap`
  This will also check whether all dependencies and suggested packages (which are in fact dependencies for building) are installed and install them if not. 
1. Execute `make install`. It creates a new tar.gz package with everything inside and installs it via `R CMD INSTALL hyperSpec_vv-date`.
1. Restart **R** to avoid trouble due to changes in the documentation data base.

#### Troubleshooting

* Vignette building fails and you suspect that this is due to the *installed* version of hyperSpec being too old:  
  `make bootstrap` should take care of this situation.

* Vignette *fileio* needs lots of example files for testing import functions. These are managed by `git lfs`. If they are missing, execute `git lfs pull` to get them.

* `Rtools` on *Windows* ships `make` version 3.79 which does not provide order-prerequisites. You need to update make in order to build hyperSpec. 
