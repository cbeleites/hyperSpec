# hyperSpec
This is the new home for R package hyperSpec.

Issues and feature requests should go [here](https://github.com/cbeleites/hyperSpec/issues)!

# Installation instructions

## Option A, simple: Installation from CRAN

open `R` and use command `install.packages("hyperSpec")`. This installs the latest version available on CRAN.

## Option B – build it yourself from source in this repo

### Preparations
1. You will need [git](https://git-scm.com/), [git-lfs](https://packagecloud.io/github/git-lfs/install) and [GNU make](https://www.gnu.org/software/make/)
1. Install packages *R.matlab* and *roxygen2*. Open **R** and exec `install.packages(c("hyperSpec", "R.matlab", "roxygen2")`. Consider installing the other suggested packages as well (see below).
1. This may sound funny, but you need a **recent version of `hyperSpec`** to build `hyperSpec` alongside with all documentation. It is recommended to fetch release [v0.98-20170223](https://github.com/cbeleites/hyperSpec/releases/tag/v0.98-20170223) from gitHub.  
   (This is the easiest and fastest way) 
1. Clone [hyperSpec source code from github](https://github.com/cbeleites/hyperSpec)
1. Essential big data are not here yet. Execute `git lfs pull` to get them.

### Building *hyperSpec*
1. If you build hyperSpec for the first time, or run into trouble as the installed version on your computer is too old, run `make bootstrap`
1. Execute `make install`. It creates a new tar.gz package with everything inside and installs it via `R CMD INSTALL hyperSpec_vv-date`.
1. Restart **R** to avoid trouble due to changes in the documentation data base.
1. `library (hyperSpec)`

#### Suggested packages
Install suggested packages in **R** session with command
`install.packages(c(
    "R.matlab",
    "roxygen2",
    "tripack",
    "deldir",
    "rgl",
    "plotrix",
    "sp",
    "baseline",
    "compiler",
    "inline",
    "Rcpp",
    "MASS",
    "fastcluster",
    "pls",
    "mvtnorm",
    "digest"))`
