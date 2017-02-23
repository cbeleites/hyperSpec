# hyperSpec
This is a new home for R package hyperSpec.

right now, I'm working in the background ...

Issues and feature requests should already go here!


# Installation instructions

**Option A, simple:** open `R` and use command `install.packages("hyperSpec")`. This would, however, install
an old version from CRAN

**Option B – build it yourself**

## Preparations
1. You will need [git](https://git-scm.com/), [git-lfs](https://packagecloud.io/github/git-lfs/install) and [GNU make](https://www.gnu.org/software/make/)
1. Install package *R.matlab*. Open **R** and exec `install.packages(c("hyperSpec", "R.matlab")`. You can install other suggested packages as well (see below).
1. This could sound funny, but would need a **recent version of `hyperSpec`** to build `hyperSpec` alongside with all documentation. It is recommended to fetch release [v0.98-20170223](https://github.com/cbeleites/hyperSpec/releases/tag/v0.98-20170223) from gitHub.
1. Clone [hyperSpec source code from github](https://github.com/cbeleites/hyperSpec)
1. Essential big data are not here yet. Execute `git lfs pull` to get them.

## Building *hyperSpec*
1. Execute `make build`. It would create a new tar.gz package with everything inside. Copy the the filename and install with `install.packages(<filename>, repos=NULL)`
1. Restart **R** in case of troubles

#### Suggested packages
Install suggested packages in **R** session with command
`install.packages(c(
    "R.matlab",
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
