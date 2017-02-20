# hyperSpec
this is going to be the new home for R package hyperSpec very soon.

right now, I'm working in the background ...

Issues and feature requests should already go here!


# Installation instructions

**Option A, simple:** open `R` and use command `install.packages("hyperSpec")`. This would, however, install
an old version from CRAN

**Option B – build it yourself**

## Preparations
1. [Install git-lfs](https://packagecloud.io/github/git-lfs/install) if you don't have it
1. You will need an **old version of *hyperSpec***, as well as package *R.matlab*. Open **R** and exec `install.packages(c("hyperSpec", "R.matlab")`. You can install other suggested packages as well (see below)
1. Clone [new hyperSpec from github](https://github.com/cbeleites/hyperSpec)
1. Essential big data are not here yet. Execute `git lfs pull`

## Building *hyperSpec*
1. Use git branch `issue-Makefile-31`
1. execute `make`
1. execute `make install`. This will install a bare version without documentation. We would need it to re-create vignettes.
1. execute `make build`. It would create a new tar.gz package with everything inside. Copy the the filename and install with `install.packages(<filename>, repos=NULL)`
1. Restart **R** in case of troubles

#### Suggested packages
Install suggested packages in **R** session with command
`install.packages(c(
    "hyperSpec",
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