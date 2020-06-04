# Plotting settings ==========================================================
# Base plot parameters -------------------------------------------------------
knitr::knit_hooks$set(small.mar = function(before, options, envir) {
  if (before) {
    par(mar = c(4.1, 4.1, 1, .6))
  }
})

knitr::opts_chunk$set(small.mar = TRUE)

# Lattice parameters ---------------------------------------------------------
trellis.pars <- trellis.par.get("layout.heights")
trellis.pars[grep("padding", names(trellis.pars))] <- 0
trellis.par.set(layout.heights = trellis.pars)

trellis.pars <- trellis.par.get("layout.widths")
trellis.pars[grep("padding", names(trellis.pars))] <- 0
trellis.par.set(layout.widths = trellis.pars)

# Settings ===================================================================
knitr::opts_chunk$set(
  echo       = TRUE,     # Should blocks with program code be shown in knitted documents?
  eval       = TRUE,     # Should program code be evaluated?
  fig.height = 2.6,      # Default height for plots.
  fig.width  = 4,        # Default width for plots.
  fig.align  = "center", # Default alignment for plots in knitted documents.
  collapse   = TRUE,
  comment    = "#>"
)

options("width" = 100, "digits" = 5)

set.seed(2020)
