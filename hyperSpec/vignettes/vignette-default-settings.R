# Plotting settings ==========================================================
# Base plot parameters -------------------------------------------------------
knitr::knit_hooks$set(small.mar = function(before, options, envir) {
  if (before) {
    par(mar = c(4.1, 4.1, 1, .6))
  }
})

knitr::opts_chunk$set(small.mar = TRUE)

# Lattice parameters ---------------------------------------------------------
set_trellis_layout_hw_default <- function() {

  trellis_layout_heights <- list(
    top.padding = 1,
    main = 1,
    main.key.padding = 1,
    key.top = 1,
    xlab.top = 1,
    key.axis.padding = 1,
    axis.top = 1,
    strip = 1,
    panel = 1,
    axis.panel = 1,
    between = 1,
    axis.bottom = 1,
    axis.xlab.padding = 1,
    xlab = 1,
    xlab.key.padding = 0,
    key.bottom = 1,
    key.sub.padding = 1,
    sub = 1,
    bottom.padding = 1
  )

  trellis_layout_widths <- list(
    left.padding = 1,
    key.left = 1,
    key.ylab.padding = 0,
    ylab = 1,
    ylab.axis.padding = 1,
    axis.left = 1,
    axis.panel = 1,
    strip.left = 1,
    panel = 1,
    between = 1,
    axis.right = 1,
    axis.key.padding = 1,
    ylab.right = 1,
    key.right = 1,
    right.padding = 1
  )

  trellis.par.set(layout.heights = trellis_layout_heights)
  trellis.par.set(layout.widths = trellis_layout_widths)
}

set_trellis_layout_hw_custom <- function() {
  trellis_layout_heights <- trellis.par.get("layout.heights")
  trellis_layout_heights[grep("padding", names(trellis_layout_heights))] <- 0
  trellis.par.set(layout.heights = trellis_layout_heights)

  trellis_layout_widths <- trellis.par.get("layout.widths")
  trellis_layout_widths[grep("padding", names(trellis_layout_widths))] <- 0
  trellis.par.set(layout.widths = trellis_layout_widths)
}

set_trellis_layout_hw_custom()

# knitr Settings =============================================================
knitr::opts_chunk$set(
  echo       = TRUE,     # Should blocks with program code be shown in knitted documents?
  eval       = TRUE,     # Should program code be evaluated?
  fig.height = 2.6,      # Default height for plots.
  fig.width  = 4,        # Default width for plots.
  fig.align  = "center", # Default alignment for plots in knitted documents.
  collapse   = TRUE,
  comment    = "#>"
)

# R Settings =================================================================
options("width" = 100, "digits" = 5)
options(rmarkdown.html_vignette.check_title = FALSE) # Vig index entry not document title
set.seed(2020)

# Temporaty options ==========================================================
# This option controls if reviewers/translator's notes are visible
# in the vignettes
options(show_reviewers_notes = FALSE)
