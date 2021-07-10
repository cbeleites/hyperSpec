### --------------------------------------------------------------------------------------------------
###
###  plotspc - Plots spectra of hyperSpec object
###
###  convenient plot interface for plotting spectra
###



#' Plotting spectra
#'
#' Plot the spectra of a `hyperSpec` object, i.e. intensity over
#' wavelength. Instead of the intensity values of the spectra matrix summary
#' values calculated from these may be used.
#'
#' This is `hyperSpec`'s main plotting function for spectra plots.
#'
#' New plots are created by [graphics::plot()], but the abscissa and
#' ordinate are drawn separately by [graphics::axis()]. Also,
#' [graphics::title()] is called explicitly to set up titles and
#' axis labels. This allows fine-grained customization of the plots.
#'
#' If package plotrix is available, its function
#' [plotrix::axis.break()] is used to produce break marks for cut
#' wavelength axes.
#'
#' @param object the `hyperSpec` object
#' @param wl.range the wavelength range to be plotted.
#'
#' Either a numeric vector or a list of vectors with different wavelength
#'   ranges to be plotted separately.
#'
#' The values can be either wavelengths or wavelength indices (according to
#'   `wl.index`).
#' @param wl.index if `TRUE`, `wl.range` is considered to give
#'   column indices into the spectra matrix. Defaults to specifying wavelength
#'   values rather than indices.
#' @param wl.reverse if `TRUE`, the wavelength axis is plotted backwards.
#' @param spc.nmax maximal number of spectra to be plotted (to avoid
#'   accidentally plotting of large numbers of spectra).
#' @param func a function to apply to each wavelength in order to calculate
#'   summary spectra such as mean, min, max, etc.
#' @param func.args `list` with furter arguments for `func`
#' @param add if `TRUE`, the output is added to the existing plot
#' @param bty see [graphics::par()]
#' @param col see [graphics::par()]. `col` might be a vector
#'   giving individual colors for the spectra.
#' @param xoffset vector with abscissa offsets for each of the
#'   `wl.range`s. If it has one element less than there are
#'   `wl.range`s, 0 is padded at the beginning.
#'
#' The values are interpreted as the distance along the wavelength axis that
#'   the following parts of the spectra are shifted towards the origin. E.g.
#'   if `wl.range = list (600 ~ 1800, 2800 ~ 3200)`, `xoffset = 750`
#'   would result in a reasonable plot. See also the examples.
#' @param yoffset ordinate offset values for the spectra. May be offsets to
#'   stack the spectra ([stacked.offsets()]). Either one for all
#'   spectra, one per spectrum or one per group in `stacked`.
#' @param nxticks hint how many tick marks the abscissa should have.
#' @param stacked if not `NULL`, a "stacked" plot is produced, see the
#'   example. `stacked` may be `TRUE` to stack single spectra.  A
#'   numeric or factor is interpreted as giving the grouping, character is
#'   interpreted as the name of the extra data column that holds the groups.
#' @param stacked.args a list with further arguments to
#'   [stacked.offsets()].
#' @param fill if not `NULL`, the area between the specified spectra is
#'   filled with color `col`. The grouping can be given as factor or
#'   numeric, or as a character with the name of the extra data column to use.
#'   If a group contains more than 2 spectra, the first and the last are used.
#'
#' If `TRUE` spectra n and nrow (spc) - n build a group.
#' @param fill.col character vector with fill color. Defaults to brightened
#'   colors from `col`.
#' @param border character vector with border color. You will need to set the
#'   line color `col` to `NA` in order see the effect.
#' @param plot.args `list` with further arguments to
#'   [graphics::plot()]
#' @param axis.args `list` with further arguments for
#'   [graphics::axis()]. `axis.args$x` should contain arguments
#'   for plotting the abscissa, `axis.args$y` those for the ordinate
#'   (again as `lists`).
#' @param title.args list with further arguments to
#'   [graphics::title()].
#'
#' `title.args` may contain two lists, `$x`, and `$y` to set
#'   parameters individually for each axis.
#' @param lines.args list with further arguments to
#'   [graphics::lines()].
#'
#' `lines.args$type` defaults to "l".
#' @param break.args list with arguments for
#'   [plotrix::axis.break()].
#' @param polygon.args list with further arguments to
#'   [graphics::polygon()] which draws the filled areas.
#' @param zeroline `NA` or a list with arguments
#'   [graphics::abline()], used to plot line (s) marking I = 0.
#'
#' `NA` suppresses plotting of the line.  The line is by default turned
#'   off if `yoffset` is not 0.
#' @param debuglevel if > 0, additional debug output is produced,
#' see [hyperSpec::options()] for details
#' @return `plotspc` invisibly returns a list with
#'
#' \item{x}{the abscissa coordinates of the plotted spectral data points}
#'
#' \item{y}{a matrix the ordinate coordinates of the plotted spectral data
#'   points}
#'
#' \item{wavelengths}{the wavelengths of the plotted spectral data points}
#'
#' This can be used together with [spc.identify()].
#' @author C. Beleites
#' @seealso [graphics::plot()], [graphics::axis()],
#'   [graphics::title()], [graphics::lines()],
#'   [graphics::polygon()], [graphics::par()] for the
#'   description of the respective arguments.
#'
#' [plotrix::axis.break()] for cut marks
#'
#' See [plot()] for some predefined spectra plots such as mean
#'   spectrum +/- one standard deviation and the like.
#'
#' [graphics::identify()] and [graphics::locator()] about
#'   interaction with plots.
#'
#' @keywords hplot
#' @concept plotting
#' @concept plot generation
#'
#' @export
#' @examples
#'
#' plotspc(flu)
#'
#' ## artificial example to show wavelength axis cutting
#' plotspc(faux_cell[sample(nrow(faux_cell), 50)],
#'   wl.range = list(600 ~ 650, 1000 ~ 1100, 1600 ~ 1700),
#'   xoffset = c(0, 300, 450)
#' )
#'
#' plotspc(faux_cell[sample(nrow(faux_cell), 50)],
#'   wl.range = list(600 ~ 650, 1000 ~ 1100, 1600 ~ 1700),
#'   xoffset = c(300, 450)
#' )
#'
#' ## some journals publish Raman spectra backwards
#' plotspc(faux_cell[sample(nrow(faux_cell), 50)], wl.reverse = TRUE)
#'
#' plotspc(laser[(0:4) * 20 + 1, , ], stacked = TRUE)
#'
#' plotspc(laser,
#'   func = mean_pm_sd,
#'   col = c(NA, "red", "black"), lines.args = list(lwd = 2),
#'   fill = c(1, NA, 1),
#'   fill.col = "yellow", border = "blue",
#'   polygon.args = list(lty = 2, lwd = 4),
#'   title.args = list(
#'     xlab = expression(lambda[emission] / nm),
#'     y = list(line = 3.4),
#'     col.lab = "darkgreen"
#'   ),
#'   axis.args = list(x = list(col = "magenta"), y = list(las = 1))
#' )
#'
#' mean.pm.sd <- aggregate(faux_cell, faux_cell$region, mean_pm_sd)
#' plot(mean.pm.sd, col = palette_matlab(3), fill = ".aggregate", stacked = ".aggregate")
#' @importFrom utils modifyList relist head tail
#' @importFrom grDevices rgb col2rgb
plotspc <- function(object,
                    ## what wavelengths to plot
                    wl.range = TRUE, wl.index = FALSE, wl.reverse = FALSE,
                    ## what spectra to plot
                    spc.nmax = hy.getOption("plot.spc.nmax"),
                    func = NULL, func.args = list(),
                    stacked = NULL, stacked.args = list(),
                    ## plot area
                    add = FALSE, bty = "l", plot.args = list(),
                    ## lines
                    col = "black", lines.args = list(),
                    ## axes
                    xoffset = 0, yoffset = 0, nxticks = 10, axis.args = list(),
                    break.args = list(),
                    ## title (axis labels)
                    title.args = list(),
                    ## parameters for filled regions
                    fill = NULL, fill.col = NULL, border = NA, polygon.args = list(),
                    ## line indicating zero intensity
                    zeroline = list(lty = 2, col = col),
                    debuglevel = hy.getOption("debuglevel")) {
  force(zeroline) # otherwise stacking messes up colors

  chk.hy(object)
  validObject(object)
  if (nrow(object) == 0) stop("No spectra.")

  ## prepare wavelengths ............................................................................
  ## somewhat more complicated here because of plotting with cut wavelength axis
  #  wl.range <- lazy (wl.range)
  #  browser ()
  #  if (is.null (wl.range$expr)) {
  #    wl.range <- seq_along (object@wavelength)
  #    wl.index <- TRUE
  #  }

  #  if (!is.list (wl.range$expr))
  #    wl.range <- list (wl.range)

  if (!wl.index) {
    wl.range <- wl2i(object, wl.range, unlist = FALSE)
    wl.range <- lapply(wl.range, function(r) r[!is.na(r)])
  }

  ## xoffset ........................................................................................
  ## may be
  ## - one number for all wl.ranges
  ## - a number for each wl.range
  ## - one less than wl.ranges: first will be 0
  if (length(xoffset) == length(wl.range) - 1) {
    xoffset <- c(0, xoffset)
  } else if (length(xoffset) == 1) {
    xoffset <- rep(xoffset, times = length(wl.range))
  }
  if (!is.numeric(xoffset) || (length(xoffset) != length(wl.range))) {
    stop(
      "xoffset must be a numeric  vector of the same length (or one less) as the list with",
      "wavenumber ranges."
    )
  }
  xoffset <- cumsum(xoffset)

  ## for indexing wavelength.range is needed unlisted
  u.wl.range <- unlist(wl.range)

  ## wavelengths are the numbers to print at the x axis
  wavelengths <- relist(object@wavelength[u.wl.range], wl.range)

  ## x are the actual x coordinates
  x <- wavelengths
  for (i in seq_along(x)) {
    x[[i]] <- x[[i]] - xoffset[i]
  }

  ## prepare spectra ................................................................................
  ## indices into columns of spectra matrix spc
  ispc <- relist(seq_along(u.wl.range), wl.range)

  rm(wl.range)
  spc <- object[[, , u.wl.range, drop = FALSE, wl.index = TRUE]]
  rm(u.wl.range)


  ## summary statistics: apply function func to spc
  if (!is.null(func)) {
    if (!is.function(func)) {
      stop("func needs to be a function.")
    }

    apply.args <- c(list(X = spc, MARGIN = 2, FUN = func), func.args)
    spc <- matrix(do.call(apply, apply.args), # apply (spc, 2, func),
      ncol = ncol(spc)
    )
    if (nrow(spc) == 0) {
      stop("No spectra after", func, "was applied.")
    }
  }

  ## do not plot too many spectra by default: can take very long and there is most probably nothing
  ## visible on the resulting picture
  if (nrow(spc) > spc.nmax) {
    if (debuglevel >= 1L) {
      message("Number of spectra exceeds spc.nmax. Only the first", spc.nmax, "are plotted.")
    }

    spc <- spc[seq_len(spc.nmax), , drop = FALSE]
  }

  ## stacked plot
  if (!is.null(stacked)) {
    stacked.args <- modifyList(
      stacked.args,
      list(x = object, stacked = stacked, .spc = spc)
    )

    if (!is.null(lines.args$type) && lines.args$type == "h") {
      stacked.args <- modifyList(stacked.args, list(min.zero = TRUE))
    }

    stacked <- do.call(stacked.offsets, stacked.args)
    if (all(yoffset == 0)) {
      yoffset <- stacked$offsets[stacked$groups]
    } else if (length(yoffset) == length(unique(stacked$groups))) {
      yoffset <- yoffset[stacked$groups]
    }
  }

  ## yoffset ........................................................................................
  ## either one value for all spectra
  ## or one per spectrum or one per group
  if (length(yoffset) != nrow(spc)) {
    if (length(yoffset) == 1) {
      yoffset <- rep(yoffset, nrow(spc))
    } else if (length(yoffset) > nrow(spc)) {
      yoffset <- yoffset[seq_len(nrow(spc))]
    } else {
      stop("yoffset must be single number or one number for each spectrum (or stacking group).")
    }
  }

  spc <- sweep(spc, 1, yoffset, "+")

  ## plot area --------------------------------------------------------------------------------------

  ## should a new plot be set up?
  if (!add) {
    ## set default plot args
    plot.args <- modifyList(
      list(
        xlim = range(unlist(x), na.rm = TRUE),
        ylim = range(spc, na.rm = TRUE)
      ),
      plot.args
    )

    ## the actual spectra are plotted below, so we do not need any line parametrers here

    ## reverse x axis ?
    if (wl.reverse) {
      plot.args$xlim <- rev(plot.args$xlim)
    }

    ## some arguments must be overwritten if given:
    plot.args <- modifyList(
      plot.args,
      list(
        x = unlist(x), y = spc[1, , drop = FALSE],
        type = "n", bty = "n",
        xaxt = "n", yaxt = "n", # axes and title are called separately
        xlab = NA, ylab = NA
      )
    ) # for finer control

    do.call(plot, plot.args)

    ## reversed x axis leads to trouble with tick positions
    ##
    if (diff(plot.args$xlim) < 0) {
      plot.args$xlim <- rev(plot.args$xlim)
    }

    ## Axes -----------------------------------------------------------------------------------------
    axis.args <- modifyList(list(x = list(), y = list()), axis.args)

    ## x-axis labels & ticks
    if (bty %in% c("o", "l", "c", "u", "]", "x")) {
      cuts <- .cut.ticks(sapply(wavelengths, min), sapply(wavelengths, max), xoffset, nxticks)

      axis.args$x <- modifyList(
        axis.args[!names(axis.args) %in% c("x", "y")],
        axis.args$x
      )
      if (is.null(axis.args$x$labels) & !is.null(axis.args$x$at)) {
        axis.args$x$labels <- axis.args$x$at
      }
      axis.args$x <- modifyList(
        list(side = 1, at = cuts$at, labels = cuts$labels),
        axis.args$x
      )

      axis(side = 1, at = max(abs(plot.args$xlim)) * c(-1.1, 1.1))
      do.call(axis, axis.args$x)

      ## plot cut marks for x axis
      break.args <- modifyList(list(style = "zigzag"), break.args)
      break.args$axis <- NULL
      break.args$breakpos <- NULL

      if (length(cuts$cut) > 0) {
        if (!requireNamespace("plotrix")) {
          cat("hyperSpec will use its own replacement for plotrix' axis.break\n\n")
          break.fun <- .axis.break
        } else {
          break.fun <- plotrix::axis.break
        }
        for (i in cuts$cut) {
          do.call(break.fun, c(list(axis = 1, breakpos = i), break.args))
        }
      }
    }

    ## y-axis labels & ticks
    if (bty %in% c("o", "l", "c", "u", "y")) {
      axis.args$y <- modifyList(
        axis.args[!names(axis.args) %in% c("x", "y", "main", "sub")],
        axis.args$y
      )

      ## default for stacked plots is marking the groups
      if (!is.null(stacked)) {
        if (!is.null(stacked.args$min.zero) && stacked.args$min.zero) {
          group.mins <- stacked$offsets
        } else {
          group.mins <- apply(spc[!duplicated(stacked$groups), , drop = FALSE], 1, min, na.rm = TRUE)
        }

        axis.args$y <- modifyList(
          list(
            at = stacked$offsets,
            labels = stacked$levels[!duplicated(stacked$levels)]
          ),
          axis.args$y
        )
      }

      axis.args$y <- modifyList(list(side = 2), axis.args$y)
      axis(side = 2, at = max(abs(plot.args$ylim)) * c(-1.1, 1.1))
      do.call(axis, axis.args$y)
    }

    ## Title: axis labels ---------------------------------------------------------------------------

    tmp <- title.args[!names(title.args) %in% c("x", "y", "ylab", "main", "sub")]
    tmp <- modifyList(tmp, as.list(title.args$x))

    tmp <- modifyList(list(xlab = object@label$.wavelength, line = 2.5), tmp)
    do.call(title, tmp)
    tmp$xlab <- NULL

    tmp <- title.args[!names(title.args) %in% c("x", "y", "xlab", "main", "sub")]
    tmp <- modifyList(tmp, as.list(title.args$y))
    tmp <- modifyList(list(ylab = object@label$spc), tmp)
    do.call(title, tmp)
    tmp$ylab <- NULL

    tmp <- title.args[!names(title.args) %in% c("x", "y", "xlab", "ylab")]
    tmp <- modifyList(tmp, as.list(title.args[c("main", "sub")]))
    do.call(title, tmp)
  }

  ## plot the spectra -------------------------------------------------------------------------------

  ## if necessary, recycle colors
  col <- rep(col, each = ceiling(nrow(spc) / length(col)), length.out = nrow(spc))


  ## should the intensity zero be marked?
  if (!(is.logical(zeroline) && is.na(zeroline))) {
    zeroline <- modifyList(list(h = unique(yoffset)), as.list(zeroline))
    do.call(abline, zeroline)
  }

  ## start loop over wavelength ranges
  for (i in seq_along(x)) {
    ## filling for polygons ........................................................................

    ## groupings for upper and lower bound of the bands
    if (!is.null(fill)) {
      if (is.character(fill) && length(fill) == 1) {
        fill <- unlist(object[[, fill]])
      } else if (isTRUE(fill)) {
        fill <- seq_len(nrow(spc) / 2)
        if (nrow(spc) %% 2 == 1) { # odd number of spectra
          fill <- c(fill, NA, rev(fill))
        } else {
          fill <- c(fill, rev(fill))
        }
      } else if (is.factor(fill)) {
        fill <- as.numeric(fill)
      } else if (!is.numeric(fill)) {
        stop(
          "fill must be either TRUE, the name of the extra data column to use for grouping,",
          "a factor or a numeric."
        )
      }

      groups <- unique(fill)
      groups <- groups[!is.na(groups)]


      polygon.args <- modifyList(
        list(x = NULL, y = NULL),
        polygon.args
      )

      ## fill color
      if (is.null(fill.col)) {
        fill.col <- character(length(groups))

        for (j in seq_along(groups)) {
          tmp <- which(fill == groups[j])
          fill.col[j] <- rgb(t(col2rgb(col[tmp[1]]) / 255) / 3 + 2 / 3)
        }
      } else {
        fill.col <- rep(fill.col, length.out = length(groups))
      }

      border <- rep(border, length.out = length(groups))

      polygon.args$x <- c(x[[i]], rev(x[[i]]))

      for (j in seq_along(groups)) {
        tmp <- which(fill == groups[j])
        polygon.args$y <- c(spc[head(tmp, 1), ispc[[i]]], rev(spc[tail(tmp, 1), ispc[[i]]]))
        polygon.args$col <- fill.col[groups[j]]
        polygon.args$border <- border[groups[j]]

        do.call(polygon, polygon.args)
      }
    }

    ## lines ........................................................................................

    lines.args <- modifyList(list(x = NULL, y = NULL, type = "l"), lines.args)

    if (lines.args$type == "h" && is.list(stacked)) {
      ## specialty: lines from the stacked zero line on!
      for (j in seq_len(nrow(spc))) {
        keep <- !is.na(spc[j, ispc[[i]]])
        lines.args$x <- rep(x[[i]][keep], each = 3)
        lines.args$y <- as.numeric(matrix(c(
          rep(yoffset[j], sum(keep)),
          spc[j, ispc[[i]]][keep],
          rep(NA, sum(keep))
        ),
        byrow = TRUE, nrow = 3
        ))
        lines.args$type <- "l"
        lines.args$col <- col[j]
        do.call(lines, lines.args)
      }
    } else {
      for (j in seq_len(nrow(spc))) {
        keep <- !is.na(spc[j, ispc[[i]]])

        lines.args$x <- x[[i]][keep]
        lines.args$y <- spc[j, ispc[[i]]][keep]
        lines.args$col <- col[j]

        do.call(lines, lines.args)
      }
    }
  }

  ## return some values that are needed by spc.identify
  invisible(list(
    x = rep(unlist(x), each = nrow(spc)),
    y = spc,
    wavelengths = rep(unlist(wavelengths), each = nrow(spc))
  ))
}



#' y Offsets for Stacked Plots
#' Calculate approriate `yoffset` values for stacking in [hyperSpec::plotspc()].
#'
#' Usually, the `stacked` argument of [hyperSpec::plotspc()] will do fine, but if you
#' need fine control over the stacking, you may calculate the y offsets yourself.
#'
#' Empty levels of the stacking factor are dropped (as no stacking offset can be calculated in that
#' case.)
#'
#' @param x a `hyperSpec` object
#' @param min.zero if `TRUE`, the lesser of zero and the minimum intensity of the spectrum is
#' used as minimum.
#' @param add.factor,add.sum proportion and absolute amount of space that should be added.
#' @param .spc for internal use. If given, the ranges are evaluated on `.spc`. However, this
#' may change in future.
#' @return a list containing \item{offsets}{numeric with the yoffset for each group in
#' `stacked`} \item{groups}{numeric with the group number for each spectrum} \item{levels}{if
#' `stacked` is a factor, the levels of the groups}
#' @author C. Beleites
#' @seealso [hyperSpec::plotspc()]
#' @rdname plotspc
#'
#' @concept plotting
#' @concept plot generation
#'
#' @export
#' @examples
#'
#' mean.pm.sd <- aggregate(faux_cell, faux_cell$region, mean_pm_sd)
#'
#' offset <- stacked.offsets(mean.pm.sd, ".aggregate")
#' plot(mean.pm.sd,
#'   fill.col = palette_matlab(3), fill = ".aggregate",
#'   stacked = ".aggregate"
#' )
#'
#' plot(aggregate(faux_cell, faux_cell$region, mean),
#'   yoffset = offset$offsets,
#'   lines.args = list(lty = 2, lwd = 2), add = TRUE
#' )
#'
#' barb <- do.call(collapse, barbiturates[1:3])
#' plot(barb,
#'   lines.args = list(type = "h"), stacked = TRUE,
#'   stacked.args = list(add.factor = .2)
#' )
stacked.offsets <- function(x, stacked = TRUE,
                            min.zero = FALSE, add.factor = 0.05, add.sum = 0,
                            # tight = FALSE, TODO
                            .spc = NULL, debuglevel = hy.getOption("debuglevel")) {
  lvl <- NULL

  if (is.null(.spc)) {
    .spc <- x@data$spc
  }

  if (is.character(stacked)) {
    stacked <- unlist(x[[, stacked]])
  } else if (isTRUE(stacked)) {
    stacked <- row.seq(x)
  }

  ## cut stacked if necessary
  if (length(stacked) != nrow(.spc)) {
    stacked <- rep(stacked, length.out = nrow(.spc))
    if (debuglevel >= 1L) {
      message("stacking variable recycled to ", nrow(.spc), " values.")
    }
  }
  if (is.numeric(stacked)) {
    stacked <- as.factor(stacked)
  } else if (!is.factor(stacked)) {
    stop("stacked must be either TRUE, the name of the extra data column to use for grouping, a factor or a numeric.")
  }

  stacked <- droplevels(stacked)
  lvl <- levels(stacked)
  groups <- seq_along(levels(stacked))
  stacked <- as.numeric(stacked)

  offset <- matrix(nrow = 2, ncol = length(groups))

  for (i in groups) {
    offset[, i] <- range(.spc[stacked == groups[i], ], na.rm = TRUE)
  }

  ## should the minimum be at zero (or less)?
  if (min.zero) {
    offset[1, ] <- sapply(offset[1, ], min, 0, na.rm = TRUE)
  }

  offset[2, ] <- offset[2, ] - offset[1, ]

  ## add some extra space
  offset[2, ] <- offset[2, ] * (1 + add.factor) + add.sum

  offset <- c(-offset[1, ], 0) + c(0, cumsum(offset[2, ]))

  list(
    offsets = offset[seq_along(groups)],
    groups = stacked,
    levels = if (is.null(lvl)) stacked else lvl
  )
}


hySpc.testthat::test(stacked.offsets) <- function() {
  context("stacked.offsets")

  test_that("ranges do not overlap", {
    spc <- do.call(collapse, barbiturates[1:3])
    ofs <- stacked.offsets(spc)
    spc <- spc + ofs$offsets
    rngs <- apply(spc[[]], 1, range, na.rm = TRUE)

    expect_equal(as.numeric(rngs), sort(rngs))
  })

  test_that("extra space", {
    spc <- new("hyperSpec", spc = matrix(c(0, 0, 2, 1:3), nrow = 3))

    expect_equal(stacked.offsets(spc, add.factor = 0)$offsets, c(0, 1, 1))
    expect_equal(stacked.offsets(spc, add.factor = 1)$offsets, c(0, 2, 4))
    expect_equal(stacked.offsets(spc, add.factor = 0, add.sum = 1)$offsets, c(0, 2, 3))
  })

  test_that("min.zero", {
    ofs <- stacked.offsets(flu, min.zero = TRUE, add.factor = 0)
    expect_equal(
      ofs$offsets,
      c(0, cumsum(apply(flu[[-nrow(flu)]], 1, max)))
    )
  })
}

###  .axis.break - poor man's version of axis.break
.axis.break <- function(axis = 1, breakpos = NULL, ...) {
  mtext("//", at = breakpos, side = axis, padj = -1, adj = 0.5)
}

### .cut.ticks - pretty tick marks for cut axes
#' @importFrom utils head
.cut.ticks <- function(start.ranges,
                       end.ranges,
                       offsets,
                       nticks) {
  stopifnot(length(start.ranges) == length(end.ranges) &
    length(start.ranges) == length(offsets))

  ## if (length (start.ranges) == 1)


  ## what part of the plot is occupied by each range?
  part <- abs(end.ranges - start.ranges) / (max(end.ranges) - min(start.ranges) - max(offsets))

  ## nice labels
  labels <- mapply(function(start, end, part) pretty(c(start, end), part * nticks + 1),
    start.ranges, end.ranges, part,
    SIMPLIFY = FALSE
  )

  ## user coordinates
  at <- mapply(`-`, labels, offsets, SIMPLIFY = FALSE)

  ## cut marks

  ## convert to device x in user coordinates
  start.ranges <- start.ranges - offsets
  end.ranges <- end.ranges - offsets

  delta <- start.ranges[-1] - head(end.ranges, -1)

  cutmarks <- head(end.ranges, -1) + delta / 2

  ## make sure that the ticks are not too close
  for (i in seq_along(delta)) {
    keep <- at[[i]] < end.ranges[i] + delta[i] / 4
    at[[i]] <- at[[i]][keep]
    labels[[i]] <- labels[[i]][keep]

    keep <- at[[i + 1]] > start.ranges[i + 1] - delta[i] / 4
    at[[i + 1]] <- at[[i + 1]][keep]
    labels[[i + 1]] <- labels[[i + 1]][keep]
  }

  list(
    labels = as.numeric(unlist(labels)),
    at = as.numeric(unlist(at)),
    cut = cutmarks
  )
}


hySpc.testthat::test(.cut.ticks) <- function() {
  context(".cut.ticks")

  ## bugfix:
  ## plotspc (paracetamol, wl.range = c (min ~ 1800, 2800 ~ max), xoffset = 900)
  ## had 2600 1/cm label printed in low wavelength range
  test_that("labels not too far outside wl.range", {
    expect_equal(
      .cut.ticks(
        start.ranges = c(96.7865, 2799.86),
        end.ranges = c(1799.95, 3200.07),
        offsets = c(0, 900),
        nticks = 10
      )$labels,
      c(seq(0, 1800, 200), seq(2800, 3400, 200))
    )
  })

  test_that("correct calculations", {
    labels <- c(
      seq(1, 2, 0.5),
      seq(3, 4, 0.5),
      seq(7, 9, 0.5)
    )

    expect_equal(
      .cut.ticks(
        start.ranges = c(1, 3, 7),
        end.ranges = c(2, 4, 9),
        nticks = 10,
        offsets = c(0, 0, 1)
      ),
      list(
        labels = labels,
        at = labels - c(
          0, 0, 0,
          0, 0, 0,
          1, 1, 1, 1, 1
        ),
        cut = c(
          mean(c(3, 2)),
          mean(c(7 - 1, 4))
        )
      )
    )
  })
}
