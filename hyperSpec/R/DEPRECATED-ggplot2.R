#' @name DEPRECATED-ggplot2
#' @concept moved to hySpc.ggplot2
#'
#' @title (DEPRECATED)
#'        Spectra plotting with \pkg{ggplot2} was moved to \pkg{hySpc.ggplot2}
#'
#' @description
#'
#' These \pkg{ggplot2}-related \pkg{hyperSpec} functions are **deprecated**
#' and they will be removed in the next release of the package.
#' Now functions from package \pkg{hySpc.ggplot2}
#' ([link](https://r-hyperspec.github.io/hySpc.ggplot2/reference/index.html))
#' should be used as alternatives to plot `hyperSpec` objects with \pkg{ggplot2}.
#'
#' @author Claudia Beleites
#'
#'
#' @import ggplot2
#' @importFrom grid pushViewport viewport popViewport grid.layout unit
#' @importFrom utils tail
#' @importFrom rlang as_label
#' @importFrom lazyeval f_rhs
#' @importFrom grDevices col2rgb rgb
NULL


#' @rdname DEPRECATED-ggplot2
#'
#' @param x `hyperSpec` object
#' @param wl.range wavelength ranges to plot
#' @param ... handed to [ggplot2::geom_line()]
#' @param mapping see  [ggplot2::geom_line()]
#' @param spc.nmax maximum number of spectra to plot
#' @param map.lineonly if `TRUE`, `mapping` will be handed to
#' [ggplot2::geom_line()] instead of [ggplot2::ggplot()].
#' @param debuglevel if > 0, additional debug output is produced
#'
#' @export
#'
#' @examples
#' qplotspc(flu)
#'
qplotspc <- function(x,
                     wl.range = TRUE, ...,
                     mapping = aes_string(
                       x = ".wavelength", y = "spc",
                       group = ".rownames"
                     ),
                     spc.nmax = hy.getOption("ggplot.spc.nmax"),
                     map.lineonly = FALSE,
                     debuglevel = hy.getOption("debuglevel")) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_ggplot2(new = "qplotspc")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  chk.hy(x)
  validObject(x)

  ## cut away everything that isn't asked for before transforming to data.frame
  if (nrow(x) > spc.nmax) {
    if (debuglevel >= 1L) {
      message(
        "Number of spectra exceeds spc.nmax. Only the first ", spc.nmax,
        " are plotted."
      )
    }
    x <- x[seq_len(spc.nmax)]
  }

  wl.range <- wl2i(x, wl.range, unlist = FALSE)

  x <- x[, , unlist(wl.range), wl.index = TRUE]

  df <- as.long.df(x, rownames = TRUE, na.rm = FALSE) # with na.rm trouble with
  # wl.range

  ## ranges go into facets
  if (length(wl.range) > 1L) {
    tmp <- wl.range
    for (r in seq_along(tmp)) {
      tmp[[r]][TRUE] <- r
    }

    df$.wl.range <- rep(unlist(tmp), each = nrow(x))
  }


  df <- df[!is.na(df$spc), , drop = FALSE]
  if (map.lineonly) {
    p <- ggplot(df) +
      geom_line(mapping = mapping, ...)
  } else {
    p <- ggplot(df, mapping = mapping) +
      geom_line(...)
  }

  p <- p + xlab(labels(x, ".wavelength")) + ylab(labels(x, "spc"))

  if (!is.null(df$.wl.range)) {
    p <- p + facet_grid(. ~ .wl.range,
      labeller = as_labeller(rep(NA, nlevels(df$.wl.range))),
      scales = "free", space = "free"
    ) +
      theme(strip.text.x = element_text(size = 0))
  }

  p
}


#' @rdname DEPRECATED-ggplot2
#'
#' @param object  hyperSpec object
#' @param mapping see  [ggplot2::geom_tile()]
#' @param ... handed to [ggplot2::geom_tile()]
#' @param func function to summarize the wavelengths
#' @param func.args arguments to `func`
#' @param map.tileonly if `TRUE`, `mapping` will be handed to
#'   [ggplot2::geom_tile()] instead of [ggplot2::ggplot()].
#'
#' @importFrom utils tail
#' @importFrom rlang as_label
#'
#' @export
#'
#' @examples
#' qplotmap(faux_cell[, , 1200])
#'
qplotmap <- function(object,
                     mapping = aes_string(x = "x", y = "y", fill = "spc"),
                     ...,
                     func = mean, func.args = list(),
                     map.tileonly = FALSE) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_ggplot2(new = "qplotmap")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chk.hy(object)
  validObject(object)

  if (nwl(object) > 1 & !is.null(func)) {
    object <- do.call(apply, c(list(object, 1, func), func.args))
  }

  if (map.tileonly) {
    p <- ggplot(as.long.df(object)) +
      geom_tile(mapping = mapping)
  } else {
    p <- ggplot(as.long.df(object), mapping = mapping) +
      geom_tile()
  }

  p <- p + coord_equal()

  ## set expand to c(0, 0) to suppress the gray backgroud
  if (is.factor(with(p$data, eval(p$mapping$x)))) {
    p <- p + scale_x_discrete(expand = c(0, 0))
  } else {
    p <- p + scale_x_continuous(expand = c(0, 0))
  }

  if (is.factor(with(p$data, eval(p$mapping$y)))) {
    p <- p + scale_y_discrete(expand = c(0, 0))
  } else {
    p <- p + scale_y_continuous(expand = c(0, 0))
  }

  ## generate axis/scale labels
  ## TODO: own function
  x <- as_label(mapping$x)
  xlabel <- labels(object)[[tail(x, 1)]]
  if (is.null(xlabel)) xlabel <- x

  y <- as_label(mapping$y)
  ylabel <- labels(object)[[tail(y, 1)]]
  if (is.null(ylabel)) ylabel <- y

  f <- as_label(mapping$fill)
  flabel <- labels(object)[[tail(f, 1)]]
  if (is.null(flabel)) flabel <- f

  p + labs(x = xlabel, y = ylabel, fill = flabel)
}


#' @rdname DEPRECATED-ggplot2
#'
#' @param object hyperSpec object
#' @param mapping see  [ggplot2::geom_point()]
#' @param ... handed to [ggplot2::geom_point()]
#' @param func function to summarize the wavelengths, if `NULL`, only the first
#'   wavelength is used
#' @param func.args arguments to `func`
#' @param map.pointonly if `TRUE`, `mapping` will be handed to
#'   [ggplot2::geom_point()] instead of [ggplot2::ggplot()].
#'
#' @export
#'
#' @examples
#' qplotc(flu)
#'
qplotc <- function(object, mapping = aes_string(x = "c", y = "spc"), ...,
                   func = NULL, func.args = list(),
                   map.pointonly = FALSE) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_ggplot2(new = "qplotc")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chk.hy(object)
  validObject(object)

  if (!is.null(func)) {
    object <- do.call(apply, c(list(object, 1, func), func.args))
  }

  ## allow to plot against the row number
  object$.row <- seq(object, index = TRUE)

  ## find out whether the wavelengths are needed individually,
  ## if not, use only the first wavelength and issue a warning

  if (any(grepl("spc", sapply(mapping, as_label))) && # use intensities
    nwl(object) > 1 && # has > 1 wavelength
    is.null(func) && # no stats function
    !any(grepl("[.]wavelength", sapply(mapping, as_label)))) {
    object <- object[, , 1, wl.index = TRUE]
    warning("Intensity at first wavelengh only is used.")
  }

  ## produce fancy y label
  ylabel <- labels(object, as_label(mapping$y))
  if (is.null(ylabel)) {
    ylabel <- as_label(mapping$y)
  }

  if (!is.null(func)) {
    ylabel <- make.fn.expr(substitute(func), c(ylabel, func.args))
  }
  ylabel <- as.expression(ylabel)

  ## expand the data.frame
  df <- as.long.df(object, rownames = TRUE, wl.factor = TRUE)

  ## if plots should be grouped, faceted, etc. by wavelength, it is better to
  ## have a factor
  if (any(grepl(
    "[.]wavelength",
    sapply(mapping[!names(mapping) %in% c("x", "y")], as_label)
  ))
  ) {
    df$.wavelength <- as.factor(df$.wavelength)
  }

  if (map.pointonly) {
    p <- ggplot(df) +
      geom_point(mapping = mapping, ...)
  } else {
    p <- ggplot(df, mapping = mapping) +
      geom_point(...)
  }

  xlabel <- labels(object)[[as_label(mapping$x)]]
  if (is.null(xlabel)) {
    xlabel <- as_label(mapping$x)
  }

  p + ylab(ylabel) +
    xlab(xlabel)
}

make.fn.expr <- function(fn, l = list()) {
  if (length(fn) > 1L) {
    fn <- "f"
  }

  l <- lapply(l, function(x) if (is.logical(x)) as.character(x) else x)

  if (is.null(names(l))) {
    names(l) <- rep("", length(l))
  }

  tmp <- mapply(
    function(x, y) if (nzchar(x) > 0L) bquote(.(x) == .(y)) else y,
    names(l), l
  )

  e <- expression(f(x))
  e[[1]][[1]] <- fn
  if (length(tmp) > 0L) {
    e[[1]][seq_along(tmp) + 1] <- tmp
  } else {
    e[[1]][2] <- NULL
  }

  e
}


#' @rdname DEPRECATED-ggplot2
#'
#' @param object `hyperSpec` object
#' @param ... handed over to [hyperSpec::qmixlegend()] and [hyperSpec::qmixtile()]
#'
#' @concept moved to hySpc.ggplot2
#'
#' @import ggplot2
#' @importFrom grid pushViewport viewport popViewport grid.layout unit
#' @importFrom lazyeval f_rhs
#'
#' @export
#'
#' @examples
#' faux_cell <- faux_cell - spc_fit_poly_below(faux_cell)
#'
#' qplotmixmap(faux_cell [, , c(800, 1200, 1500)],
#'   purecol = c(A = "green4", B = "yellow", C = "royalblue")
#' )
#'
qplotmixmap <- function(object, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_ggplot2(new = "qplotmixmap")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Suppress excessive deprecation warnings from internally used functions.
  suppress_warnings(classes = "deprecatedWarning", expr = {

    p <- qmixtile(object@data, ...) +
      coord_equal()

    xlabel <- labels(object)[[as_label(p$mapping$x)]]
    if (is.null(xlabel)) {
      xlabel <- as_label(p$mapping$x)
    }

    ylabel <- labels(object)[[as_label(p$mapping$y)]]
    if (is.null(ylabel)) {
      ylabel <- as_label(p$mapping$y)
    }

    p <- p +
      xlab(xlabel) +
      ylab(ylabel)

    l <- qmixlegend(object@data$spc, ...)

    legendright(p, l)
  })

  invisible(list(map = p, legend = l))
}

#' @rdname DEPRECATED-ggplot2
#'
#' @param p plot object
#' @param l legend object
#' @param legend.width,legend.unit size of legend part
#'
#' @export
legendright <- function(p, l, legend.width = 8, legend.unit = "lines") {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_ggplot2()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  plot.new()
  pushViewport(viewport(layout = grid.layout(1, 2,
    widths = unit(c(1, legend.width), c("null", legend.unit))
  )))
  print(p, viewport(layout.pos.col = 1), newpage = FALSE)
  print(l, viewport(layout.pos.col = 2), newpage = FALSE)
  popViewport()
}


#' @rdname DEPRECATED-ggplot2
#'
#' @param object matrix to be plotted with mixed colour channels
#' @param purecol pure component colours, names determine legend labels
#' @param mapping see [ggplot2::geom_tile()]
#' @param ... `qmixtile`: handed to [colmix.rgb()]
#'
#'   `qmixlegend()` and `colmix.rgb()` hand further arguments to the
#'   `normalize` function
#' @param map.tileonly if `TRUE`, `mapping` will be handed to
#'   [ggplot2::geom_tile()] instead of [ggplot2::ggplot()].
#'
qmixtile <- function(object,
                     purecol = stop("pure component colors needed."),
                     mapping = aes_string(x = "x", y = "y", fill = "spc"),
                     ...,
                     map.tileonly = FALSE) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_ggplot2()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## calculate fill colours
  fill <- colmix.rgb(object[[f_rhs(mapping$fill)]], purecol, ...)
  object[[f_rhs(mapping$fill)]] <- fill

  if (map.tileonly) {
    p <- ggplot(object) +
      geom_tile(mapping = mapping, data = object)
  } else {
    p <- ggplot(object, mapping = mapping) +
      geom_tile()
  }

  p + scale_fill_identity() + theme(legend.position = "none")
}

#' @rdname DEPRECATED-ggplot2
#'
#' @param na.rm see [base::min()]
#' @param legend should a legend be produced instead of normalized values?
#' @param n of colours to produce in legend
#'
#' @export
normalize.colrange <- function(x, na.rm = TRUE, legend = FALSE, n = 100, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_ggplot2()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## legend
  if (legend) {
    y <- apply(x, 2, function(x) seq(min(x), max(x), length.out = n))
    dy2 <- abs(y[2, ] - y[1, ]) / 2

    list(
      ymin = sweep(y, 2, dy2, `-`),
      ymax = sweep(y, 2, dy2, `+`),
      fill = apply(x, 2, function(x) seq(0, 1, length.out = n))
    )
  } else {
    ## normalized values
    x <- sweep(x, 2, apply(x, 2, min, na.rm = na.rm), `-`)
    sweep(x, 2, apply(x, 2, max, na.rm = na.rm), `/`)
  }
}

#' @rdname DEPRECATED-ggplot2
#' @export
normalize.range <- function(x, na.rm = TRUE, legend = FALSE, n = 100, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_ggplot2()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (legend) {
    y <- matrix(seq(min(x), max(x), length.out = n), nrow = n, ncol = ncol(x))
    dy2 <- abs(y[2, ] - y[1, ]) / 2

    list(
      ymin = sweep(y, 2, dy2, `-`),
      ymax = sweep(y, 2, dy2, `+`),
      fill = apply(x, 2, function(x) seq(0, 1, length.out = n))
    )
  } else {
    x <- x - min(x, na.rm = na.rm)
    x / max(x, na.rm = na.rm)
  }
}

#' @rdname DEPRECATED-ggplot2
#' @export
normalize.null <- function(x, na.rm = TRUE, legend = FALSE, n = 100, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_ggplot2()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (legend) {
    y <- apply(x, 2, function(x) seq(min(x), max(x), length.out = n))

    list(
      ymin = sweep(y, 2, min),
      ymax = sweep(y, 2, max),
      fill = apply(x, 2, function(x) seq(0, 1, length.out = n))
    )
  } else {
    x
  }
}

#' @rdname DEPRECATED-ggplot2
#'
#' @param min numeric with value corresponding to "lowest" colour for each column
#' @param max numeric with value corresponding to "hightest" colour for each column
#'
#' @export
normalize.minmax <- function(x, min = 0, max = 1, legend = FALSE, n = 100,
                             ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_ggplot2()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (legend) {
    y <- matrix(seq(0, 1, length.out = n), nrow = n, ncol = ncol(x))
    y <- sweep(y, 2, max - min, `*`)
    y <- sweep(y, 2, min, `+`)

    dy2 <- abs(y[2, ] - y[1, ]) / 2

    l <- list(
      ymin = sweep(y, 2, dy2, `-`),
      ymax = sweep(y, 2, dy2, `+`),
      ymax = y + dy2,
      fill = matrix(seq(0, 1, length.out = n), nrow = n, ncol = ncol(x))
    )

    l$ymin[1, ] <- pmin(l$ymin[1, ], apply(x, 2, min, na.rm = TRUE))
    l$ymax[n, ] <- pmax(l$ymax[n, ], apply(x, 2, max, na.rm = TRUE))

    l
  } else {
    x <- sweep(x, 2, min, `-`)
    sweep(x, 2, max, `/`)
  }
}

#' @rdname DEPRECATED-ggplot2
#'
#' @param dx width of label bar
#' @param ny number of colours in legend
#' @param labels component names
#'
#' @export
qmixlegend <- function(x, purecol, dx = 0.33, ny = 100, labels = names(purecol),
                       normalize = normalize.colrange, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_ggplot2()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.matrix(x)) {
    x <- matrix(x, ncol = 1)
  }

  if (is.null(labels)) {
    labels <- colnames(x)
  }
  if (is.null(labels)) {
    labels <- seq_len(ncol(x))
  }

  if (!is.null(normalize)) {
    l <- normalize(x, ..., legend = TRUE)
  } else {
    l <- x
  }

  df <- data.frame()
  for (column in seq_along(purecol)) {
    tmp <- colmix.rgb(l$fill[, column, drop = FALSE], purecol[column],
      normalize = NULL, ...
    )

    df <- rbind(df, data.frame(
      column = labels[column],
      col = tmp,
      ymin = l$ymin[, column],
      ymax = l$ymax[, column]
    ))
  }
  df$column <- as.factor(df$column)
  df$xmin <- as.numeric(df$column) - dx
  df$xmax <- as.numeric(df$column) + dx

  l <- ggplot(df, aes(x = column), col = col) +
    geom_point(aes(x = column, y = 1), col = NA) +
    ylab("") +
    xlab("")
  l <- l + geom_rect(aes_string(
    xmin = "xmin", xmax = "xmax", ymin = "ymin", ymax = "ymax",
    fill = "col", colour = "col"
  ))

  l <- l + theme(
    plot.margin = unit(c(0.5, 0, 0, 0), "lines"),
    legend.position = "none"
  ) +
    scale_fill_identity() + scale_colour_identity()

  l
}

#' @rdname DEPRECATED-ggplot2
#'
#' @param x matrix with component intensities in columns
#' @param against value to mix against (for `sub = TRUE` only, 1 = white, 0 = black)
#' @param sub subtractive color mixing?
#' @param normalize function to normalize the values.
#'
#' @concept moved to hySpc.ggplot2
#'
#' @importFrom grDevices col2rgb rgb
#' @export
colmix.rgb <- function(x, purecol, against = 1, sub = TRUE,
                       normalize = normalize.colrange, ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  deprecated_ggplot2()
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(normalize)) {
    x <- normalize(x, ...)
  }

  if (is.character(purecol)) {
    purecol <- t(col2rgb(purecol)) / 255
  }

  if (sub) {
    x <- against - x %*% (against - purecol)
  } else {
    x <- x %*% purecol
  }

  x[x < 0] <- 0
  x[x > 1] <- 1

  cols <- rep(NA, nrow(x))
  cols[!is.na(x[, 1])] <- rgb(x[!is.na(x[, 1]), ])

  cols
}

# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(qplotspc) <- function() {
  context("ggplot2")

  test_that("deprecated", {
    expect_warning(res <- qplotspc(flu), "deprecated")
    expect_warning(res <- qplotmap(flu), "deprecated")
    expect_warning(res <- qplotc(flu), "deprecated")
  })
}
