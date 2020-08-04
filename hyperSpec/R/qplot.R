#' @title Spectra plotting with \pkg{ggplot2}: `qplotspc().`
#' @description
#' Spectra plotting with \pkg{ggplot2}.
#'
#' These functions are still experimental and may change in future.
#'
#' @param x `hyperSpec` object
#' @param wl.range wavelength ranges to plot
#' @param ... handed to [ggplot2::geom_line()]
#' @param mapping see  [ggplot2::geom_line()]
#' @param spc.nmax maximum number of spectra to plot
#' @param map.lineonly if `TRUE`, `mapping` will be handed to
#' [ggplot2::geom_line()] instead of [ggplot2::ggplot()].
#' @param debuglevel if > 0, additional debug output is produced
#' @return a [ggplot2::ggplot()] object
#' @author Claudia Beleites
#'
#' @concept moved to hySpc.ggplot2
#'
#' @export
#'
#' @seealso [plotspc()]
#'
#' [ggplot2::ggplot()], [ggplot2::geom_line()]
#' @examples
#'
#' qplotspc(faux_cell)
#'
#' qplotspc(paracetamol, c(2800 ~ max, min ~ 1800)) +
#'   scale_x_reverse(breaks = seq(0, 3200, 400))
#'
#' qplotspc(aggregate(faux_cell, faux_cell$region, mean),
#'   mapping = aes(x = .wavelength, y = spc, colour = region)
#' ) +
#'   facet_grid(region ~ .)
#'
#' qplotspc(aggregate(faux_cell, faux_cell$region, mean_pm_sd),
#'   mapping = aes(x = .wavelength, y = spc, colour = region, group = .rownames)
#' ) +
#'   facet_grid(region ~ .)
qplotspc <- function(x,
                     wl.range = TRUE, ...,
                     mapping = aes_string(
                       x = ".wavelength", y = "spc",
                       group = ".rownames"
                     ),
                     spc.nmax = hy.getOption("ggplot.spc.nmax"),
                     map.lineonly = FALSE,
                     debuglevel = hy.getOption("debuglevel")) {
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

#' @title Spectra plotting with \pkg{ggplot2}: `qplotmap().`
#' @description
#' Spectra plotting with \pkg{ggplot2}.
#'
#' These functions are still experimental and may change in future.
#'
#' Note that `qplotmap()` will currently produce the wrong scales if x or y are
#' discrete.
#'
#' @param object  hyperSpec object
#' @param mapping see  [ggplot2::geom_tile()]
#' @param ... handed to [ggplot2::geom_tile()]
#' @param func function to summarize the wavelengths
#' @param func.args arguments to `func`
#' @param map.tileonly if `TRUE`, `mapping` will be handed to
#'   [ggplot2::geom_tile()] instead of [ggplot2::ggplot()].
#' @return a [ggplot2::ggplot()] object
#' @export
#'
#' @author Claudia Beleites
#'
#' @concept moved to hySpc.ggplot2
#'
#' @seealso [plotmap()]
#'
#'   [ggplot2::ggplot()], [ggplot2::geom_tile()]
#' @examples
#' qplotmap(faux_cell)
#' qplotmap(faux_cell) + scale_fill_gradientn(colours = alois.palette())
#' @importFrom utils tail
#' @importFrom rlang as_label
qplotmap <- function(object,
                     mapping = aes_string(x = "x", y = "y", fill = "spc"),
                     ...,
                     func = mean, func.args = list(),
                     map.tileonly = FALSE) {
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


#' @title Spectra plotting with \pkg{ggplot2}: `qplotc().`
#' @description
#' Spectra plotting with \pkg{ggplot2}.
#'
#' These functions are still experimental and may change in future.
#' @title Spectra plotting with ggplot2
#' @param object hyperSpec object
#' @param mapping see  [ggplot2::geom_point()]
#' @param ... handed to [ggplot2::geom_point()]
#' @export
#' @param func function to summarize the wavelengths, if `NULL`, only the first
#'   wavelength is used
#' @param func.args arguments to `func`
#' @param map.pointonly if `TRUE`, `mapping` will be handed to
#'   [ggplot2::geom_point()] instead of [ggplot2::ggplot()].
#' @return a [ggplot2::ggplot()] object
#' @author Claudia Beleites
#'
#' @concept moved to hySpc.ggplot2
#'
#' @seealso [plotc()]
#'
#'   [ggplot2::ggplot()], [ggplot2::geom_point()]
#' @examples
#' qplotc(flu)
#' qplotc(flu) + geom_smooth(method = "lm")
qplotc <- function(object, mapping = aes_string(x = "c", y = "spc"), ...,
                   func = NULL, func.args = list(),
                   map.pointonly = FALSE) {
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
