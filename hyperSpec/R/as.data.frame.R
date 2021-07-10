#' Conversion of a `hyperSpec` object into a data frame or matrix
#'
#' `as.data.frame()` returns `x@@data` (as data.frame), `as.matrix()`
#' returns the spectra matrix `x@@data$spc` as matrix.
#'
#' @rdname asdataframe
#' @name as.data.frame
#' @aliases as.data.frame as.data.frame,hyperSpec-method as.data.frame.hyperSpec
#' @docType methods
#' @param x a `hyperSpec` object
#' @param row.names if `TRUE`, a column `.row` is created containing row names or row
#' indices if no rownames are set. If character vector, the rownames are set accordingly.
#' @param optional ignored
#' @return `x@@data` and `x@@data$spc` (== `x$spc` == `x[[]]`), respectively.
#' @author C. Beleites
#' @method as.data.frame hyperSpec
#' @export
#' @seealso [base::as.data.frame()]
#'
#' @keywords methods
#' @concept hyperSpec conversion
#'
#' @examples
#'
#' as.data.frame(faux_cell[1:3, , 600 ~ 620])
#' as.matrix(faux_cell[1:3, , 600 ~ 620])
#' lm(c ~ spc, data = flu[, , 450])
as.data.frame.hyperSpec <- function(x, row.names = TRUE, optional = NULL, ...) {
  validObject(x)

  x <- x@data

  if (!is.null(row.names)) {
    if (isTRUE(row.names)) {
      if (is.null(rownames(x))) {
        x$.row <- seq_len(nrow(x))
      } else {
        x$.row <- rownames(x)
      }
    } else {
      rownames(x) <- row.names
    }
  }

  x
}
#' @method as.matrix hyperSpec
#' @rdname asdataframe
#' @param ... ignored
#' @aliases as.matrix as.matrix,hyperSpec-method
#' @export
#'
#' @concept hyperSpec conversion
#'
#' @seealso and [base::as.matrix()]
#'
#' `[[]]` for a shortcut to `as.matrix`
as.matrix.hyperSpec <- function(x, ...) {
  validObject(x)

  unclass(x@data$spc) # remove the AsIs
}


#' `as.wide.df()` converts the spectra matrix to a data.frame. The extra
#' data together with this data is returned. The column names of the spectra
#' matrix are retained (if they are numbers, without preceeding letters).
#'
#' @param wl.prefix prefix to prepend wavelength column names
#' @rdname asdataframe
#' @aliases  as.wide.df
#' @export
#'
#' @concept hyperSpec conversion
#'
#' @return
#'
#' `as.wide.df` returns a data.frame that consists of the extra data and
#'   the spectra matrix converted to a data.frame. The spectra matrix is
#'   expanded *in place*.
#' @examples
#'
#' as.wide.df(faux_cell[1:5, , 600 ~ 610])
#' summary(as.wide.df(faux_cell[1:5, , 600 ~ 610]))
as.wide.df <- function(x, wl.prefix = "") {
  chk.hy(x)
  validObject(x)

  ispc <- match("spc", colnames(x@data))

  ## logical indexing creates n by 0 data.frame that can be cbound, thus
  ## avoiding trouble with empty or 0 x 0 data.frames:
  before <- seq_len(ncol(x@data)) < ispc
  after <- seq_len(ncol(x@data)) > ispc

  ## colnames should be preserved
  cols <- c(
    colnames(x@data)[before],
    paste0(wl.prefix, colnames(x@data$spc)),
    colnames(x@data)[after]
  )

  x <- cbind(
    x@data[, before],
    as.data.frame(unclass(x@data[, ispc])),
    x@data[, after]
  )
  colnames(x) <- cols
  x
}

hySpc.testthat::test(as.wide.df) <- function() {
  context("as.wide.df")

  test_that("faux_cell", {
    expect_equal(
      as.wide.df(faux_cell[1:5, , 600 ~ 610]),
      cbind(faux_cell[1:5]$.., faux_cell[[1:5, , 600 ~ 610]])
    )
  })

  test_that("column names", {
    expect_equal(
      colnames(as.wide.df(faux_cell)),
      c(grep("spc", colnames(faux_cell), value = TRUE, invert = TRUE), colnames(faux_cell$spc))
    )

    expect_equal(
      colnames(as.wide.df(faux_cell)),
      c(grep("spc", colnames(faux_cell), value = TRUE, invert = TRUE), wl(faux_cell))
    )

    expect_true(!any(is.na(colnames(as.wide.df(barbiturates[[1]])))))
  })

  test_that("column names with wl.prefix", {
    expect_equal(
      colnames(as.wide.df(faux_cell, wl.prefix = "wl")),
      c(grep("spc", colnames(faux_cell), value = TRUE, invert = TRUE), paste0("wl", colnames(faux_cell$spc)))
    )
  })
}

#' `as.long.df` returns a long-format data.frame.
#'
#' The data.frame returned by `as.long.df` is guaranteed to have columns
#' `spc` and `.wavelength`. If `nwl (x) == 0` these columns
#' will be `NA`.
#'
#' @rdname asdataframe
#' @aliases as.long.df
#' @param rownames should the rownames be in column `.rownames` of the
#'   long-format data.frame?
#' @param wl.factor should the wavelengths be returned as a factor (instead of
#'   numeric)?
#' @param na.rm if `TRUE`, rows where spc is not `NA` are deleted.
#' @export
#'
#' @concept hyperSpec conversion
#'
#' @return `as.long.df` returns the stacked or molten version of `x@@data`. The
#'   wavelengths are in column `.wavelength`.
#' @seealso
#'
#' [utils::stack()] and [reshape::melt()] or [reshape2::melt()] for
#' other functions producing long-format data.frames.
#' @examples
#'
#' as.long.df(flu[, , 405 ~ 410])
#' summary(as.long.df(flu[, , 405 ~ 410]))
#' summary(as.long.df(flu[, , 405 ~ 410], rownames = TRUE))
#' summary(as.long.df(flu[, , 405 ~ 410], wl.factor = TRUE))
as.long.df <- function(x, rownames = FALSE, wl.factor = FALSE, na.rm = TRUE) {
  chk.hy(x)
  validObject(x)

  ispc <- match("spc", colnames(x@data))

  if (nwl(x) == 0) {
    tmp <- cbind(
      data.frame(
        .wavelength = rep(NA, nrow(x)),
        spc = rep(NA, nrow(x))
      ),
      x@data[, -ispc, drop = FALSE]
    )
  } else {
    tmp <- x@data[rep(row.seq(x), nwl(x)), -ispc, drop = FALSE]

    tmp <- cbind(
      data.frame(
        .wavelength = rep(x@wavelength, each = nrow(x)),
        spc = as.numeric(x[[]])
      ),
      tmp
    )
    if (wl.factor) {
      tmp$.wavelength <- as.factor(tmp$.wavelength)
      wl <- colnames(x@data$spc) # there may be a fancily formatted version in the column
      # names
      if (is.null(wl)) {
        wl <- x@wavelength
      } # if not, use the wavelength vector

      levels(tmp$.wavelength) <- wl
    }
  }

  if (rownames) {
    tmp <- data.frame(
      .rownames = as.factor(rep(rownames(x),
        length.out = nrow(tmp)
      )),
      tmp
    )
  }

  if (na.rm) {
    tmp <- tmp[!is.na(tmp$spc), ]
  }

  tmp
}

#'
#' `as.t.df` produces a 'transposed' data.frame with columns containing the spectra.
#' @rdname asdataframe
#' @aliases as.t.df
#' @return `as.t.df` returns a data.frame similar to `as.long.df`, but each
#'   spectrum in its own column. This is useful for exporting summary spectra,
#'   see the example.
#' @export
#' @examples
#' df <- as.t.df(apply(faux_cell, 2, mean_pm_sd))
#' head(df)
#'
#' if (require(ggplot2)) {
#'   ggplot(df, aes(x = .wavelength)) +
#'     geom_ribbon(aes(ymin = mean.minus.sd, ymax = mean.plus.sd),
#'       fill = "#00000040"
#'     ) +
#'     geom_line(aes(y = mean))
#' }
as.t.df <- function(x) {
  chk.hy(x)
  validObject(x)

  df <- as.data.frame(t(unclass(x@data$spc)))
  colnames(df) <- rownames(x@data)

  cbind(.wavelength = x@wavelength, df)
}
