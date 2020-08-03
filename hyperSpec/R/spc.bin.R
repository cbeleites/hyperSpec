#' Wavelength Binning.
#'
#' In order to reduce the spectral resolution and thus gain signal to noise
#' ratio or to reduce the dimensionality of the spectral data set, the
#' spectral resolution can be reduced.
#'
#' The mean of every `by` data points in the spectra is calculated.
#'
#' Using `na.rm = TRUE` always takes about twice as long as `na.rm = FALSE`.
#'
#' If the spectra matrix does not contain too many `NA`s, `na.rm = 2` is faster than
#' `na.rm = TRUE`.
#'
#' @param spc the `hyperSpec` object
#' @param by reduction factor
#' @param na.rm decides about the treatment of `NA`s:
#'
#' if `FALSE` or `0`, the binning is done using `na.rm = FALSE`
#'
#' if `TRUE` or `1`, the binning is done using `na.rm = TRUE`
#'
#' if `2`, the binning is done using `na.rm = FALSE`, and resulting `NA`s are
#' corrected with \code{mean(\dots{}, na.rm = TRUE)}.
#' @param ... ignored
#' @return A `hyperSpec` object with `ceiling (nwl (spc) / by)` data points per spectrum.
#' @rdname spc-bin
#'
#' @export
#'
#' @keywords manip datagen
#' @concept spectra smoothing
#'
#' @author C. Beleites
#' @examples
#'
#' spc <- spc.bin(flu, 5)
#'
#' plot(flu[1, , 425:475])
#' plot(spc[1, , 425:475], add = TRUE, col = "blue")
#'
#' nwl(flu)
#' nwl(spc)
spc.bin <- function(spc, by = stop("reduction factor needed"), na.rm = TRUE, ...) {
  chk.hy(spc)
  validObject(spc)

  n <- ceiling(nwl(spc) / by)

  small <- nwl(spc) %% by
  if (small != 0) {
    s <- if (small == 1) "" else "s"
    warning("Last data point averages only ", small, " point", s, ".")
  }

  bin <- rep(seq_len(n), each = by, length.out = nwl(spc))

  na <- is.na(spc@data$spc)

  if ((na.rm > 0) && any(na)) {
    if (na.rm == 1) {
      na <- apply(!na, 1, tapply, bin, sum, na.rm = FALSE)
      spc@data$spc <- t(apply(spc@data$spc, 1, tapply, bin, sum, na.rm = TRUE) / na)

    } else {
      # faster for small numbers of NA
      tmp <- t(apply(spc@data$spc, 1, tapply, bin, sum, na.rm = FALSE))
      tmp <- sweep(tmp, 2, rle(bin)$lengths, "/")

      na <- which(is.na(tmp), arr.ind = TRUE)
      bin <- split(wl.seq(spc), bin)

      for (i in seq_len(nrow(na))) {
        tmp[na[i, 1], na[i, 2]] <-
          mean(spc@data$spc[na[i, 1], bin[[na[i, 2]]]], na.rm = TRUE)
      }
      spc@data$spc <- tmp
    }

  } else {
    # considerably faster
    spc@data$spc <- t(apply(spc@data$spc, 1, tapply, bin, sum, na.rm = FALSE))
    spc@data$spc <- sweep(spc@data$spc, 2, rle(bin)$lengths, "/")
  }

  .wl(spc) <- as.numeric(tapply(spc@wavelength, bin, mean, na.rm = na.rm > 0))
  spc <- .fix_spc_colnames(spc)

  validObject(spc)
  spc
}


# Unit tests -----------------------------------------------------------------
.test(spc.bin) <- function() {
  context("spc.bin")

  # Perform tests
  test_that("spc.bin() returnts output silently", {
    expect_silent(spc.bin(flu, 1))
  })

  test_that("spc.bin() returns errors", {
    expect_error(spc.bin(flu), "reduction factor needed")
  })

  test_that("spc.bin() returns warnings", {
    expect_warning(spc.bin(flu, 2), "Last data point averages only 1 points.")
    expect_warning(spc.bin(flu, 2, na.rm = TRUE), "Last data point averages only 1 points.")

    flu[[3, ]] <- NA_real_
    expect_warning(spc.bin(flu, 2, na.rm = TRUE), "Last data point averages only 1 points.")
  })

  # FIXME (tests): add tests to check the correctness of the output!!!
}
