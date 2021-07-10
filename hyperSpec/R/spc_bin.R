#' @name spc-bin
#' @title Wavelength binning
#'
#' @description
#' In order to reduce the spectral resolution and thus gain signal to noise
#' ratio or to reduce the dimensionality of the spectral data set, the
#' spectral resolution can be reduced.
#'
#' @details
#' The mean of every `by` data points in the spectra is calculated.
#'
#' Using `na.rm = TRUE` always takes about twice as long as `na.rm = FALSE`.
#'
#' If the spectra matrix does not contain too many `NA`s, `na.rm = 2` is
#' faster than `na.rm = TRUE`.
#'
#' @param spc The `hyperSpec` object.
#' @param by Reduction factor.
#' @param na.rm decides about the treatment of `NA`s:
#'
#' - if `FALSE` or `0`, the binning is done using `na.rm = FALSE`,
#' - if `TRUE` or `1`, the binning is done using `na.rm = TRUE`,
#' - if `2`, the binning is done using `na.rm = FALSE`, and resulting `NA`s are
#'   corrected with `mean(..., na.rm = TRUE)`. See section "Details".
#' @param ... Ignored.
#'
#' @return A [`hyperSpec`][hyperSpec::hyperSpec-class] object with
#'        `ceiling(nwl(spc)/by)` data points per spectrum.
#'
#' @export
#'
#' @keywords manip datagen
#' @concept spectra smoothing
#'
#' @author C. Beleites
#'
#' @examples
#' spc <- spc_bin(flu, 5)
#'
#' plot(flu[1, , 425:475])
#' plot(spc[1, , 425:475], add = TRUE, col = "blue")
#'
#' nwl(flu)
#' nwl(spc)
spc_bin <- function(spc, by = stop("reduction factor needed"), na.rm = TRUE, ...) {
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
  spc <- .spc_fix_colnames(spc)

  validObject(spc)
  spc
}


# Unit tests -----------------------------------------------------------------

hySpc.testthat::test(spc_bin) <- function() {
  context("spc_bin")

  sp <- generate_hy_spectra()

  # Perform tests
  test_that("spc_bin() returnts output silently", {
    expect_silent(spc_bin(sp, 1))
    expect_silent(spc_bin(sp, 10))
  })


  test_that("spc_bin() returns errors", {
    expect_error(spc_bin(sp), "reduction factor needed")
  })

  test_that("$spc after spc_bin() is correct", {
    bin_1_6 <- new("hyperSpec", matrix(rep(1:6, 5), nrow = 1))

    expect_silent(res1 <- spc_bin(bin_1_6, 5)[[]])
    expect_equal(ncol(res1), 6)
    expect_equal(as.vector(res1), c(3.0, 3.2, 3.4, 3.6, 3.8, 4.0))

    expect_silent(res2 <- spc_bin(bin_1_6, 6)[[]])
    expect_equal(ncol(res2), 5)
    expect_equal(as.vector(res2), c(3.5, 3.5, 3.5, 3.5, 3.5))
  })


  test_that("spc_bin() returns warnings", {
    expect_warning(spc_bin(sp, 7), "Last data point averages only 1 point.")
    expect_warning(spc_bin(sp, 3), "Last data point averages only 2 points.")
  })


  test_that("spc_bin() sets spc matrix column names correctly", {
    # Wavelengths should be identical
    sp_binned <- spc_bin(sp, 1)

    # Wavelengths should be identical
    expect_silent(wl_regular <- wl(sp))
    expect_silent(wl_binned <- wl(sp_binned))
    expect_equal(wl_regular, wl_binned)

    # Column names in wide-format dataset should be identical too (issue #237)
    expect_silent(names_regular <- colnames(as.wide.df(sp)))
    expect_silent(names_binned <- colnames(as.wide.df(sp_binned)))
    expect_equal(names_regular, names_binned)
  })


  test_that("na.rm in spc_bin() works", {
    sp_na <- generate_hy_spectra(n_wl = 9, n = 5)
    sp_na[[, , 3, wl.index = TRUE]] <- NA_real_
    expect_true(any(is.na(sp_na[[]])))

    # NA's are present
    na_rm_false <- spc_bin(sp_na, 3, na.rm = FALSE)[[]]
    expect_equal(ncol(na_rm_false), 3)
    expect_equal(nrow(na_rm_false), 5)
    expect_true(any(is.na(na_rm_false)))

    # All rows should contain NA's (in the first column)
    expect_equal(
      apply(na_rm_false, 1, function(x) any(is.na(x))),
      c(TRUE, TRUE, TRUE, TRUE, TRUE)
    )

    # Only the first column should contain NA's
    expect_equal(
      unname(apply(na_rm_false, 2, function(x) any(is.na(x)))),
      c(TRUE, FALSE, FALSE)
    )

    # NA's are removed (1st algorithm)
    expect_silent(na_rm_true1 <- spc_bin(sp_na, 3, na.rm = TRUE)[[]])
    expect_equal(ncol(na_rm_true1), 3)
    expect_equal(nrow(na_rm_true1), 5)
    expect_false(any(is.na(na_rm_true1)))

    # NA's are removed (2nd algorithm)

    # FIXME (tests): add appropriate example to work with na.rm = 2

    # expect_silent(na_rm_true2 <- spc_bin(sp_na, 3, na.rm = 2)[[]])
    # expect_equal(ncol(na_rm_true2), 3)
    # expect_equal(nrow(na_rm_true2), 5)
    # expect_false(any(is.na(na_rm_true2)))
  })
}
