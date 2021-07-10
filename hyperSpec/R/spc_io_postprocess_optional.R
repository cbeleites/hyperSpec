#' Helper Function to Harmonize Treatment of File Import Results
#'
#' This function provides two ways of post-processing imported spectra:
#'
#' - optionally remove empty spectra (some spectrograph software will produce
#'   empty spectra when measurements are cancelled)
#' - optionally keep the filenames in column `spc$filename`
#'
#' The desired overall behavior can be set by options via [hy.setOptions()]. All
#' file import filters should call `.spc_io_postprocess_optional()` to ensure the same
#' behavior.
#'
#' @param spc hyperSpec object for file import post-processing
#' @param filename filename(s) to become extra data column of `spc`
#' @param ... (ignored)
#' @param file.remove.emptyspc should empty (all `NA` or all `0`) spectra be
#'        removed?
#' @param file.keep.name should file names be kept and put into `spc$filename`?
#' @param tolerance intensities in +/- `tolerance` are considered `0` for
#'        `file.remove.emptyspc = TRUE`
#'
#' @return hyperSpec object
#'
#' @keywords internal
#'
#' @concept io
#' @concept manipulation
#'
#' @export
#'
.spc_io_postprocess_optional <- function(spc, filename, ...,
                             file.remove.emptyspc = hy.getOption("file.remove.emptyspc"),
                             file.keep.name = hy.getOption("file.keep.name"),
                             tolerance = hy.getOption("tolerance")) {
  tolerance <- .checkpos(tolerance, "tolerance")

  if (file.remove.emptyspc) {
    ## number of NAs in each spectrum
    nas <- rowSums(is.na(spc))

    ## number of zero-values in each spectrum
    zeros <- rowSums(abs(spc) < tolerance, na.rm = TRUE)

    spc <- spc[nas + zeros < nwl(spc)]
  }

  if (file.keep.name & nrow(spc) > 0L) {
    if (is.null(spc@data$filename)) {
      if (is(filename, "connection")) {
        filename <- summary(filename)$description
      }

      spc@data$filename <- filename
      spc@label$filename <- "filename"
    } else {
      warning("$filename already exists. => Skipping file.keep.name")
    }
  }

  spc
}


hySpc.testthat::test(.spc_io_postprocess_optional) <- function() {
  context(".spc_io_postprocess_optional")

  test_that("removing of zero/NA spectra", {
    tmp <- fluNA # spectrum 2 is all NA
    tmp[[3]] <- 0
    tmp[[5]] <- runif(nwl(tmp), min = -hy.getOption("tolerance") / 2, max = hy.getOption("tolerance") / 2)
    tmp[[, , 450 ~ 455]] <- NA

    expect_equivalent(
      .spc_io_postprocess_optional(tmp, file.remove.emptyspc = TRUE, file.keep.name = FALSE),
      tmp [-c(2, 3, 5)]
    )
  })

  test_that("filenames", {
    flu$filename <- NULL
    tmp <- .spc_io_postprocess_optional(flu, filename = "test", file.remove.emptyspc = FALSE, file.keep.name = TRUE)
    expect_true(all(tmp$filename == "test"))

    expect_warning(tmp <- .spc_io_postprocess_optional(tmp,
      filename = "test2",
      file.remove.emptyspc = FALSE, file.keep.name = TRUE
    ))
    expect_true(all(tmp$filename == "test"))
  })


  options.state <- .options
  on.exit(do.call(hy.setOptions, options.state))

  test_that("option treatment", {
    hy.setOptions(file.remove.emptyspc = FALSE)
    skip("TODO: implement")
    do.call(hy.setOptions, options.state)
  })
}
