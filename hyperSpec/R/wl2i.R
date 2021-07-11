### -----------------------------------------------------------------------------
###
### .getindex
###
###
## does the actual work of looking up the index for wl2i, .extract and .replace
## extrapolate = TRUE returns first resp. last index for wavelength outside
## hyperSpec@wavelength.
## extrapolate = FALSE returns NA in this case

.getindex <- function(x, wavelength, extrapolate = TRUE) {
  if (!extrapolate) {
    wavelength[wavelength < min(x@wavelength)] <- -Inf
    wavelength[wavelength > max(x@wavelength)] <- +Inf
  }
  tmp <- wavelength[is.finite(wavelength)]
  if (length(tmp) > 0) {
    tmp <- sapply(
      tmp,
      function(x, y) which.min(abs(x - y)),
      x@wavelength
    )
    wavelength[is.finite(wavelength)] <- tmp
  }
  wavelength
}

#' Conversion between Wavelength and Spectra Matrix Column.
#'
#' Index `wl2i()` returns the column indices for the spectra matrix for the given wavelengths.
#' `i2wl()` converts column indices into wavelengths.
#'
#' If `wavelength` is numeric, each of its elements is converted to the respective index.
#' Values outside the range of `x@@wavelength` become `NA`.
#'
#' If the range is given as a formula (i.e. `start ~ end`, a sequence
#'
#' index corresponding to start : index corresponding to end
#'
#' is returned. If the wavelengths are not ordered, that may lead to chaos.
#' In this case, call [hyperSpec::wl_sort()] first.
#'
#' Two special variables can be used: `min` and `max`, corresponding to the
#' lowest and highest wavelength of `x`, respectively.
#'
#' start and end may be complex numbers. The resulting index for a complex x is then
#'
#' index (Re (x)) + Im (x)
#'
#' @aliases wl2i
#' @param x a `hyperSpec` object
#' @param wavelength the wavelengths to be converted into column indices,
#'   either numeric or a formula, see details.
#' @param i the column indices into the spectra matrix for which the
#'   wavelength is to be computed
#' @param unlist if multiple wavelength ranges are given, should the indices be unlisted or kept in a list?
#' @return A numeric containing the resulting indices for `wl2i`
#' @author C. Beleites
#'
#' @export
#'
#' @concept wavelengths
#'
#' @examples
#'
#' flu
#' wl2i(flu, 405:407)
#' wl2i(flu, 405 ~ 407)
#'
#' ## beginning of the spectrum to 407 nm
#' wl2i(flu, min ~ 407)
#'
#' ## 2 data points from the beginning of the spectrum to 407 nm
#' wl2i(flu, min + 2i ~ 407)
#'
#' ## the first 3 data points
#' wl2i(flu, min ~ min + 2i)
#'
#' ## from 490 nm to end of the spectrum
#' wl2i(flu, 490 ~ max)
#'
#' ## the last 8 data points
#' wl2i(flu, max - 7i ~ max)
#'
#' ## get 450 nm +- 3 data points
#' wl2i(flu, 450 - 3i ~ 450 + 3i)
#'
#' wl2i(flu, 300:400) ## all NA:
#' wl2i(flu, 600 ~ 700) ## NULL: completely outside flu's wavelength range
#' @importFrom lazyeval lazy lazy_eval is_formula f_eval_lhs f_eval_rhs
wl2i <- function(x, wavelength = stop("wavelengths are required."), unlist = TRUE) {
  chk.hy(x)
  validObject(x)

  ## wavelength may have been forced already before.
  ## in that case, no special evaluation can be done.
  ## However, we cannot know whether we have the experession forced already or not,
  ## so we have to try
  try(
    {
      wavelength <- lazy(wavelength)

      wavelength <- lazy_eval(wavelength,
        data = list(
          max = max(x@wavelength), min = min(x@wavelength),
          maxwl = max(x@wavelength), minwl = min(x@wavelength)
        )
      )
    },
    silent = TRUE
  )

  ## make sure we have a list of ranges to be converted
  if (!is.list(wavelength)) {
    wavelength <- list(wavelength)
  }

  results <- list()

  for (r in seq_along(wavelength)) {

    ## ~ sequence vs. scalars and : sequences
    if (is_formula(wavelength[[r]])) {
      from <- f_eval_lhs(wavelength[[r]])
      to <- f_eval_rhs(wavelength[[r]])
    } else {
      ## sequence with : or scalar
      from <- NULL
      to <- wavelength[[r]]
    }

    ## conversion to indices
    if (is.logical(to)) {
      to <- seq_len(nwl(x))[to]
    } else {
      to <- .getindex(x, Re(to), extrapolate = FALSE) + Im(to)
    }

    if (is.null(from)) {
      results[[r]] <- to
      results[[r]][!is.finite(results[[r]])] <- NA
    } else {
      from <- .getindex(x, Re(from), extrapolate = FALSE) + Im(from)

      ## completely outside range
      results[[r]] <- NULL

      ## start outside left
      if (from == -Inf) from <- 1

      ## end outside right
      if (to == Inf) to <- nwl(x)

      if (is.finite(from) && is.finite(to)) {
        ## crop indices to range:
        ## outside range indices can happen with complex wavelength specifications like min - 1i
        if (from < 1L) from <- 1L
        if (to > nwl(x)) to <- nwl(x)

        results[[r]] <- seq(from, to)
      }
    }
  }

  if (unlist) {
    unlist(results)
  } else {
    results
  }
}

hySpc.testthat::test(wl2i) <- function() {
  context("wl2i")
  test_that(": sequence of wavelengths", {
    skip("skip")
    expect_equal(wl2i(flu, 405:407), c(1, 3, 5))
  })

  test_that("~ sequence of indices", {
    expect_equal(wl2i(flu, 405 ~ 407), 1:5)
  })

  test_that("min special variables", {
    expect_equal(wl2i(flu, min ~ 407), 1:5)
    expect_equal(wl2i(flu, min + 2i ~ 407), 3:5)
    expect_equal(wl2i(flu, min ~ min + 2i), 1:3)
  })

  test_that("max special variables", {
    expect_equal(wl2i(flu, 493 ~ max), 177:181)
    expect_equal(wl2i(flu, max - 7i ~ max - 2i), 174:179)
  })

  test_that("complex numbers for indices", {
    expect_equal(wl2i(flu, 450 - 3i ~ 450 + 3i), 88:94)

    ## hitting range
    expect_equal(wl2i(flu, min - 1i ~ max + 1i), seq_len(nwl(flu)))
  })

  test_that("logical indices", {
    expect_equal(wl2i(flu, c(TRUE, TRUE, TRUE, rep(FALSE, nwl(flu) - 3))), 1:3)
  })


  test_that("behavior outside spectral range", {
    ## completely outside range
    tmp <- wl2i(flu, 300:400)
    expect_true(all(is.na(tmp)))
    expect_equal(length(tmp), 101)

    expect_true(is.null(wl2i(flu, 600 ~ 700)))

    ## one side outside
    expect_equal(wl2i(flu, 400 ~ 407), 1:5)
    expect_equal(wl2i(flu, 490 ~ 500), 171:181)

    ## enclosing range
    expect_equal(wl2i(flu, 400 ~ 500), seq_len(nwl(flu)))
  })

  test_that("list of ranges", {
    expect_equal(
      wl2i(flu, c(300:400, 405 ~ 407, min ~ min + 2i)),
      c(wl2i(flu, 300:400), wl2i(flu, 405 ~ 407), wl2i(flu, min ~ min + 2i))
    )

    expect_equal(wl2i(flu, c(min ~ min + 5i, 405 ~ 407), unlist = TRUE), c(1:6, 1:5))
    expect_equal(wl2i(flu, c(min ~ min + 5i, 405 ~ 407), unlist = FALSE), list(1:6, 1:5))
  })

  test_that("inside extraction", {})
}

#' @rdname wl2i
#' @aliases i2wl
#' @return `i2wl` returns a numeric with the wavelengths
#' @export
#' @examples
#'
#' i2wl(faux_cell, 17:20)
i2wl <- function(x, i) {
  chk.hy(x)
  validObject(x)

  x@wavelength[i]
}

## check for wrong complex invocation
## grepl("[(][[:digit:].]+[+-][[:digit:].]+i[)]", deparse (substitute (1i %~% max - 3i | 2800 %~% 3000, list (max = 3000))))
