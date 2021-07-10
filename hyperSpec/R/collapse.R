#' @title Collapse/bind several `hyperSpec` objects into one object
#' @description
#' The spectra from all objects will be put into one object.
#' The resulting object has all wavelengths that occur in any of the input objects,
#' `wl.tolerance` is used to determine which difference in the wavelengths is
#' tolerated as equal: clusters of approximately equal wavelengths will span at most `2 * wl.tolerance`.
#' Differences up to +/- `wl.tolerance` are considered equal.
#'
#' The returned object has wavelengths that are the weighted average
#' (by number of spectra) of the wavelengths within any such cluster of approximately
#' equal wavelengths.
#'
#' Labels will be taken from the first object where they are encountered. However,
#' the order of processing objects is not necessarily the same as the order of objects
#' in the input: `collapse` first processes groups of input objects that share all
#' wavelengths (within `wl.tolerance`).
#'
#' Data points corresponding to wavelengths not in the original spectrum will be set to NA.
#' Extra data is combined in the same manner.
#'
#' If the objects are named, the names will be preserved in extra data column `$.name`.
#' If the wavelengths are names, names are preserved and taken from the first object where they were encountered,
#' the same applies to possible column names of the spectra matrix.
#'
#' @author C. Beleites
#'
#' @param ... hyperSpec objects to be collapsed into one object. Instead of giving several
#' arguments, a list with all objects to be collapsed may be given.
#' @param wl.tolerance tolerance to decide which wavelengths are considered equal.
#' @param collapse.equal logical indicating whether to try first finding groups of spectra
#' with (approximately) equal wavelength axes. If the data is known to contain few or no
#' such groups, `collapse()` will be faster with this first pass being turned off.
#' @aliases collapse collapse.hyperSpec
#' @seealso [merge()],  [rbind()], and [plyr::rbind.fill()]
#' @return a hyperSpec object
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#'
#' @export
#'
#' @keywords manip
#' @concept manipulation
#'
#' @examples
#' barbiturates[1:3]
#' collapse(barbiturates[1:3])
#'
#' a <- barbiturates[[1]]
#' b <- barbiturates[[2]]
#' c <- barbiturates[[3]]
#'
#' a
#' b
#' c
#' collapse(a, b, c)
#'
#' collapse(barbiturates[1:3], collapse.equal = FALSE)
collapse <- function(..., wl.tolerance = hy.getOption("wl.tolerance"), collapse.equal = TRUE) {
  wl.tolerance <- .checkpos(wl.tolerance, "wl.tolerance")
  dots <- list(...)

  ## accept also a list of hyperSpec objects
  if (length(dots) == 1 && is.list(dots[[1]])) {
    dots <- dots[[1]]
  }

  ## check the arguments
  lapply(dots, chk.hy)
  lapply(dots, validObject)

  dots <- lapply(dots, wl_sort)

  ## check: wl.tolerance should be smaller than *half* of the smallest wavelength difference within each object
  ## half because we later check for distance <= wl.tolerance, so ± => window size is 2 wl.tolerance
  .assert.suitable.tolerance(dots, wl.tolerance)

  ## make sure there aren't any NAs in wavelength
  dots <- .assert.noNA.wl(dots)

  ## names cause problems with unlisting labels.
  ## preserve them in column .name
  if (!is.null(names(dots))) {
    dots <- mapply(function(object, name) {
      object$.name <- name
      object
    }, dots, names(dots))
    names(dots) <- NULL
  }

  ## shall we do a first round collapsing objects that have their whole wavelength axes approximately equal?
  if (collapse.equal) {
    dots <- .collapse.equal(dots, wl.tolerance)

    if (length(dots) == 1L) {
      return(dots[[1]])
    }
  }

  ## Now cluster approximately equal wavelengths together

  ## prepare new labels
  labels <- unlist(lapply(dots, slot, "label"))
  labels <- labels[unique(names(labels))]
  labels <- lapply(labels, function(l) if (is.language(l)) l <- as.expression(l) else l)

  ## cluster wavelengths into groups of ± wl.tolerance from center
  wl.df <- .cluster.wavelengths(dots, wl.tolerance)

  ## assign cluster number to columns
  # wl.df is ordered by wavelength, each object in dots is ordered by wavelength, so
  for (i in seq_along(dots)) {
    colnames(dots[[i]]@data$spc) <- wl.df$wlcluster[wl.df$iobj == i]
  }

  ## now we're ready for the  actual work of collapsing the objects
  dots <- rbind.fill(lapply(dots, slot, "data"))

  ## careful with constructing the wavelength vector: the columns in $spc are in no particular order,
  ## but the colnames indicate wavelength rank.
  ## so reorder $spc accor
  dots$spc <- dots$spc[, order(as.numeric(colnames(dots$spc)))]

  ## we now need summarized wl.df data:
  wl.df <- group_by(wl.df, .data$wlcluster)
  wl.df <- summarise(wl.df,
    wl = sum(.data$wl * .data$nspc) / sum(.data$nspc), # weighted average
    old.wlnames = .data$old.wlnames[1L]
  )

  ## prepare wavelength vector & restore old names (as far as possible)
  wl <- wl.df$wl
  if (any(!is.na(wl.df$old.wlnames))) {
    names(wl) <- wl.df$old.wlnames
  }

  ## make a new hyperSpec object
  new("hyperSpec", wavelength = wl, data = dots, labels = labels)
}


hySpc.testthat::test(collapse) <- function() {
  context("collapse")

  test_that("correctly assembled", {
    new <- do.call(collapse, barbiturates[1:3])
    wl <- unlist(lapply(barbiturates[1:3], slot, "wavelength"))
    expect_equal(
      wl(new),
      sort(wl[!duplicated(wl)])
    )

    for (s in 1:3) {
      expect_equal(as.numeric(new[[s, , wl(barbiturates[[s]])]]),
        as.numeric(barbiturates[[s]][[]]),
        label = paste0("barbiturates[[", s, "]]")
      )
    }
  })

  tmp <- collapse(a = flu, b = flu)

  test_that("wavelength label is not lost", {
    tmp <- collapse(flu, flu)
    expect_equal(labels(tmp, ".wavelength"), labels(flu, ".wavelength"))

    tmp <- collapse(flu[, , min ~ 410], flu[, , 414 ~ 420])
    expect_equal(labels(tmp, ".wavelength"), labels(flu, ".wavelength"))
  })

  test_that("labels that are expressions stay expressions", {
    tmp <- collapse(flu, flu)
    expect_true(is.expression(labels(tmp)$.wavelength))
    expect_true(is.expression(labels(tmp)$spc))

    tmp <- collapse(flu[, , min ~ 405], flu[, , 414 ~ 420])
    expect_true(is.expression(labels(tmp)$.wavelength))
    expect_true(is.expression(labels(tmp)$spc))
  })

  test_that("collapse does not mess up labels if a named list is collapsed", {
    expect_equal(
      labels(tmp)[names(labels(flu))],
      labels(flu)
    )
  })

  test_that("named lists should return .name column", {
    expect_equal(tmp$.name, rep(c("a", "b"), each = nrow(flu)))
  })

  test_that("no difference whether list or single arguments are given", {
    tmp2 <- list(a = flu, b = flu)
    tmp2 <- collapse(a = flu, b = flu)
    expect_equal(tmp, tmp2,
      check.attributes = TRUE, check.names = TRUE, check.column.order = FALSE, check.label = TRUE
    )
  })

  test_that("wl.tolerance", {
    tmp <- flu
    wl(tmp) <- wl(tmp) + 0.01
    expect_equal(nwl(collapse(tmp, flu)), 2 * nwl(flu))
    expect_equal(nwl(collapse(tmp, flu, wl.tolerance = 0.1)), nwl(flu))
  })

  test_that("check warning occurs for too large tolerance", {
    expect_warning(collapse(flu, wl.tolerance = 0.25 + .Machine$double.eps))
  })

  test_that("bugfix: wl.tolerance generated warning for negative diff (wl (spc))", {
    tmp <- flu
    wl(tmp) <- rev(wl(tmp))
    expect_silent(collapse(tmp, tmp))
  })

  test_that("result has orded wavelengths", {
    tmp <- collapse(barbiturates[1:3])

    expect_true(all(diff(wl(tmp)) >= 0))
  })

  test_that("collapsing objects with equal wavelength axes", {
    expect_equivalent(collapse(barbiturates[[1]], barbiturates[[1]]),
      barbiturates[[1]][c(1, 1)],
      check.label = TRUE
    )
  })

  test_that("new wavelengths are weighted mean of wavelength bin: shortcut for equal wavelength axes", {
    tmp <- flu
    wl(tmp) <- wl(flu) + 0.03
    tmp <- collapse(flu, flu, tmp, wl.tolerance = 0.05)
    expect_equal(wl(tmp), wl(flu) + 0.01)

    tmp <- flu
    wl(tmp) <- wl(flu) + 0.03
    tmp <- collapse(flu[rep(1, 12)], tmp, wl.tolerance = 0.05)
    expect_equal(wl(tmp), wl(flu) + 0.01)
  })

  test_that("names of wavelength kept", {
    a <- barbiturates[[1]]
    b <- barbiturates[[2]]

    names(wl(a)) <- paste("Mass A", seq_len(nwl(a)))
    names(wl(b)) <- paste("Mass B", seq_len(nwl(b)))

    tmp <- collapse(a, b)

    expect_true(all(names(wl(a)) %in% names(wl(tmp))))
    expect_true(
      all(
        grep("B", names(wl(tmp)), value = TRUE) %in% names(wl(b))
      )
    )
    expect_true(all(grepl("Mass [AB]", names(wl(tmp)))))
  })


  test_that("factor behaviour of collapse", {
    a <- faux_cell[faux_cell$region == "nucleus"]
    a$region <- droplevels(a$region)
    b <- faux_cell[faux_cell$region != "nucleus"]
    b$region <- droplevels(b$region)

    tmp <- collapse(a, b)
    tmp$region

    expect_equal(
      sort(levels(tmp$region)),
      sort(levels(faux_cell$region))
    )
  })

  test_that("hyperSpec objects with 1 wavelength", {
    expect_equivalent(collapse(flu[, , 450], flu[, , 450]),
      flu[rep(1:nrow(flu), 2), , 450],
      check.labels = TRUE
    )

    tmp <- flu[rep(1:nrow(flu), 2)]
    tmp[[7:12]] <- NA
    tmp[[7:12, , 450]] <- flu[[, , 450]]
    expect_equivalent(collapse(flu[, , 450], flu),
      tmp,
      check.labels = TRUE
    )
  })

  test_that("hyperSpec objects with 0 wavelengths", {
    expect_equivalent(collapse(flu[, , FALSE], flu[, , FALSE]),
      flu[rep(1:nrow(flu), 2), , FALSE],
      check.labels = TRUE
    )

    tmp <- collapse(flu[, , FALSE], flu[, "spc", 405 ~ 406])
    expect_equal(tmp$c, c(flu$c, rep(NA, nrow(flu))))
    expect_equal(tmp$spc, rbind(flu[[, , 405 ~ 406]] + NA, flu[[, , 405 ~ 406]]))
    expect_equal(labels(tmp), lapply(labels(flu), as.expression))
  })

  test_that("hyperSpec objects with wavelength being/containing NA", {
    expect_warning(collapse(flu[, , 0]))


    expect_equal(
      suppressWarnings(collapse(flu[, , 0], flu)),
      collapse(flu[, , FALSE], flu)
    )

    expect_equal(
      suppressWarnings(collapse(flu[, , c(0, 405)], flu)),
      collapse(flu[, , 405], flu)
    )
  })
}

## warn if wl.tolerance is too large, i.e. it would lead to cluster multiple wavelengths of the same object together.
.assert.suitable.tolerance <- function(dots, wl.tolerance) {
  wl.diff <- sapply(dots, function(x) if (nwl(x) < 2L) NA else min(diff(wl(x)))) # wavelengths are ordered => no abs needed

  i.warn <- wl.diff < 2 * wl.tolerance

  if (any(isTRUE(i.warn))) {
    warning(sprintf(
      "object %i: wl.tolerance (%g) too large compared to smallest wavelength difference within object (%f). Columns will be lost.",
      which(i.warn), wl.tolerance, wl.diff[i.warn]
    ))
  }
}

.assert.noNA.wl <- function(dots) {
  i.NA <- sapply(dots, function(x) any(is.na(wl(x))))

  if (any(i.NA)) {
    warning(sprintf(
      "object %i: wavelength vector contains NAs: these columns will be dropped",
      which(i.NA)
    ))
    dots[i.NA] <- lapply(dots[i.NA], function(x) x[, , !is.na(wl(x))])
  }

  dots
}



#' Try finding groups of hyperSpec objects with (approximately) equal wavelength axes
#'
#' ... and directly rbind.fill them.
#'
#' @param dots list with hyperSpec object to collapse
#' @param wl.tolerance wavelength difference tolerance
#'
#' @return possible shorter list of dots
#' @noRd
.collapse.equal <- function(dots, wl.tolerance) {
  ## bind groups of objects that have *all* wavelengths equal
  ## within wl.tolerance from 1st object of potential group

  i <- 1

  while (i < length(dots)) {
    bind_directly <- sapply(tail(dots, -i), function(x) {
      (nwl(x) == nwl(dots[[i]])) && all(abs(wl(x) - wl(dots[[i]])) < wl.tolerance)
    })
    bind_directly <- which(bind_directly)

    if (length(bind_directly) > 0L) {
      n <- 0
      wl <- rep(0, nwl(dots[[i]]))

      for (j in c(i, i + bind_directly)) {
        wl <- wl + nrow(dots[[j]]) * wl(dots[[j]])
        n <- n + nrow(dots[[j]])

        # also ensure same column names within spc.
        colnames(dots[[j]]@data$spc) <- colnames(dots[[i]]@data$spc)
      }
      wl <- wl / n

      dots[[i]]@data <- rbind.fill(lapply(dots[c(i, i + bind_directly)], slot, "data"))
      .wl(dots[[i]]) <- structure(wl, names = names(wl(dots[[i]])))

      labels <- unlist(lapply(dots[c(i, i + bind_directly)], labels))
      labels <- lapply(labels, function(l) if (is.language(l)) l <- as.expression(l) else l)

      labels(dots[[i]]) <- labels[!duplicated(names(labels))]

      dots <- dots[-(i + bind_directly)]
    }

    i <- i + 1
  }

  dots
}


#' Find clusters of approximately equal wavelengths
#'
#' @param dots list of hyperSpec objects to collapse
#' @param wl.tolerance wavelength difference tolerance
#'
#' @concept wavelengths
#'
#' @return data.frame with information about suitable wavelength bins
#' @noRd
.cluster.wavelengths <- function(dots, wl.tolerance) {

  # set up data.frame to hold relevant information
  wl.df <- lapply(seq_along(dots), function(i) {
    if (nwl(dots[[i]]) == 0L) {
      return(data.frame())
    }

    data.frame(
      wl = wl(dots[[i]]),
      iobj = i,
      nspc = nrow(dots[[i]]),
      wlcol = seq_len(nwl(dots[[i]])),
      wlcluster = NA
    )
  })
  wl.df <- do.call("rbind", wl.df)

  ## save old wavelength names
  wl.df$old.wlnames <- NA

  for (i in seq_along(dots)) {
    wln <- names(dots[[i]]@wavelength)
    if (!is.null(wln)) wl.df$old.wlnames[wl.df$iobj == i] <- wln
  }

  wl.df <- wl.df[order(wl.df$wl), ]

  ## computational shortcut:
  ## wavelengths that are > 2 * wl.tolerance apart must be in different clusters,
  ## so cluster analysis can be split there

  wl.diff <- diff(wl.df$wl) > 2 * wl.tolerance

  ## assign preliminary clusters
  wl.df$wlcluster <- c(1, 1 + cumsum(wl.diff))

  maxcluster <- tail(wl.df$wlcluster, 1)

  ## preliminary clusters may need to be split further
  for (i in seq_len(tail(wl.df$wlcluster, 1))) {
    tmp <- wl.df[wl.df$wlcluster == i, ]

    ## only 1 wavelength in cluster => nothing to do
    if (length(tmp) <= 1L) {
      next
    }

    ## all wavelengths within 2 * wl.tolerance => nothing to do
    if (tail(tmp$wl, 1) - tmp$wl[1] <= 2 * wl.tolerance) {
      next
    }

    ## clustering needs to be done on actually unique wavelengths only
    unique.wl <- unique(tmp$wl)

    # make clusters that span at most 2 * wl.tolerance
    dist <- dist(unique.wl)
    dend <- hclust(dist, method = "complete")

    u <- data.frame(
      wl = unique.wl,
      wlcluster = cutree(dend, h = 2 * wl.tolerance) + maxcluster
    )
    maxcluster <- tail(u$wlcluster, 1)

    ## "expand" unique wavelengths => wavelengths per object
    tmp <- merge(tmp, u, by = "wl", suffixes = c(".prelim", ""))
    tmp$wlcluster.prelim <- NULL

    wl.df$wlcluster[wl.df$wlcluster == i] <- tmp$wlcluster
  }

  ## cluster numbers so far are in no particular order => rename them so they correspond to increasing wavelengths
  ## this saves one call to wl_sort () later on.
  wl.df$wlcluster <- as.numeric(factor(wl.df$wlcluster, levels = unique(wl.df$wlcluster)))

  wl.df
}

hySpc.testthat::test(.cluster.wavelengths) <- function() {
  context(".cluster.wavelengths")

  test_that("clustering with last window being long", {
    a <- as.hyperSpec(matrix(1:6, ncol = 3), wl = c(0, 2, 4))
    b <- as.hyperSpec(matrix(1:6, ncol = 3), wl = c(0, 2, 5))

    expect_equal(wl(collapse(a, b, wl.tolerance = 0.25)), c(0, 2, 4, 5))
    expect_equal(wl(collapse(a, b, wl.tolerance = 0.5)), c(0, 2, 4.5))
  })

  test_that("new wavelengths are weighted mean of wavelength bin", {
    a <- barbiturates[[1]][, , min ~ 30]
    b <- barbiturates[[2]][, , min ~ 30]
    wl(b) <- wl(b) + 0.03 / 2

    expect_equal(
      wl(collapse(a, b, a, wl.tolerance = 0.03)),
      sort(c(
        27.0499992370605, 28.1499996185303, 30.0499992370605,
        27.1649996185303, 28.0649992370605, 30.1649996185303,
        mean(c(29.0499992370605, 29.0499992370605, 29.0649992370605))
      ))
    )
  })
}
