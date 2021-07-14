### --------------------------------------------------------------------------~
###
### .extract - internal function doing the work for extracting with [] and [[]]
###
### --------------------------------------------------------------------------~

# Function -------------------------------------------------------------------

#' @include wl2i.R
#' @noRd
.extract <- function(x, i, j, l,
                     ...,
                     wl.index = FALSE) {
  if (!missing(i)) {
    x@data <- x@data[i, , drop = FALSE]
  }

  if (!missing(j)) {
    x@data <- x@data[, j, drop = FALSE]
    x@label <- x@label[c(".wavelength", colnames(x@data))]
  }

  if (!missing(l)) {
    if (is.null(x@data$spc)) {
      warning("Selected columns do not contain specta. l ignored.")
    } else {
      if (!wl.index) {
        l <- wl2i(x, l)
      }

      x@data$spc <- x@data$spc[, l, drop = FALSE]
      .wl(x) <- x@wavelength[l]
    }
  }

  x
}


.extract_h <- function(x, i, j, l, ...,
                        wl.index = FALSE,
                        drop = FALSE # drop has to be at end
) {
  validObject(x)

  if (drop) {
    warning("Ignoring drop = TRUE.")
  }

  dots <- list(...)
  if (length(dots) > 0L) {
    warning("Ignoring additional parameters: ", .pastenames(dots))
  }

  x <- .extract(x, i, j, l, wl.index = wl.index)

  if (is.null(x@data$spc)) {
    x@data$spc <- matrix(NA, nrow(x@data), 0)
    x@wavelength <- numeric(0)
  }

  x
}


#' These methods allow one to extract and replace parts of the `hyperSpec` object.
#' They can be used for selecting/deleting spectra, cutting the spectral range, and extracting
#' or setting the extra data belonging to the object. Convenient shortcuts are provided
#' for commonly used operations.
#'
#' While the parts of the `hyperSpec` object can be accessed directly, it is good practice to
#' use the functions provided by the package to handle the objects rather than accessing the
#' slots directly. This also ensures that proper (i.e. *valid*) objects are returned.
#' In some cases, however, direct access to the slots can considerably speed up calculations.
#'
#' The main functions to retrieve the data of a `hyperSpec` object are `[]` and `[[]]`.
#' The difference between these functions is that `[]` returns a `hyperSpec` object, whereas
#' `[[]]` returns a `data.frame` containing `x$spc`, the spectral data.
#' To modify a `hyperSpec` object, the corresponding functions are `[<-` and `[[<-`.
#' The first form is used to modify the entire `hyperSpec` object and the second form modifies
#' the spectral data in `x$spc`.
#'
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#     IF THE FOLLOWING MATERIAL IS EDITTED THE CORRESPONDING MATERIAL IN
#                 hyperSpec.Rmd SHOULD BE EDITTED AS WELL.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#' `hyperSpec` objects are triple indexed:
#'
#' * `x[i, j, l, wl.index = TRUE/FALSE]`
#' * `x[[i, j, l, wl.index = TRUE/FALSE]]`
#' * `i` refers to rows of the `@data` slot. `i` can be integer indices or a logical vector.
#' * `j` refers to columns of the `@data` slot. `j` can be integer indices, a logical vector
#'    or the name of a column.  _However, there is no guaranteed order to_ `colnames(x)`
#'    _so using integer indices and logical vectors is unwise._
#' * `l` refers to wavelengths. Note the argument `wl.index` which determines how `l` is interpreted.
#' * If there is only one index given, e.g. `x[1:3]`, it refers to the row index `i`.
#'   Likewise if there are only two indices given they refer to `i` and `j`.
#'
#' ## Extraction (Getter) Actions
#'
#' | **Getter** | **Action** |
#' | ------ | ------ |
#' | `x[]` | Returns the entire `hyperSpec` object unchanged. |
#' | `x[i, , ]` | Returns the `hyperSpec` object with selected rows; equivalent to `x[i]`. |
#' | `x[, j, ]` | Returns the `hyperSpec` object with empty `x$spc` slot. If you want the column `j`, `x[["name"]]` returns a `data.frame` containing `j` or `x$name` returns it as a vector. |
#' | `x[, , l, wl.index = TRUE/FALSE]` | Returns the `hyperSpec` object with selected wavelengths. |
#' | `x[[]]` | Returns the spectra matrix (`x$spc`). |
#' | `x[[i, , ]]` | Returns the spectra matrix (`x$spc`) with selected rows. |
#' | `x[[, j, ]]` | Returns a `data.frame` with the selected columns.  Safest to give `j` as a character string. |
#' | `x[[, , l, wl.index = TRUE/FALSE]]` | Returns the spectra matrix (`x$spc`) with selected wavelengths. |
#' | `x$name` | Returns the column `name` as a vector. |
#' | `x$.` | Returns the complete `data.frame` `x@data`, with the spectra in column `$spc`. |
#' | `x$..` | Returns all the extra data (`x@data` without `x$spc`). |
#' | [wl()] | Returns the wavelengths. |
#' | [labels()][hyperSpec::labels()] | Returns the labels. |
#'
#' One can see that there are several ways to get the spectral data:
#' `x$spc`, `x[[]]`, `x$..`.  The first two forms return a matrix,
#' while the last returns a `data.frame`.
#'
#' ## Replacement (Setter) Actions
#'
#' | **Setter** | **Action** |
#' | ------ | ------ |
#' | `x[i, ,] <-` | Replaces the specified rows of the `@data` slot, including `x$spc` and any extra data columns. Other approaches are probably easier. |
#' | `x[, j,] <-` | Replaces the specified columns.  Safest to give `j` as a character string. |
#' | `x[i, j] <-` | Replaces the specified column limited to the specified rows.  Safest to give `j` as a character string. |
#' `x[, , l, wl.index = TRUE/FALSE] <-` | Replaces the specified wavelengths. |
#' | `x[[i, ,]] <-` | Replaces the specified row of `x$spc` |
#' | `x[[, j,]] <-` | As `[[]]` refers to just the spectral data in `x$spc`, this operation is not valid.  See below. |
#' | `x[[, , l, wl.index = TRUE/FALSE]] <-` | Replaces the intensity values in `x$spc` for the specified wavelengths. |
#' | `x[[i, , l, wl.index = TRUE/FALSE]] <-` | Replaces the intensity values in `x$spc` for the specified wavelengths limited to the specified rows. |
#' | `x$.. <-` | Sets the extra data (`x@data` without touching `x$spc`). The column names must match exactly in this case. |
#' | `wl<-` | Sets the wavelength vector. |
#' | `labels<-` | Sets the labels. |
#'
#'
#' @title Extract and replace parts of `hyperSpec` objects
#' @rdname extractreplace
#' @docType methods
#' @aliases [ [,hyperSpec-method
#'
#' @param x A `hyperSpec` Object.
#' @param i Index of rows in `x@@data`. Integer, logical, or in the case of
#'          `[[` and `[[<-`, a `nrow` by 2 logical matrix or
#'          integer index matrix. In this case the indexing is done
#'          inside the spectra matrix. See the examples.
#' @param j Index of `x@@data` columns.  Integer, logical or character.  The order
#'          of columns in a `hyperSpec` object is not fixed and thus
#'          using integer or logical vectors may not be safe.
#' @param l Index of wavelengths in the spectra matrix. The behavior of `l` depends
#'          upon the value of `wl.index`.
#' @param wl.index If `FALSE` (the default), the values in `l` are
#'          taken to be wavelengths.  If `TRUE`, the values in `l` are
#'          taken to be indices for the spectral matrix.
#' @param drop For `[[`: drop unnecessary dimensions, see
#'          [base::drop()] and [base::Extract()]. Ignored (quietly) for
#'          `[`, as otherwise invalid `hyperSpec` objects might result.
#' @param ... Ignored.
#' @param name Name of the data column to extract. `$spc` yields the spectra matrix.
#'
#' @return
#'  * For `[`, `[<-`, `[[<-`, and `$<-` a `hyperSpec` object.
#'  * For `[[` a matrix or `data.frame`.
#'  * For `$` the given column of the `data.frame` `x@@data`.
#'  * `x[[]]` returns the complete spectra matrix.
#'  * `x$.` returns the complete slot `@@data`,
#'  * `x$..` returns the `data.frame` in `@@data` but without the column
#'               `@@data$spc` containing the spectra matrix.
#'
#' @seealso [wl2i()] on conversion of wavelength ranges to indices.
#'          [base::drop()] and [base::Extract()] for details on `drop`.
#'
#' @keywords methods manip
#' @examples
#'
#' ##### Access rows using "i"  (first index)
#'
#' # numeric index
#' plot(flu, "spc", lines.args = list(lty = 2))
#' plot(flu[1:3], "spc", add = TRUE, col = "red") # select spectra
#' plot(flu[-(1:3)], "spc", add = TRUE, col = "blue") # delete spectra
#'
#' # logical index
#' plot(flu, "spc", lines.args = list(lty = 2))
#' index <- rnorm(6) > 0
#' index
#' plot(flu[index], "spc", add = TRUE, col = "red") # select spectra
#' plot(flu[!index], "spc", add = TRUE, col = "blue") # select spectra
#'
#' ##### Access columns using "j" (2nd index)
#'
#' range(faux_cell[[, "x"]]) # safest to use the name of the column
#' colnames(faux_cell[[, 1]])
#' dim(faux_cell[[, c(TRUE, FALSE, FALSE)]])
#' head(faux_cell$x) # get "j" via name
#'
#' ##### Access wavelengths using "l" (3rd index)
#'
#' dim(flu[[]])
#' fluA <- flu[[, , 420 ~ 450]] # matches the wavelength values
#' dim(fluA)
#' fluB <- flu[[, , 31:91, wl.index = TRUE]]
#' identical(fluA, fluB)
#'
#' ##### Indexing via both j and l, result is data.frame
#'
#' result <- flu[[, 1:2, 405 ~ 410]]
#' result
#' class(result)
#' colnames(result)
#'
#' ##### Shortcuts
#'
#' # extract the spectra matrix
#' flu[[]][1:5, 1:5] # show only a bit here
#'
#' # extract the data.frame including the spectra matrix
#' flu$.
#' dim(flu$.)
#' colnames(flu$.) # = colnames(flu[])
#' flu$.$spc
#'
#' # extract the data.frame minus the spectra matrix
#' flu$..
#' colnames(flu$..)
#'
#' ##### Indexing via matrices
#'
#' # indexing the spectra matrix with index matrix nrow by 2
#' # hyperSpec converts numeric matrices to integers for indexing
#' ind <- matrix(c(1, 2, 4, 406, 405.5, 409), ncol = 2)
#' ind
#' flu[[ind]]
#'
#' # same as just above, but using wl.index
#' ind <- matrix(c(1, 2, 4, c(3, 2, 9)), ncol = 2)
#' ind
#' flu[[ind, wl.index = TRUE]]
#'
#' # indexing via a logical matrix (applied to spectra matrix)
#' summary(flu[[flu < 125]])
#' @export
#'
#' @concept manipulation
#'
setMethod("[", signature = signature(x = "hyperSpec"), .extract_h)


# Function -------------------------------------------------------------------

.extract2_h <- function(x, i, j, l, ..., wl.index = FALSE, drop = FALSE) {
  validObject(x)

  dots <- list(...)
  if (length(dots) > 0L) {
    warning("Ignoring additional parameters: ", .pastenames(dots))
  }

  ## check wheter a index matrix is used
  if (!missing(i) && is.matrix(i)) {
    if (!is.logical(i) && !(is.numeric(i) && ncol(i) == 2)) {
      stop(
        "Index matrix i  must either be logical of the size of x$spc,",
        "or a n by 2 matrix."
      )
    }

    if (is.numeric(i) && !wl.index) {
      i[, 2] <- .getindex(x, i[, 2], extrapolate = FALSE)
    }

    x@data$spc[i] # return value
  } else { # index by row and columns
    x <- .extract(x, i, j, l, wl.index = wl.index)
    if (missing(j)) {
      unclass(x@data$spc[, , drop = drop])
    } # retrun value; removes the "AsIs"
    else {
      x@data[, , drop = drop] # return value: data.frame
    }
  }
}

#' @rdname extractreplace
#' @export
#'
#' @concept manipulation
#'
#' @aliases [[ [[,hyperSpec-method
## ' @name [[
setMethod("[[", signature = signature(x = "hyperSpec"), .extract2_h)


# Function -------------------------------------------------------------------

.dollar_shortcuts <- function(x, name) {
  validObject(x)

  if (name == ".") { ## shortcut
    x@data[, , drop = FALSE]
  } else if (name == "..") {
    x@data[, -match("spc", colnames(x@data)), drop = FALSE]
  } else {
    x@data[[name]]
  }
}


#' @rdname extractreplace
#' @aliases $ $,hyperSpec-method
#' @export
#'
#' @concept manipulation
#'
setMethod("$", signature = signature(x = "hyperSpec"), .dollar_shortcuts)
