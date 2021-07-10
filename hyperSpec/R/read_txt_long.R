### ---------------------------------------------------------------------------
###
###  read_txt_long: import measurements from .txt file
###
###  Format:
###  (y x) wl int
###

#' Import and Export of `hyperSpec` objects
#'
#' Besides [base::save()] and [base::load()], two general ways to import and
#' export data into `hyperSpec` objects exist.
#'
#' Firstly, `hyperSpec` objects can be imported and exported as ASCII files.
#'
#' A second option is using the package [R.matlab::R.matlab()], which
#' provides the functions [R.matlab::readMat()] and [R.matlab::writeMat()].
#'
#' Package \pkg{hyperSpec} comes with a number of pre-defined functions to
#' import manufacturer specific file formats. For details, see
#' `vignette("file-io")`.
#'
#' [hyperSpec::read.spc()] imports Thermo Galactic's `.spc` file format,
#' and ENVI files may be read using [hyperSpec::read.ENVI()].
#'
#' These functions are very flexible and provide lots of arguments.
#'
#' If you use them to read or write manufacturer specific ASCII formats,
#' please consider writing a wrapper function and contributing this function
#' to \pkg{hyperSpec}. An example is in the \dQuote{flu} vignette (see
#' `vignette("flu", package = "hyperSpec"`).
#'
#' Note that R accepts many packed formats for ASCII files, see
#' [base::connections()]. For `.zip` files, see [utils::unzip()].
#'
#' For further information, see the examples below and the documentation of
#' [R.matlab::R.matlab()].
#'
#' @name read_txt
#' @rdname read_txt
#'
#' @aliases read_txt_long import export
#' @param file filename or connection
#' @param cols the column names specifying the column order.
#' @param sep,row.names further parameters are handed over to [read_txt_long()]
#'
#' For data import, a list with elements `colname = label`; for export a
#'   character vector with the colnames.  Use `wavelength` to specify the
#'   wavelengths.
#' @param header the file has (shall have) a header line
#' @param ... arguments handed to [utils::read.table()] and
#'   [utils::write.table()], respectively.
#' param decreasing logical vector giving the sort order
#' @author C. Beleites
#' @seealso
#'
#' - [utils::read.table()] and [utils::write.table()],
#' - [R.matlab::R.matlab()] for `.mat` files,
#' - [hyperSpec::read.ENVI()] for ENVI data,
#' - [hyperSpec::read.spc()] for `.spc` files,
# - Manufacturer specific file formats: [read_txt_Renishaw()].
#'
#' @keywords IO file
#' @concept io
#'
#' @export
#' @importFrom utils read.table unstack
#' @examples
#' \dontrun{
#' vignette("file-io")
#' }
#'
#' ## Export & import matlab files
#' if (require(R.matlab)) {
#'   # export to matlab file
#'   writeMat(paste0(tempdir(), "/test.mat"),
#'     x = flu[[]], wavelength = flu@wavelength,
#'     label = lapply(flu@label, as.character)
#'   )
#'
#'   # reading a matlab file
#'   data <- readMat(paste0(tempdir(), "/test.mat"))
#'   print(data)
#'   mat <- new("hyperSpec",
#'     spc = data$x,
#'     wavelength = as.numeric(data$wavelength),
#'     label = data$label[, , 1]
#'   )
#' }
#'
#' ## ASCII export & import
#'
#'
#' write_txt_long(flu,
#'   file = paste0(tempdir(), "/flu.txt"),
#'   cols = c(".wavelength", "spc", "c"),
#'   order = c("c", ".wavelength"),
#'   decreasing = c(FALSE, TRUE)
#' )
#'
#' read_txt_long(
#'   file = paste0(tempdir(), "/flu.txt"),
#'   cols = list(
#'     .wavelength = expression(lambda / nm),
#'     spc = "I / a.u", c = expression("/"(c, (mg / l)))
#'   )
#' )
#'
#' write_txt_wide(flu,
#'   file = paste0(tempdir(), "/flu.txt"),
#'   cols = c("c", "spc"),
#'   col.labels = TRUE, header.lines = 2, row.names = TRUE
#' )
#'
#' write_txt_wide(flu,
#'   file = paste0(tempdir(), "/flu.txt"),
#'   col.labels = FALSE, row.names = FALSE
#' )
#'
#' read_txt_wide(
#'   file = paste0(tempdir(), "/flu.txt"),
#'   # give columns in same order as they are in the file
#'   cols = list(
#'     spc = "I / a.u",
#'     c = expression("/"("c", "mg/l")),
#'     filename = "filename",
#'     # plus wavelength label last
#'     .wavelength = "lambda / nm"
#'   ),
#'   header = TRUE
#' )
read_txt_long <- function(file = stop("file is required"),
                          cols = list(
                            .wavelength = expression(lambda / nm),
                            spc = "I / a.u."
                          ),
                          header = TRUE,
                          ...) {
  txtfile <- read.table(file = file, header = header, ...)

  if (header) {
    cln <- match(colnames(txtfile), names(cols))
    cln <- cols[cln]
    names(cln) <- colnames(txtfile)
    cols <- cln
    rm(cln)
  } else {
    if (ncol(txtfile) != length(cols)) {
      warning(paste(
        "cols does not correspond to the columns in", file,
        ". Guessing remaining columns."
      ))
      cols <- c(character(ncol(txtfile) - 2), cols)
    }
  }


  if (is.na(match("spc", names(cols)))) {
    stop("cols$spc must exist.")
  }

  wavelength <- match(".wavelength", names(cols))
  if (is.na(wavelength)) {
    stop("cols$.wavelength must exist.")
  }

  colnames(txtfile) <- names(cols)

  ## wavelength axis
  wavelength <- as.numeric(levels(as.factor(txtfile$.wavelength)))

  spc <- as.matrix(unstack(txtfile, form = spc ~ .wavelength))
  if ((nrow(spc) == length(wavelength)) & (ncol(spc) != length(wavelength))) {
    spc <- t(spc)
  }

  colnames(spc) <- levels(txtfile$.wavelength)

  txtfile <- txtfile[txtfile$.wavelength == txtfile$.wavelength[1], ]
  txtfile$.wavelength <- NULL
  txtfile$spc <- I(spc)

  spc <- new("hyperSpec", wavelength = wavelength, data = txtfile, labels = cols)

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, filename = file)
}
