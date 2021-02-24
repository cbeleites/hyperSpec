### ---------------------------------------------------------------------------
###
###  read.txt.long: import measurements from .txt file
###
###  Format:
###  (y x) wl int
###

#' @rdname DEPRECATED-textio
#' @concept moved to hySpc.read.txt
#'
#' @param file filename or connection
#' @param cols the column names specifying the column order.
#'
#' For data import, a list with elements `colname = label`; for export a
#'   character vector with the colnames.  Use `wavelength` to specify the
#'   wavelengths.
#' @param header the file has (shall have) a header line.
#' @param sep,row.names have their usual meaning (see
#'        [utils::read.table()]), but different default values.
#' @param ... arguments handed to [utils::read.table()].
#'
#' @author C. Beleites
#'
#' @seealso
#'
#' - [utils::read.table()] and [utils::write.table()],
#' - [R.matlab::R.matlab()] for `.mat` files,
#' - [hyperSpec::read.ENVI()] for ENVI data,
#' - [hyperSpec::read.spc()] for `.spc` files,
#' - Manufacturer specific file formats: [read.txt.Renishaw()].
#'
#'
# @keywords IO file
# @concept io
#'
#' @export
#' @importFrom utils read.table unstack
#'
#' @examples
#'
#' \dontrun{
#' vignette("fileio")
#' }
#'
#' ## export & import matlab files
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
#' ## ascii export & import
#'
#'
#' write_txt_long(flu,
#'   file = paste0(tempdir(), "/flu.txt"),
#'   cols = c(".wavelength", "spc", "c"),
#'   order = c("c", ".wavelength"),
#'   decreasing = c(FALSE, TRUE)
#' )
#'
#' read.txt.long(
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
#' read.txt.wide(
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

read.txt.long <- function(file = stop("file is required"),
                          cols = list(
                            .wavelength = expression(lambda / nm),
                            spc = "I / a.u."
                          ),
                          header = TRUE,
                          ...) {
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hySpc_deprecated(new = "read_txt_long")
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

  txtfile <- txtfile [txtfile$.wavelength == txtfile$.wavelength[1], ]
  txtfile$.wavelength <- NULL
  txtfile$spc <- I(spc)

  spc <- new("hyperSpec", wavelength = wavelength, data = txtfile, labels = cols)

  ## consistent file import behaviour across import functions
  .spc_io_postprocess_optional(spc, filename = file)
}
