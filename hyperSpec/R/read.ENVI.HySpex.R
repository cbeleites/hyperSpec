read.ENVI.HySpex <- function (file = stop ("read.ENVI.HySpex: file name needed"),
                              headerfile = NULL, header = list (), keys.hdr2data = NULL, ...) {

  headerfile <- .find.ENVI.header (file, headerfile)
  keys <- readLines (headerfile)
  keys <- .read.ENVI.split.header (keys)
  keys <- keys [c ("pixelsize x", "pixelsize y", "wavelength units")]

  header <- modifyList (keys, header)

  ## most work is done by read.ENVI
  spc <- read.ENVI (file = file, headerfile = headerfile, header = header, ..., pull.header.lines = FALSE)

  label <- list (x = "x / pixel",
                 y = "y / pixel",
                 spc = 'I / a.u.',
                 .wavelength = as.expression (bquote (lambda / .(u), list (u = keys$`wavelength units`))))

  labels (spc) <- label

  spc
}
