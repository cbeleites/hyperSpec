# The contents of "read.txt.PerkinElmer.R"

read.txt.PerkinElmer <- function(files = stop("filenames needed"), ..., label = list()) {
  ##  set default labels
  label <- modifyList(
    list(
      .wavelength = expression(lambda / nm),
      spc = expression(I[fl] / "a.u.")
    ),
    label
  )

  if (length(files) == 0) {
    warning("No files found.")
    return(new("hyperSpec"))
  }

  ## read the first file
  buffer <- matrix(scan(files[1], ...), ncol = 2, byrow = TRUE)

  ## first column gives the wavelength vector
  wavelength <- buffer[, 1]

  ## preallocate the spectra matrix:
  ##  one row per file x as many columns as the first file has
  spc <- matrix(ncol = nrow(buffer), nrow = length(files))

  ## the first file's data goes into the first row
  spc[1, ] <- buffer[, 2]

  ## now read the remaining files
  for (f in seq(along = files)[-1]) {
    buffer <- matrix(scan(files[f], ...), ncol = 2, byrow = TRUE)

    ## check whether they have the same wavelength axis
    if (!all.equal(buffer[, 1], wavelength)) {
      stop(paste(files[f], "has different wavelength axis."))
    }

    spc[f, ] <- buffer[, 2]
  }

  ## make the hyperSpec object
  spc <- new("hyperSpec", wavelength = wavelength, spc = spc, label = label)

  ## consistent file import behaviour across import functions
  hyperSpec::.spc_io_postprocess_optional(spc, files)
}
