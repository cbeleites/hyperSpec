## TODO: blog post after feature is finished

## consistent optional treatment during file import

.fileio.optional <- function (spc, filename, ...,
                              file.remove.emptyspc = hy.getOption ("file.remove.emptyspc"),
                              file.keep.name = hy.getOption ("file.keep.name"),
															tolerance = hy.getOption ("tolerance")){

	tolerance <- .checkpos (tolerance, "tolerance")

  if (file.remove.emptyspc) {
    ## number of NAs in each spectrum
    nas <- rowSums (is.na (spc))

    ## number of zero-values in each spectrum
    zeros <- rowSums (abs (spc) < tolerance, na.rm = TRUE)

    spc <- spc [nas + zeros < nwl (spc)]
  }

  if (file.keep.name & nrow (spc) > 0L){
    if (is.null (spc@data$filename)){
      if (is (filename, "connection"))
        filename <- summary(filename)$description

      spc@data$filename <- filename
      spc@label$filename <- "filename"
    } else {
      warning ("$filename already exists. => Skipping file.keep.name")
    }
  }

  spc
}

##' @include unittest.R
.test (.fileio.optional) <- function (){
  context (".fileio.optional")

  test_that ("removing of zero/NA spectra", {
    tmp <- fluNA # spectrum 2 is all NA
    tmp [[3]] <- 0
    tmp [[5]] <- runif (nwl(tmp), min = - hy.getOption("tolerance") / 2, max = hy.getOption("tolerance") / 2)
    tmp [[,, 450~455]] <- NA

    expect_equivalent (.fileio.optional (tmp, file.remove.emptyspc = TRUE, file.keep.name = FALSE),
                       tmp [- c(2, 3, 5)])
  })

  test_that ("filenames", {
    flu$filename <- NULL
    tmp <- .fileio.optional (flu, filename = "test", file.remove.emptyspc = FALSE, file.keep.name = TRUE)
    expect_true (all (tmp$filename == "test"))

    expect_warning(tmp <- .fileio.optional (tmp, filename = "test2",
                                            file.remove.emptyspc = FALSE, file.keep.name = TRUE)
    )
    expect_true (all (tmp$filename == "test"))
  })


  options.state <- .options
  on.exit(do.call (hy.setOptions, options.state))

  test_that("option treatment", {
    hy.setOptions(file.remove.emptyspc = FALSE)
    skip ("TODO: implement")
    do.call (hy.setOptions, options.state)
  })
}
