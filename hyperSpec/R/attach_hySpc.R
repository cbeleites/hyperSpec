#' Load all installed packages of the hySpc.\* package family.
#'
#' This function loads all installed packages of the **hySpc._something_**
#' package family.
#'
#' @param dont_attach Packages that should *not* be attached.
#' @param ... Further parameters are handed to [base::library()].
#'
#' @export
#' @importFrom utils installed.packages
#' @concept hyperSpec-main
#' @examples
#' attach_hySpc()

attach_hySpc <- function(dont_attach = "hySpc.testthat", ...) {
  installed_pkgs <- row.names(installed.packages())
  hySpc_packages <- grep("^hySpc[.]", installed_pkgs, value = TRUE)

  hySpc_packages <- setdiff(hySpc_packages, unique(c(dont_attach, .packages())))

  if (length(hySpc_packages) > 0) {
    message(
      "\n",  "The following packages are being attached: \n\n",
      paste0("  ", hySpc_packages, collapse = "\n"),  "\n"
    )

  } else {
    message("All installed `hySpc` family packages are already attached.")
  }

  lapply(hySpc_packages, function(x) {
    do.call("library", list(package = x))
  })
}
