#' Load all installed packages of the hySpc.\* package family
#'
#' This function loads all installed packages of the **hySpc._something_**
#' package family.
#'
#' @param dont_attach Packages that should *not* be attached
#' @param ... further parameters are handed to [base::library()]
#'
#' @export
#' @importFrom utils installed.packages
#' @examples
#' attach_hySpc()
attach_hySpc <- function(dont_attach = "hySpc.testthat", ...){
  hySpc_packages <- installed.packages()
  hySpc_packages <- grep("^hySpc[.]", row.names(hySpc_packages), value = TRUE)

  hySpc_packages <- setdiff(hySpc_packages, dont_attach)

  lapply(hySpc_packages, function(x){
    do.call("library", list(package = x, ...))
    }
  )
}
