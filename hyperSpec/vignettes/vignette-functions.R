# Citation/Bibliography ======================================================

#' Create package citation code for Rmd files.
#'
#' [cite_pkg()] - cites all references provided in a package.
#'
#' @param bib bibliography as returned invisibly by [knitr::write_bib()]
#' @param ... (character) Names of packages to cite
#' @md
#' @examples
#' cite_pkg()
#'
#' cite_pkg("Rcpp")

cite_pkg <- function(bib = write_bib(...), ..., prefix = "R-") {
  keys <- sapply(bib, function(x) x [[1]])
  keys <- gsub("@[A-Za-z]*[{]|,$", "", keys)

  keys <- grep(paste0("^(R-)?", ...), keys, value = TRUE)

  paste0(paste0("@", keys, collapse = ";"))
}
