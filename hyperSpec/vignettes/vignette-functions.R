# Citation/Bibliography ======================================================

#' Create package citation code for Rmd files.
#'
#' [cite_pkg()] - cites all references provided in a package.
#'
#' @param ... (character) Names of packages to cite.
#' @param bib bibliography as returned invisibly by [knitr::write_bib()].
#' @param prefix (character) Prefix string for keys in BibTeX entries;
#'        by default, it is `R-` unless `option('knitr.bib.prefix')` has been
#'        set to another string. The `prefix` should contain no symbols that
#'        have special meaning in regular expressions.
#'
#' @section Note
#'
#' All core R base packages should be cited by using `cite_pkg("base")`,
#' And **not**, e.g., `cite_pkg("utils")`.
#' See section examples for details.
#'
#' @examples
#' cite_pkg()
#'
#' cite_pkg("Rcpp")
#'
#' cite_pkg("Rcpp", prefix = "")
#'
#' cite_pkg("Rcpp", prefix = "pkg-")
#'
#' # Issues to consider: -------------------------------------
#' # All core R base packages should be cited by using
#' cite_pkg("base")
#'
#' knitr::write_bib(c("base"))
#' knitr::write_bib(c("graphics"))
#' knitr::write_bib(c("graphics", "utils"))
#' knitr::write_bib(c("base", "graphics", "utils"))
#'
#' cite_pkg(c("base"))
#' cite_pkg(c("graphics"))
#' cite_pkg(c("graphics", "utils"))
#' cite_pkg(c("base", "graphics", "utils"))

cite_pkg <- function(...,
  bib = {capture.output({x <- knitr::write_bib(..., prefix = prefix)}); x},
  prefix = getOption("knitr.bib.prefix", "R-")) {

  keys <- sapply(bib, function(x) x[[1]])
  keys <- gsub("@[A-Za-z]*[{]|,$", "", keys)

  keys <- grep(paste0("^(", prefix, ")?", ...), keys, value = TRUE)

  paste0("@", keys, collapse = ";")
}
