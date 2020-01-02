
# Citation/Bibliography ======================================================

#' Check if package exists
#'
#' @param pkg (character) A vector of package names.
#' @param lib.loc See [base::system.file()].
#'
#' @return A named vector of logical values.
#'
#' @md
#'
#' @examples
#' pkgs <- c("base", "ggplot2", "compiler", "mu")
#' pkg_exists(pkgs)

pkg_exists <- function(pkg = stop("package name needed"), lib.loc = NULL) {
  sapply(pkg, function(p) {
    dir <- system.file(package = p, lib.loc = lib.loc)
    nzchar(dir) > 0L
  })
}

#' Check if package belongs to R base system
#'
#' @param pkg (character) A vector of package names.
#'
#' @return A named vctor of logical values.
#'         `FALSE` means that either that a package is not a "base" package or
#'          that it is not installed (does not exist on a computer).
#'
#' @md
#'
#' @examples
#' pkgs <- c("base", "ggplot2", "compiler", "mu")
#' is_base_pkg(pkgs)
#'
is_base_pkg <- function(pkg) {
  sapply(pkg, function(x) {
    pkg_exists(x) && grepl("^base$", packageDescription(x, fields = "Priority"))
  })
}


#' Rename package that belongs to R base system to "base"
#'
#' @param pkg (character) A vector of package names.
#'
#' @return A vector of package names with all R base packages renamed to "base".
#'
#' @examples
#' pkgs <- c("base", "ggplot2", "compiler", "mu")
#' pkg_or_base(pkgs)
#'
pkg_or_base <- function(pkg) {
  pkg[sapply(pkg, is_base_pkg)] <- "base"
  pkg
}


#' Make Bibtex bibliography
#'
#' Make Bibtex bibliography. Similar to [knitr::write_bib()], just for all R base
#' packages a bibliography of base R is created.
#'
#' @param ... (character) Names of packages. Default is `.packages()`.
#' @param file   See [knitr::write_bib()].
#' @param prefix See [knitr::write_bib()].
#' @param tweak  See [knitr::write_bib()].
#' @param width  See [knitr::write_bib()].
#'
#' @return See [knitr::write_bib()].
#' @seealso
#'
#' - [knitr::write_bib()]
#'
#' @md
#' @examples
#' make_bib("base", "ggplot2")
#'
#' u <- make_bib()
make_bib <- function(..., file = NULL, tweak = TRUE, width = NULL, prefix = "") {
  if (is.null(file)) {file <- ""}
  pkg <- c(...)
  if (length(pkg) == 0L) {
    pkg <- .packages()
  }
  pkg <- unique(pkg_or_base(pkg))

  knitr::write_bib(pkg, file = file, tweak = tweak, width = width,
    prefix = prefix)
}

#' Get citation keys that are created with make_bib()
#'
#' @param ... (character) Names of packages. Default is `.packages()`.
#' @md
#' @examples
#' get_citation_keys()
#' get_citation_keys("base", "ggplot2")
get_citation_keys <- function(...) {
  capture.output(rez <- make_bib(...))
  unname(sapply(rez, function(x) gsub("@.*?\\{(.*?),", "\\1", x[1], perl = TRUE)))
}

#' Create package citation code for Rmd files.
#'
#' [cite_pkg()] - cites a package.
#' [cite_pkg_all()] - cites all references provided in a package.
#'
#' @param ... (character) Names of packages. Default is `base`.
#' @param sort (logical) Should keys be sorted alphabetically?
#' @md
#' @examples
#' cite_pkg()
#'
#' cite_pkg("Rcpp", "ggplot2")
cite_pkg_all <- function(..., sort = FALSE) {
  keys <- get_citation_keys(...)
  if (isTRUE(sort)) {keys <- sort(keys)}
  paste0("[", paste0("@", keys, collapse = ";"), "]")
}

cite_pkg <- function(...) {
  keys <- c(...)
  paste0("[", paste0("@", keys, collapse = ";"), "]")
}

# Other ======================================================================

nice_paste <- function(...) {
  fnames <- c(...)

  if (length(fnames) == 2L) {
    fnames <- paste(fnames, collapse = " and ")
  }

  if (length(fnames) > 1L) {
    fnames[length(fnames)] <- paste("and", tail(fnames, 1))
    fnames <- paste(fnames, collapse = ", ")
  }

  fnames
}

list_funs_rmd <- function(pattern, envir = getNamespace("hyperSpec")){
  funs <- ls(envir = envir, pattern = pattern)
  funs <- paste0("`", funs, "()`{.r}")
  nice_paste(funs)
}
