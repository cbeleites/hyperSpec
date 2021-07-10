#' @name DEPRECATED-read.ini
#' @concept moved to hySpc.read.txt
#'
#' @title (DEPRECATED)
#'        Read INI files
#'
#' @description
#'
#' These data input functions are **deprecated** and they will be removed in
#' the next release of \pkg{hyperspec} package.
#' Now functions in package \pkg{hySpc.read.txt}
#' ([link](https://r-hyperspec.github.io/hySpc.read.txt/reference/index.html))
#' should be used as the alternatives.
#'
#' @details
#'
#' `read.ini` reads ini files of the form
#' ```
#' [section]
#' key = value
#' ```
#' into a list.
#'
#' `read.ini` sanitizes the element names and tries to convert scalars and comma separated
#' numeric vectors to numeric.
#' @rdname DEPRECATED-read-ini
#' @param con connection or file name
#' @param skip number of lines to skip before first `[section]` starts
#' @param encoding see [base::readLines()]
#' @author C. Beleites
#' @return a list with one element per section in the .ini file, each containing a list with elements
#' for the key-value-pairs.
#'
#' @export
#'


read.ini <- function(con = stop("Connection con needed."), skip = NULL, encoding = "unknown") {
  Lines <- readLines(con, encoding = encoding)
  ## remove leading lines, if they are not a section
  if (!is.null(skip)) {
    Lines <- Lines [-seq_len(skip)]
  }

  sections <- grep("[[].*[]]", Lines)

  content <- Lines [-sections]
  ini <- as.list(gsub("^.*=[[:blank:]]*", "", content)) # removes blanks behind equal sign
  names(ini) <- .sanitize.name(gsub("[[:blank:]]*=.*$", "", content)) # see above: removes in front of equal sign

  # try converting to numeric
  tmp <- lapply(ini, function(x) strsplit(x, ",")[[1]])
  tmp <- suppressWarnings(lapply(tmp, as.numeric))
  numbers <- !sapply(tmp, function(x) any(is.na(x)))
  ini [numbers] <- tmp [numbers]

  tmp <- rep.int(seq_along(sections), diff(c(sections, length(Lines) + 1)) - 1)
  ini <- split(ini, tmp)

  sections <- Lines [sections]
  sections <- .sanitize.name(gsub("^.(.*).$", "\\1", sections))
  names(ini) <- sections

  ini
}

.sanitize.name <- function(name) {
  gsub("[^a-zA-Z0-9._]", ".", name)
}
