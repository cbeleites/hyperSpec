##' Read INI files
##'
##' \code{read.ini} reads ini files of the form
##'
##' [section]
##' key = value
##'
##' into a list.
##'
##' \code{read.ini} sanitizes the element names and tries to convert scalars and comma separated
##' numeric vectors to numeric.
##' @export
##' @rdname read-ini
##' @param con connection or file name
##' @param skip number of lines to skip before first \code{[section]} starts
##' @param encoding see \code{\link[base]{readLines}}
##' @author C. Beleites
##' @return a list with one element per section in the .ini file, each containing a list with elements
##' for the key-value-pairs.
##' @keywords IO file

read.ini <- function (con = stop ("Connection con needed."), skip = NULL, encoding = "unknown"){
  Lines  <- readLines (con, encoding = encoding)
  ## remove leading lines, if they are not a section
  if (!is.null (skip))
      Lines  <- Lines [-seq_len (skip)]

  sections <- grep ("[[].*[]]", Lines)

  content <- Lines [- sections]
  ini <- as.list (gsub ("^.*=[[:blank:]]*", "", content)) # removes blanks behind equal sign
  names (ini) <- .sanitize.name (gsub ("[[:blank:]]*=.*$", "", content)) # see above: removes in front of equal sign

  # try converting to numeric
  tmp <- lapply (ini, function (x) strsplit (x, ",") [[1]])
  tmp <- suppressWarnings (lapply (tmp, as.numeric))
  numbers <- ! sapply (tmp, function (x) any (is.na (x)))
  ini [numbers] <- tmp [numbers]

  tmp <- rep.int (seq_along (sections), diff (c (sections, length (Lines) + 1)) - 1)
  ini <- split (ini, tmp)

  sections <- Lines [sections]
  sections <- .sanitize.name (gsub ("^.(.*).$", "\\1", sections))
  names (ini) <- sections

  ini
}

.sanitize.name <- function (name){
  gsub ("[^a-zA-Z0-9._]", ".", name)
}
