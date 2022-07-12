###-----------------------------------------------------------------------------
###
### split.string - split string at pattern
###
###

split.string <- function (x, separator, trim.blank = TRUE, remove.empty = TRUE) {

  stopifnot(length(x) == 1) # we want a single character string

  pos <- gregexpr(separator, x)
  pos <- pos[[1]]

  if (length(pos) == 1 & pos[1] == -1) { # -1 means no match
    return(x)
  }

  pos <- matrix (c (1, pos + attr (pos, "match.length"),
                    pos - 1, nchar (x)),
                 ncol = 2)

  if (pos [nrow (pos), 1] > nchar (x))
    pos <- pos [- nrow (pos), ]

  x <- apply (pos, 1, function (p, x) substr (x, p [1], p [2]), x)

  if (trim.blank){
    blank.pattern <- "^[[:blank:]]*([^[:blank:]]+.*[^[:blank:]]+)[[:blank:]]*$"
    x <- sub (blank.pattern, "\\1", x)
  }

  if (remove.empty){
    x <- x [sapply (x, nchar) > 0]
  }

  x
}
