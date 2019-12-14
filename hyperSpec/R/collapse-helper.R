collapse.helper <- function (){
tmp <- flu
wl (tmp) <- wl (tmp) + 0.011

dots <- list (flu, tmp)

wl.skeleton <- lapply (dots, wl)


tmp <- sapply (wl.skeleton, function (x) {
  any (diff (x) < wl.tolerance)  ## wavelengths have been ordered above
})
if (any (tmp))
  warning ("wl.tolerance (", wl.tolerance, ") larger than smallest wavelength difference within object(s) ",
           which (tmp))

wl.flesh <- unlist (wl.skeleton)
wl.i <- rep (NA_integer_, length (wl.flesh))
#wl.i [seq_along(wl (dots [[1]]))] <- seq_along(wl (dots [[1]]))
new.wl <- wl.skeleton [[1]]

for (i in seq_along(wl.skeleton [[1]])){
  wl.i [abs (wl.flesh - wl.flesh [i]) <= wl.tolerance] <- i
}

    # adding wavelengthy can be done in chunks of all unknown wls per object.
while (sum (is.na (wl.i)) > 0L){
  wl <- wl.flesh [which (is.na (wl.i)) [1]]
  new.wl <- c ()
}

# ave (wl.flesh, wl.i)

}
