#################################################################################
###
###  plotmap - plot spectral maps
###
###  plots intensity or extra data column over 2 extra data columns

## TODO: check whether function should be applied or not



#' Plot a map and identify/select spectra in the map
#'
#' [lattice::levelplot()] functions for hyperSpec objects.  An image or map of a summary
#' value of each spectrum is plotted. Spectra may be identified by mouse click.
#'
#' The `model` can contain the special column name `.wavelength` to specify the wavelength
#' axis.
#'
#' `plotmap`, `map.identify`, and the `levelplot` methods internally use the same
#' gateway function to [lattice::levelplot()]. Thus `transform.factor` can be used
#' with all of them and the panel function defaults to [lattice::panel.levelplot.raster()]
#' for all three. Two special column names, `.rownames` and `.wavelength` may be used.
#'
#' `levelplot` plots the spectra matrix.
#'
#' `plotvoronoi` calls `plotmap` with different default settings, namely the panel
#' function defaults to [latticeExtra::panel.voronoi()].
#' [latticeExtra::panel.voronoi()] depends on either of the packages 'tripack' or 'deldir'
#' being installed. For further information, please consult the help page of
#' [latticeExtra::panel.voronoi()].  On the [faux_cell()] data set, `plotmap`
#' is roughly 5 times faster than `plotvoronoi` using tripack, and ca. 15 times faster than
#' `plotvoronoi` using deldir. Package tripack, however, is free only for non-commercial
#' use. Also, it seems that tripack version hang (R running at full CPU power, but not responding
#' nor finishing the calculation) for certain data sets. In this case, `mix = TRUE` may help.
#'
#' `map.identify` calls `plotmap` and `plotvoronoi`, respectively and waits for
#' (left) mouse clicks on points. Other mouse clicks end the input.
#'
#' Unlike [lattice::panel.identify()], the indices returned by `map.identify` are in
#' the same order as the points were clicked. Also, multiple clicks on the same point are returned
#' as multiple entries with the same index.
#'
#' `map.identify` uses option `debuglevel` similar to [spc.identify()]:
#' `debuglevel == 1` will plot the tolerance window if no data point was inside (and
#' additionally labels the point) while `debuglevel == 2` will always plot the tolerance
#' window.
#'
#' The `map.sel.*` functions offer further interactive selection, see
#' [map.sel.poly()].
#'
#' @rdname levelplot
#' @aliases plotmap plotvoronoi levelplot,formula,hyperSpec-method
#'   levelplot,hyperSpec,missing-method map.identify
#' @param object,data the `hyperSpec` object
#' @param model,x formula specifying the columns of object that are to be
#'   displayed by [lattice::levelplot()]
#' @param func,func.args Before plotting, `plotmap` applies function
#'   `func` with the arguments given in the list `func.args` to each
#'   of the spectra. Thus a single summary value is displayed for each of the
#'   spectra.
#'
#' This can be suppressed manually by setting `func` to NULL. It is automatically suppressed if
#' `.wavelength` appears in the formula.
#' @param voronoi Should the plot for identifying spectra by mouse click be
#'   produced by `plotmap` (default) or `plotvoronoi`?
#' @param ... further arguments are passed down the call chain, and finally
#'   to [lattice::levelplot()]
#' @return `map.identify` returns a vector of row indices into
#'   `object` of the clicked points.
#'
#' The other functions return a lattice object.
#' @author C. Beleites
#' @seealso `vignette(plotting)`, `vignette(hyperSpec)`
#'
#' [plot()]
#' @export
#'
#' @keywords hplot
#' @concept plotting
#' @concept plot generation
#'
#' @examples
#' \dontrun{
#' vignette(plotting)
#' vignette(hyperSpec)
#' }
#'
#' levelplot(spc ~ y * x, faux_cell[, , 1003]) # properly rotated
#' plotmap(faux_cell[, , 1003])
#'
#' # plot spectra matrix
#' levelplot(spc ~ .wavelength * t, laser, contour = TRUE, col = "#00000080")
#' # see also plotmat
#'
#' plotmap(faux_cell, region ~ x * y)
#'
#' # Voronoi plots
#' smpl <- sample(faux_cell, 300)
#' plotmap(smpl, region ~ x * y)
#' if (require(tripack)) {
#'   plotvoronoi(smpl, region ~ x * y)
#' }
#' if (require(deldir)) {
#'   plotvoronoi(smpl, region ~ x * y,
#'     use.tripack = FALSE
#'   )
#' }
#' @importFrom utils modifyList
plotmap <- function(object, model = spc ~ x * y,
                    func = mean, func.args = list(), ...) {
  chk.hy(object)
  validObject(object)

  if (!is.null(func) & !any(grepl("[.]wavelength", model))) {
    object <- do.call(apply, c(list(object, 1, func), func.args))
  }

  dots <- modifyList(
    list(aspect = "iso"),
    list(...)
  )

  dots <- c(list(x = model, data = object), dots)

  do.call(.levelplot, dots)
}
