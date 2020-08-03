# These functions generate datasets that are mainly used for unit testing.
# V. Gegzna

# Generate spectra -----------------------------------------------------------
# Generate data that contains several spectra

# n_wl - number of wavelengths (points / spectrum).
# n - number of spectra.

generate_hy_spectra <- function(n_wl = 50, n = 20) {
  gr <- rep(c("A", "B"), length.out = n)
  x <- seq(-3, 3, length.out = n_wl)

  mean <- seq(-1.5, 1.5, length.out = n)
  amp <- sqrt(seq(160, 2, length.out = n))
  wls <- seq(400, 700, length.out = n_wl)

  mat <- apply(data.frame(mean, amp), 1, function(i) {
    i[["amp"]] * dnorm(x, mean = i[["mean"]])
  })

  new("hyperSpec",
    spc = t(mat),
    data = data.frame(gr = gr, x = (1:n) / 4, c = amp, t = mean - min(mean)),
    wavelength = wls,
    labels = list(spc = "I, a.u.", x = "l, cm", .wavelength = "lambda, nm")
  )
}

# Generate profiles ----------------------------------------------------------
# Generate data that contains depth, concentration, ts profiles at a single wl

# n - number of spectra.
# wavelength - a single numeric value for `wavelength`.

generate_hy_profile <- function(n = 20, wavelength = 550) {
  new("hyperSpec",
    spc = as.matrix(cos((1:n) / pi)),
    data = data.frame(
      z = log(1:n),
      c = sin(1:n),
      t = seq(0, 400, length.out = n)
    ),
    wavelength = wavelength,
    labels = list(
      spc = "I, a.u.",
      c = "Concentration, mol/l",
      t = "t, s",
      z = "z, mm",
      .wavelength = "lambda, nm"
    )
  )
}

# Generate spectroscopic map -------------------------------------------------

# n_wl - number of wavelengths (points / spectrum).
# n_xy - number of pixels in x and y directions.
# k - integer not larger than n_xy. If k = n_xy, there are no empty pixels
# in the map.

generate_hy_map <- function(n_wl = 5, n_xy = 7, k = 5) {
  new("hyperSpec",
    spc = matrix(rep(c(1:5, 2), length.out = n_wl * n_xy * k), ncol = n_wl),
    data = data.frame(
      x = rep(1:n_xy, each = k),
      y = rep(1:n_xy, times = k),
      gr = factor(rep(LETTERS[1:3], length.out = n_xy * k))
    ),
    wavelength = seq(5000, 8000, length.out = n_wl),
    labels = list(
      spc = "I, a.u.",
      x = "x, px",
      y = "y, px",
      .wavelength = "k, 1/cm"
    )
  )
}


# Unit tests -----------------------------------------------------------------

.test(generate_hy_spectra) <- function() {
  context("data for unit tests")

  test_that("generate_hy_spectra() data dimensions did not change", {
    hy_spectra <- generate_hy_spectra()

    expect_is(hy_spectra, "hyperSpec")
    expect_equal(nrow(hy_spectra), 20)
    expect_equal(ncol(hy_spectra), 5)
    expect_equal(nwl(hy_spectra), 50)
  })

  test_that("generate_hy_profile() data dimensions did not change", {
    hy_profile <- generate_hy_profile()

    expect_is(hy_profile, "hyperSpec")
    expect_equal(nrow(hy_profile), 20)
    expect_equal(ncol(hy_profile), 4)
    expect_equal(nwl(hy_profile), 1)
  })

  test_that("generate_hy_map() data dimensions did not change", {
    hy_map <- generate_hy_map()

    expect_is(hy_map, "hyperSpec")
    expect_equal(nrow(hy_map), 35)
    expect_equal(ncol(hy_map), 4)
    expect_equal(nwl(hy_map), 5)
  })
}
