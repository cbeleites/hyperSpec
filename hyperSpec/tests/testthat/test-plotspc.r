context("plotspc")

test_that("BARBITURATES", {
  expect_silent(
    spc <- do.call(collapse, barbiturates[1:3])
  )

  expect_silent(
    plotspc(spc, col = palette_matlab_dark(3), stacked = TRUE, lines.args = list(type = "h"))
  )
})
