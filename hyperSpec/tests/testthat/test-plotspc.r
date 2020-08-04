context("plotspc")

test_that("BARBITURATES", {
  expect_silent(
    spc <- do.call(collapse, barbiturates[1:3])
  )

  expect_silent(
    plotspc(spc, col = matlab.dark.palette(3), stacked = TRUE, lines.args = list(type = "h"))
  )
})
