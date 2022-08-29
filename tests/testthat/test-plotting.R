test_that("grid works", {
  expect_s3_class(plotly::plot_ly() |> roboplot_set_grid(), "plotly")
})
