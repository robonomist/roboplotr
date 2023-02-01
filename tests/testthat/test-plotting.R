test_that("Test for plot grid colors", {
  expect_s3_class(roboplot_tester |> roboplotr_set_grid(list(x = "black", y = "black"),list(x = "darkgrey", y = "darkgrey")), "plotly")
})

test_that("Test for plot background color", {
  expect_s3_class(roboplot_tester |> roboplotr_set_background(), "plotly")
})

test_that("Test for plot margins", {
  expect_s3_class(roboplot_tester |> roboplotr_set_margin(list(t = 20, r = 20, b = 20, l = 20, pad = 10)), "plotly")
})

test_that("Test for logos", {
  expect_s3_class(roboplot_tester |> roboplotr_logo(), "plotly")
})

test_that("Test for hovertemplate time format", {
  expect_match(roboplotr_hovertemplate_freq("Annual"), "%Y")
  expect_match(roboplotr_hovertemplate_freq("dingus"), "%Y-%m-%d")
  expect_match(roboplotr_hovertemplate_freq("dingus"), "%Y-%m-%d")
})

test_that("Test for hovertemplate templating list", {
  expect_named(set_hovertext(NULL), c("dateformat","rounding","unit","extra"))
  expect_equal(set_hovertext(rounding = 3)$rounding, 3)
  expect_match(set_hovertext(frequency = "Daily")$dateformat, "%-d\\.%-m\\.%Y")
})

test_that("Roboplot options are reset", {
  set_roboplot_options(reset = TRUE)
  expect_mapequal(getOption("roboplot.caption"), list(prefix = "",lineend = "",updated = FALSE))
  expect_match(getOption("roboplot.colors.background"),"white")
  expect_mapequal(getOption("roboplot.colors.border"), list(x = "black",y = "black"))
  expect_mapequal(getOption("roboplot.colors.grid"), list(x = "#E8E8E8",y = "#E8E8E8"))
  expect_mapequal(getOption("roboplot.colors.ticks"), list(x = "#E8E8E8",y = "#E8E8E8"))
  expect_setequal(getOption("roboplot.colors.traces"), c("#c1272d","#f15a24","#f7931e","#dcc48a","#118f9a","#951d46"))
  expect_setequal(getOption("roboplot.dashtypes"), c("solid","dash","dot","longdash","dashdot","longdashdot"))
  expect_mapequal(getOption("roboplot.font.caption"), list(size = 10, family = "Arial, sans-serif", path = NULL, color = "#696969"))
  expect_mapequal(getOption("roboplot.font.main"), list(size = 14, family = "Arial, sans-serif", path = NULL, color = "#696969"))
  expect_mapequal(getOption("roboplot.font.title"), list(size = 17, family = "Arial, sans-serif", path = NULL, color = "#696969", bold = T))
  expect_equal(getOption("roboplot.height"), 550)
  expect_equal(getOption("roboplot.linewidth"), 2)
  expect_match(getOption("roboplot.logo"),system.file("images", "robonomist_wide.png", package = "roboplotr"))
  expect_setequal(getOption("roboplot.modebar.buttons"), c("closest","compare","img_w","data_dl"))
  expect_setequal(getOption("roboplot.patterntypes"), c("","/","\\","x","-","|","+","." ))
  expect_mapequal(getOption("roboplot.imgdl.wide"), list(x = 1280, y = 720, title = 31, main = 24, caption = 19, suffix = "_levea", type = "png"))
  expect_mapequal(getOption("roboplot.imgdl.narrow"), list(x = 810, y = 720, title = 31, main = 24, caption = 19, suffix = "_kapea", type = "png"))
  expect_mapequal(getOption("roboplot.imgdl.small"), list(x = 889, y = 500, title = 23, main = 18, caption = 14, suffix = "_pieni", type = "png"))
  expect_mapequal(getOption("roboplot.shinyapp"),list(shinyapp = FALSE, container = NULL))
  expect_match(getOption("roboplot.verbose"),"All")
  expect_match(getOption("roboplot.xaxis.ceiling"),"default")
})

test_that("Colors are valid", {
  expect_true(roboplotr_are_colors("black"))
  expect_true(roboplotr_are_colors("#F00000"))
  expect_false(roboplotr_are_colors("maplegreen"))
  expect_condition(roboplotr_valid_colors("maplegreen"))
  expect_no_condition(roboplotr_valid_colors("black"))
})

test_that("Test for zeroline", {
  expect_s3_class(roboplot_tester |> roboplotr_rangeslider(enable = FALSE), "plotly")
  expect_s3_class(roboplot_tester |> roboplotr_rangeslider(enable = T), "plotly")
  expect_s3_class(roboplot_tester |> roboplotr_rangeslider(enable = T, slider_range = c("2020-01-01","2022-01-01")), "plotly")
})

