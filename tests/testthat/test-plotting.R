test_that("Test for plot grid colors", {
  expect_s3_class(roboplot_tester |> roboplotr_grid(list(x = "black", y = "black"),list(x = "darkgrey", y = "darkgrey")), "plotly")
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
  expect_named(set_hovertext(NULL), c("dateformat","rounding","unit","extra","col"))
  expect_equal(set_hovertext(rounding = 3)$rounding, 3)
  expect_match(set_hovertext(frequency = "Daily")$dateformat, "%-d\\.%-m\\.%Y")
})

test_that("Roboplot options are reset", {
  set_roboplot_options(reset = TRUE)
  expect_match(getOption("roboplot.caption.template"), "Lähde: \\{text\\}.")
  expect_match(getOption("roboplot.colors.background"),"white")
  expect_mapequal(getOption("roboplot.border"), list(xcolor = "black",ycolor = "black", xmirror = TRUE, ymirror = TRUE, xwidth = 1, ywidth = 1))
  expect_mapequal(getOption("roboplot.grid"), list(xcolor = "#E8E8E8", ycolor = "#E8E8E8", xwidth = 1, ywidth = 1, xdash = "solid", ydash = "solid",xtick = NULL, ytick = NULL))
  expect_setequal(getOption("roboplot.colors.traces"), c("#c1272d","#f15a24","#f7931e","#dcc48a","#118f9a","#951d46"))
  expect_setequal(getOption("roboplot.dashtypes"), c("solid","dash","dot","longdash","dashdot","longdashdot"))
  expect_mapequal(getOption("roboplot.font.caption"), list(size = 10, family = "Arial, sans-serif", path = NULL, color = "#696969", .font = "Arial", .fallback = "Arial"))
  expect_mapequal(getOption("roboplot.font.main"), list(size = 14, family = "Arial, sans-serif", path = NULL, color = "#696969", .font = "Arial", .fallback = "Arial"))
  expect_mapequal(getOption("roboplot.font.title"), list(size = 17, family = "Arial, sans-serif", path = NULL, color = "#696969", bold = T, .font = "Arial", .fallback = "Arial"))
  expect_equal(getOption("roboplot.height"), 550)
  expect_equal(getOption("roboplot.linewidth"), 2)
  expect_match(getOption("roboplot.logo"),system.file("images", "robonomist_wide.png", package = "roboplotr"))
  expect_setequal(getOption("roboplot.modebar"), list(buttons = c("closest","compare","img_w","data_dl"), display = "hover"))
  expect_setequal(getOption("roboplot.patterntypes"), c("","/","\\","x","-","|","+","." ))
  expect_mapequal(getOption("roboplot.imgdl.wide"), list(x = 1280, y = 720, title = 18, main = 15, caption = 11, suffix = "_levea", type = "png"))
  expect_mapequal(getOption("roboplot.imgdl.narrow"), list(x = 810, y = 720, title = 18, main = 15, caption = 11, suffix = "_kapea", type = "png"))
  expect_mapequal(getOption("roboplot.imgdl.small"), list(x = 889, y = 500, title = 16, main = 13, caption = 10, suffix = "_pieni", type = "png"))
  expect_equal(getOption("roboplot.rounding"), 1)
  expect_equal(getOption("roboplot.shinyapp"),FALSE)
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

test_that("Test for rangeslider", {
  expect_s3_class(roboplot_tester |> roboplotr_rangeslider(enable_rangeslider = list(rangeslider = FALSE)), "plotly")
  expect_s3_class(roboplot_tester |> roboplotr_rangeslider(enable_rangeslider = list(rangeslider = TRUE)), "plotly")
  expect_s3_class(roboplot_tester |> roboplotr_rangeslider(enable_rangeslider = list(rangeslider = FALSE)), "plotly")
  expect_s3_class(roboplot_tester |> roboplotr_rangeslider(enable_rangeslider = list(rangeslider = TRUE, max = c("2020-01-01","2022-01-01"))), "plotly")
})

# roboplotr_rangeslider ---------------------------------------------------


