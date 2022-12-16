.onLoad <- function(..., override = F) {
  op <- options()
  op.roboplot.options <- list(
    roboplot.artefacts = list(auto = F, filepath = getwd(), render = T, self_contained = F, artefacts = "html"),
    roboplot.caption = list(prefix = "L\uE4hde: ", lineend = ".", updated = NULL),
    roboplot.colors.background = "white",
    roboplot.colors.border = list(x = "black", y = "black"),
    roboplot.colors.traces =  c("#c1272d","#f15a24","#f7931e","#dcc48a","#118f9a","#951d46"),
    roboplot.colors.grid = list(x = "#E8E8E8", y = "#E8E8E8"),
    roboplot.colors.ticks = list(x = "#E8E8E8", y = "#E8E8E8"),
    roboplot.dashtypes = c("solid", "dash", "dot", "longdash", "dashdot", "longdashdot"),
    roboplot.patterntypes = c("","/","\\","x","-","|","+","."),
    roboplot.font.caption = list(size = 10, family = "Arial, sans-serif", path = NULL, color = "#696969"),
    roboplot.font.main = list(size = 14, family = "Arial, sans-serif", path = NULL, color = "#696969"),
    roboplot.font.title = list(size = 17, family = "Arial, sans-serif", path = NULL, color = "#696969", bold = T),
    roboplot.height = 550,
    roboplot.linewidth = 2,
    roboplot.logo = system.file("images", "robonomist.png", package = "roboplotr"),
    roboplot.modebar.buttons = c("closest","compare","img_w","data_dl"),
    roboplot.imgdl.wide = list(x = 1280, y = 720, title = 31, main = 24, caption = 19, suffix = "_levea", type = "png"),
    roboplot.imgdl.narrow = list(x = 810, y = 720, title = 31, main = 24, caption = 19, suffix = "_kapea", type = "png"),
    roboplot.imgdl.small = list(x = 889, y = 500, title = 23, main = 18, caption = 14, suffix = "_pieni", type = "png"),
    roboplot.width = NULL,
    roboplot.xaxis.ceiling = "default",
    roboplot.verbose = "All"
  )

  if(override) {
    options(op.roboplot.options)
  } else {
    toset <- !(names(op.roboplot.options) %in% names(op))
    if(any(toset)) options(op.roboplot.options[toset])
  }

  invisible()
}
