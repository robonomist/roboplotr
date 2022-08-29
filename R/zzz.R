.onLoad <- function(...) {
  op <- options()
  op.roboplot.options <- list(
    roboplot.colors.background = "white",
    roboplot.colors.border = list(x = "black", y = "black"),
    roboplot.colors.traces =  c("#c1272d","#f15a24","#f7931e","#dcc48a","#118f9a","#951d46"),
    roboplot.colors.grid = list(x = "#E8E8E8", y = "#E8E8E8"),
    roboplot.colors.font = "#696969",
    roboplot.colors.ticks = list(x = "#E8E8E8", y = "#E8E8E8"),
    roboplot.dashtypes = c("solid", "dash", "dot", "longdash", "dashdot", "longdashdot"),
    roboplot.patterntypes = c("","/","\\","x","-","|","+","."),
    roboplot.font.caption = list(size = 10, family = "sans-serif", path = NULL, color = "#696969"),
    roboplot.font.main = list(size = 14, family = "sans-serif", path = NULL, color = "#696969"),
    roboplot.font.title = list(size = 17, family = "sans-serif", path = NULL, color = "#696969", bold = function(x) paste0("<b>",x,"</b>")),
    roboplot.height = 550,
    roboplot.linewidth = 2,
    roboplot.logo = system.file("images", "robonomist.png", package = "roboplotr"),
    roboplot.png.font.size.lg = list(title = 31, main = 24, caption = 19),
    roboplot.png.font.size.sm = list(title = 23, main = 18, caption = 14),
    roboplot.modebar.buttons = c("closest","compare","img_w","data_dl")
  )
  toset <- !(names(op.roboplot.options) %in% names(op))
  if(any(toset)) options(op.roboplot.options[toset])

  invisible()
}
