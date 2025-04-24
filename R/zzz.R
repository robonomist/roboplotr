.onLoad <- function(..., override = F) {
  op <- options()
  op.roboplot.options <- list(
    roboplot.accessible = F,
    roboplot.artefacts = list(auto = F, filepath = getwd(), render = T, self_contained = F, zoom = 1, artefacts = "html", width = 992, height = 744, delay = 0.2),
    roboplot.axes = list(yformat = NULL, xformat = NULL, yposition = NULL, xposition = NULL, yfont = NULL, y2font = NULL, xfont = NULL, xangle = NULL, yangle = NULL),
    roboplot.border = list(xcolor = "black", ycolor = "black", xmirror = TRUE, ymirror = TRUE, xwidth = 1, ywidth = 1),
    roboplot.caption.template = "L\u00e4hde: {text}.",
    roboplot.caption.xref = "plot",
    roboplot.colors.background = "white",
    roboplot.colors.traces =  c("#c1272d","#f15a24","#f7931e","#dcc48a","#118f9a","#951d46"),
    roboplot.dashtypes = c("solid", "dash", "dot", "longdash", "dashdot", "longdashdot"),
    roboplot.empty.roboplot = list(message = NULL, title = T, caption = F, modebar = T, height = 550),
    roboplot.errorbars = list(xcolor = "#545454", ycolor = "#545454"),
    roboplot.grid = list(xcolor = "#E8E8E8", ycolor = "#E8E8E8", xcolor_minor = NULL, ycolor_minor = NULL, xwidth = 1, ywidth = 1, xdash = "solid", ydash = "solid", xtick = NULL, ytick = NULL),
    roboplot.patterntypes = c("","/","\\","x","-","|","+","."),
    roboplot.font.caption = list(size = 10, family = "Arial, sans-serif", path = NULL, color = "#696969", .font = "Arial", .fallback = "Arial"),
    roboplot.font.main = list(size = 14, family = "Arial, sans-serif", path = NULL, color = "#696969", .font = "Arial", .fallback = "Arial"),
    roboplot.font.title = list(size = 17, family = "Arial, sans-serif", path = NULL, color = "#696969", bold = T, .font = "Arial", .fallback = "Arial"),
    roboplot.height = 550,
    roboplot.infobox = list(background = "#E8E8E8", border = "black", font = "#696969", border_width = 1),
    roboplot.labels = list(bar = "none", pie = "percent", scatter = "none"),
    roboplot.legend = list(tidy = FALSE, xref = "paper"),
    roboplot.linewidth = 2,
    roboplot.locale = list(
      locale = "fi",
      separators = ", ",
      date = "%-d.%-m.%Y",
      ylegendlabs = list(left = "Vasen Y-akseli", right = "Oikea Y-akseli"),
      robotable_labels = list(
        search = "Etsi:",
        info = "N\u00e4ytet\u00e4\u00e4n rivit _START_-_END_ / _TOTAL_",
        lengthMenu = "N\u00e4yt\u00e4 _MENU_ rivi\u00e4 per sivu",
        emptyTable = "Tietoja ei saatavilla",
        first = "Ensimm\u00e4inen",
        last = "Viimeinen"
      ),
      modebar_dl_label = list(data = "Lataa tiedot", plot = "Lataa kuvio")
    ),
    roboplot.logo = system.file("images", "robonomist_wide.png", package = "roboplotr"),
    roboplot.markers = list(symbol = "circle", size = 8),
    roboplot.modebar = list(buttons = c("closest","compare","img_w","data_dl"), display = "hover", zoom = 1),
    roboplot.imgdl.wide = list(x = 1280, y = 720, title = 18, main = 15, caption = 11, suffix = "", type = "png", button_label = "Lataa kuvio (leve\U00E4)"),
    roboplot.imgdl.narrow = list(x = 810, y = 720, title = 18, main = 15, caption = 11, suffix = "", type = "png", button_label = "Lataa kuvio (kapea)"),
    roboplot.imgdl.small = list(x = 889, y = 500, title = 16, main = 13, caption = 10, suffix = "", type = "png", button_label = "Lataa kuvio (pieni)"),
    roboplot.rounding = 1,
    roboplot.table_options = list(stripecolor = "black", stripeopacity = 0.023),
    roboplot.title = list(include = T, xref = "plot"),
    roboplot.trace.border = list(color = "#E8E8E8", width = 1),
    roboplot.width = NA,
    roboplot.shinyapp = FALSE,
    roboplot.xaxis.ceiling = "default",
    roboplot.zeroline = list(color = "black", width = 2),
    roboplot.zoom = "scroll",
    roboplot.verbose = "All",
    roboplot.cur.options = "roboplotr"
  )

  if(override) {
    options(op.roboplot.options)
  } else {
    toset <- !(names(op.roboplot.options) %in% names(op))
    if(any(toset)) options(op.roboplot.options[toset])
  }

  invisible()
}
