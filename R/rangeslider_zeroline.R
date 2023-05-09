#' @importFrom plotly layout
#' @importFrom dplyr case_when
#' @importFrom plotly rangeslider
roboplotr_rangeslider <- function(p, enable_rangeslider, height = 0.1) {

  `enable_rangeslider$rangeslider` <- enable_rangeslider$rangeslider
  roboplotr_check_param(`enable_rangeslider$rangeslider`, c("date","Date","logical","character"))

  if(is.logical(enable_rangeslider$rangeslider)) {
    enable <- enable_rangeslider$rangeslider
    slider_range <- NULL
  } else {
    `enable_rangeslider$max` <- enable_rangeslider$max
    roboplotr_check_param(`enable_rangeslider$max`, c("date","Date","character","numeric"))
    slider_range <- map(enable_rangeslider, ~ as_date(.x) |> as.character()) |> unname()
    enable <- T
  }

  if(enable == T) {
    height <- case_when(height > 0.5 ~ 0.5, height < 0.1 ~ 0.1, TRUE ~ height)
    p |> rangeslider(start = slider_range[[1]], end = slider_range[[2]], thickness = height)
  } else { p }
}

#' @importFrom plotly layout
roboplotr_zeroline <- function(p, z) {
  if(!is.logical(z$zeroline) & !is.double(z$zeroline)) {
    stop("Zeroline must be TRUE, FALSE or of type double.", call. = F)
  } else if (z$zeroline == F & !is.numeric(z$zeroline)) {
    p
  } else {
    zero_line <- ifelse(z$zeroline == T, 0, z$zeroline)
    p |> layout(shapes= list(type = "line", x0 = z$xrange$min, x1 = z$xrange$max, xref = "x", y0 = zero_line, y1 = zero_line, yref = "y", layer = "below", line = list(width = 2))) |>
      onRender(jsCode = "
function(gd, params, data) {
let zeroline_relayout = {'shapes[0].x0': gd.layout.xaxis.range[0], 'shapes[0].x1': gd.layout.xaxis.range[1]}
Plotly.relayout(gd, zeroline_relayout)

gd.on('plotly_afterplot', function() {
  var line_label = data.zeroline
  let yticks = $(gd).find('g.ytick text')
  let zeroline = $(gd).find('path.zl')
  if(line_label == 0) {
    if (zeroline.length > 0) {
    zeroline[0].style['stroke'] = 'black'
    }
  } else {
  let label_translate
  if (zeroline.length > 0) { zeroline[0].style['stroke'] = 'rgb(232, 232, 232)' };
  yticks.filter(function(d, i) {
  if(this.textContent.trim().replace(',','.').replace('\u2212','-').replace(' ','') == line_label) {
  label_translate = i.getAttribute('transform')
    let lines = $(gd).find('path.ygrid')
    lines.filter(function(d, i) {
    if(i.getAttribute('transform') == label_translate) {
      i.style['stroke'] = 'black'
    }
    })
  }
  })
  }

})
}
                                                         ", data = list(zeroline = zero_line))
  }
}
