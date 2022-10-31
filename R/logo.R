#' @importFrom plotly layout
#' @importFrom RCurl base64Encode
roboplotr_logo <- function(p){

  image_file <- getOption("roboplot.logo")
  txt <- base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
  p |> layout(
    images = list(
      source = paste('data:image/png;base64', txt, sep=','),
      xref = "paper",
      yref = "domain",
      x = 1,
      sizex = 1,
      sizey = 1,
      xanchor="right",
      yanchor = "bottom"
    )
  )
}
