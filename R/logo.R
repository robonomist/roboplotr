#' @importFrom plotly layout
#' @importFrom RCurl base64Encode
roboplotr_logo <- function(p){

  image_file <- getOption("roboplot.logo")
  if(image_file == "none") {
    image_file <- system.file("images","none.png",package = "roboplotr")
  }
  txt <- base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
  p |> layout(
    images = list(
      source = paste('data:image/png;base64', txt, sep=','),
      xref = "paper",
      yref = "paper",
      x = 1,
      sizex = 1,
      sizey = 1,
      xanchor="right",
      yanchor = "bottom"
    )
  )
}

#' @importFrom plotly layout
#' @importFrom RCurl base64Encode
robotable_logo <- function(height = "30px"){
  image_file <- getOption("roboplot.logo")
  if(image_file == "none") {
    image_file <- system.file("images","none.png",package = "roboplotr")
  }
  txt <- base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
  tags$img(src = paste('data:image/png;base64', txt, sep=','), style = "float: right", height = height)

}
