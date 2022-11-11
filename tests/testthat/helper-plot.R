attr(energiantuonti, "frequency") <- "Quarterly"
roboplot_tester <- energiantuonti |>
  dplyr::filter(Alue %in% c("Kanada","Norja","Yhdistynyt kuningaskunta"),
                Suunta == "Tuonti") |>
  plotly::plot_ly(x = ~ time,y = ~ value)