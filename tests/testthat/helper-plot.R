attr(energiantuonti, "frequency") <- "Quarterly"
roboplot_tester <- energiantuonti |>
  dplyr::filter(Alue %in% c("USA","Norja","Iso-Britannia"),
                Suunta == "Tuonti") |>
  plotly::plot_ly(x = ~ time,y = ~ value)
