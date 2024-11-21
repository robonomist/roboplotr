## code to prepare `energiantuonti` dataset

energiantuonti <-
  robonomistServer::data_get("StatFin/ehk/statfin_ehk_pxt_13j9.px", tidy_time = T) |> 
  dplyr::filter(
    Maa != "KAIKKI MAAT YHTEENSÄ",
    Tiedot == "Arvo (miljoonaa euroa)",
    Energiatuote == "ENERGIATUOTTEET YHTEENSÄ",
    value > 0
  ) |> 
  tidyr::drop_na() |>
  dplyr::select(Alue = Maa, Suunta, time, value) |>
  dplyr::mutate(Alue = forcats::fct_reorder(Alue, value, sum, .desc = T, na.rm = T)) |>
  dplyr::mutate(rank = as.numeric(Alue)) |>
  dplyr::filter(rank <= 10) |>
  dplyr::mutate(Alue = as.character(Alue)) |>
  dplyr::select(-rank)

attr(energiantuonti, "robonomist_title") <- "Roboplotr-paketin testaamiseen energian tuonti ja vienti alkuperämaittain (suurimmat maat), muuttujina Alue ja Suunta"

usethis::use_data(energiantuonti, overwrite = TRUE)
