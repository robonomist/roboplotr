## code to prepare `energiantuonti` dataset

energiantuonti <- robonomistClient::data_get("StatFin_Passiivi/ene/ehk/statfinpas_ehk_pxt_004_202100_fi.px") |>
  dplyr::filter(Tiedot == "Arvo, M€", Maa %in% c("Venäjä","OECD-maat","EU-maat","Ruotsi","Alankomaat","Norja","Yhdysvallat (USA)","Yhdistynyt kuningaskunta","Belgia","Kanada" ),
         `Tuonti/Vienti` %in% c("Tuonti","Vienti"),
         stringr::str_detect(Kausi, "^[0-9]\\.\\s"),
         Tuote == "ENERGIA YHTEENSÄ", value > 0) |>
  dplyr::mutate(time = stringr::str_glue("{Vuosi}Q{stringr::str_sub(Kausi,1,1)}") |>
           lubridate::yq()) |>
  dplyr::select(Alue = Maa, Suunta = `Tuonti/Vienti`, time, value)

attr(energiantuonti, "robonomist_title") <- "Roboplotr-paketin testaamiseen energian tuonti ja vienti alkuperämaittain (suurimmat maat), muuttujina Alue ja Suunta"

usethis::use_data(energiantuonti, overwrite = TRUE)
