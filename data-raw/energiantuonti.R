## code to prepare `energiantuonti` dataset

library(tidyverse)
library(robonomistServer)

energiantuonti <- robonomistServer::data_get("StatFin_Passiivi/ene/ehk/statfinpas_ehk_pxt_004_202100_fi.px") %>%
  filter(Tiedot == "Arvo, M€", Maa %in% c("Venäjä","OECD-maat","EU-maat","Ruotsi","Alankomaat","Norja","Yhdysvallat (USA)","Yhdistynyt kuningaskunta","Belgia","Kanada" ),
         `Tuonti/Vienti` %in% c("Tuonti","Vienti"),
         str_detect(Kausi, "^[0-9]\\.\\s"),
         Tuote == "ENERGIA YHTEENSÄ", value > 0) |>
  mutate(time = glue::glue("{Vuosi}Q{str_sub(Kausi,1,1)}") |>
           lubridate::yq()) %>%
  select(Alue = Maa, Suunta = `Tuonti/Vienti`, time, value)

attr(energiantuonti, "robonomist_title") <- "roboplotr-paketin testamiseen energian tuonti ja vienti alkuperämaittain (suurimmat maat), muuttujina Alue ja Suunta"

usethis::use_data(energiantuonti, overwrite = TRUE)
