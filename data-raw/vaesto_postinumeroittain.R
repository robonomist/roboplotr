## code to prepare `nettomuutto_postinumeroalueittain` dataset

vaesto_postinumeroittain <- robonomistServer::data_get("paavo/uusin/paavo_pxt_12f7.px", tidy_time = T) |>
  dplyr::filter(Tiedot == "Asukkaat yhteensä (HE)") |>
  dplyr::filter(stringr::str_detect(Postinumeroalue, "[0-9]"), time == max(time)) |>
  dplyr::mutate(
    Postinumero = stringr::str_extract(Postinumeroalue, "[0-9]{5}"),
    Alue = stringr::str_extract(Postinumeroalue, "[^\\(]{1,}(?=\\)$)"),
    Postinumeroalue = stringr::str_remove_all(Postinumeroalue, c("^[0-9]*\\s*| \\([^\\(]{1,}$")),
    Postinumeroalue = stringr::str_glue("{Postinumeroalue} ({Postinumero} {Alue})")
  ) |>
  dplyr::select(Postinumero, Postinumeroalue, Alue, time, value) |>
  dplyr::filter(stringr::str_detect(Postinumero, "^[0]"), time == max(time))

vaesto_postinumeroittain <- geofi::get_zipcodes() |>
  dplyr::select(Postinumero = posti_alue, geom) |>
  dplyr::filter(stringr::str_detect(Postinumero, "^[0]")) |>
  dplyr::left_join(dplyr::distinct(vaesto_postinumeroittain, Postinumero, Alue, Postinumeroalue, time, value), by = "Postinumero") |>
  dplyr::mutate(value = tidyr::replace_na(value, 0)) |>
  dplyr::group_by(geom, Postinumero, Postinumeroalue, Alue, time) |>
  dplyr::summarize(value = sum(value), .groups = "drop") |>
  sf::st_transform(4326)

attr(vaesto_postinumeroittain, "robonomist_title") <- "Roboplotr-paketin testaamiseen väestö postinumeroalueittain tietyiltä alueilta muuttujina Postinumeroalue, Alue, geom, time ja value"

usethis::use_data(vaesto_postinumeroittain, overwrite = TRUE,compress = "xz")
