## code to prepare datasets for vignettes go here

vignette_data_oecd_gdp <-
  robonomistClient::data_get("oecd/DSD_EO@DF_EO") |>
  dplyr::filter(
    REF_AREA %in% c(
      "Finland",
      "Euro area (17 countries)",
      "United States",
      "United Kingdom",
      "Japan"
    ),
    FREQ == "Quarterly",
    MEASURE == "Gross domestic product, volume, market prices",
    time >= "2017-01-01"
  ) |>
  dplyr::collect() |>
  dplyr::mutate(value = 100 * value / mean(value[lubridate::year(time) == 2019]), .by = REF_AREA) |>
  dplyr::mutate(Country = dplyr::recode(REF_AREA, "Euro area (17 countries)" = "Euro area")) |>
  dplyr::select(Country, time, value)

attr(vignette_data_oecd_gdp, "frequency") <- "Quarterly"

usethis::use_data(vignette_data_oecd_gdp, overwrite = TRUE)

vignette_data_es_fred_inflation <- (function() {
  inflation_eu <-
    robonomistClient::data_get("eurostat/prc_hicp_manr") |>
    dplyr::filter(
      unit == "Annual rate of change",
      coicop == "All-items HICP",
      geo %in% c(
        "Germany (until 1990 former territory of the FRG)",
        "Euro area - 19 countries  (from 2015)",
        "Finland",
        "Sweden"
      ),
      time >= "2015-01-01"
    ) |>
    dplyr::mutate(
      geo = dplyr::recode(
        geo,
        "Euro area - 19 countries  (from 2015)" = "Euro area",
        "Germany (until 1990 former territory of the FRG)" = "Germany"
      )
    ) |>
    dplyr::select(Country = geo, time, value)

  inflation_us <-
    robonomistServer::data_get("fred/CPIAUCSL", units = "pc1") |>
    tidyr::drop_na() |>
    dplyr::filter(time >= "2015-01-01") |>
    dplyr::mutate(Country = "USA", time, value, .keep = "none")

  d <- dplyr::bind_rows(inflation_us, inflation_eu)

  d
})()

usethis::use_data(vignette_data_es_fred_inflation, overwrite = TRUE)


vignette_data_statfin_producer_prices <-
  robonomistClient::data_get("StatFin/thi/statfin_thi_pxt_118g.px",
                             lang = "en",
                             tidy_time = TRUE) |>
  dplyr::filter(
    stringr::str_detect(
      `Products by activity (CPA 2015, MIG)`,
      "^(01|02|03|07|08|10|16|17|19|20|21|22|23|24|25|26|27|28|29|30|35|36|42) "
    ),
    `Index series` %in% c("Producer price index for manufactured products"),
    Information %in% c("Annual change, %")
  ) |>
  dplyr::filter(time == max(time)) |>
  dplyr::rename(CPA = `Products by activity (CPA 2015, MIG)`) |>
  tidyr::drop_na(value) |>
  dplyr::mutate(CPA = forcats::fct_reorder(stringr::str_replace(CPA, "[0-9]{2} ", ""), value))

usethis::use_data(vignette_data_statfin_producer_prices, overwrite = T)

vignette_data_statfin_producer_price_manufacture <-
  robonomistClient::data_get("tidy/thi") |>
  dplyr::filter(
    Indeksisarja == "Teollisuuden tuottajahintaindeksi",
    Tiedot == "Pisteluku (2015=100)",
    Toimiala == "Yhteensä"
  ) |>
  dplyr::mutate(value = 100 * (value / lag(value, 12) - 1), Series = "Producer Price Index for Manufactured Products") |>
  tidyr::drop_na()

usethis::use_data(vignette_data_statfin_producer_price_manufacture,
                  overwrite = T)

vignette_data_fao_food_price_index <-
  robonomistClient::data_get("fao/CP", tidy_time = T) |>
  dplyr::filter(
    stringr::str_detect(Area, "economies"),
    Item == "Consumer Prices, Food Indices (2015 = 100), weighted average"
  ) |>
  dplyr::arrange(time)

usethis::use_data(vignette_data_fao_food_price_index, overwrite = T)

vignette_data_ec_petrol_price <-
  robonomistClient::data_get("tidy/ec_oil_bulletin") |>
  dplyr::filter(
    Country %in% c("Finland", "Sweden", "Denmark"),
    Variable == "Euro-super 95",
    Taxes == T,
    time >= "2018-01-01"
  ) |>
  dplyr::mutate(
    value = value / 1000,
    Country = dplyr::recode(
      Country,
      FI = "Finland",
      DE = "Germany",
      SE = "Sweden"
    )
  )

usethis::use_data(vignette_data_ec_petrol_price, overwrite = T)

vignette_data_entsoe_electricity_price <-
  stringr::str_c("entsoe/dap_", c("FI","DE_LU","SE3","FR")) |>
  purrr::map(robonomistClient::data_get, start_time = "2020-01-01") |>
  dplyr::bind_rows() |>
  dplyr::arrange(time) |>
  dplyr::mutate(ma28 = slider::slide_index_dbl(value, time, mean, .before = lubridate::days(28)),
         .by = Area) |>
  dplyr::group_by(Area, time = lubridate::floor_date(time, "days") |> lubridate::as_date()) |>
  dplyr::summarise(value = mean(ma28), .groups = "drop") |>
  dplyr::arrange(Area, time)

usethis::use_data(vignette_data_entsoe_electricity_price, overwrite = T)

vignette_data_ec_ecomomic_sentiment <-
  robonomistClient::data_get("ec/esi_nace2") |>
  dplyr::filter(
    Country %in%
      c("European Union (current composition)", "Finland", "Germany", "Sweden"),
    Indicator ==
      "The Economic sentiment indicator is a composite measure (average = 100)",
    time >= "2017-01-01"
  ) |>
  dplyr::mutate(
    Country =
      dplyr::recode(Country, "European Union (current composition)" = "EU") |>
      forcats::fct_relevel("EU","Finland"),
    Indicator = "Economic sentiment indicator"
  )

usethis::use_data(vignette_data_ec_ecomomic_sentiment, overwrite = T)

vignette_data_ec_consumer_confidence <-
  robonomistClient::data_get("ec/esi_nace2") |>
  dplyr::filter(Country %in% c("European Union (current composition)", "Finland"),
         Indicator == "Consumer confidence indicator (20%)",
         time >= "1996-01-01"
  ) |>
  dplyr::mutate(
    Country = dplyr::recode(Country, "European Union (current composition)" = "EU") |>
      forcats::fct_relevel("EU","Finland")
  )

usethis::use_data(vignette_data_ec_consumer_confidence, overwrite = T)

vignette_data_bof_euribor <-
  robonomistClient::data_get("tidy/euribor") |>
  dplyr::filter(stringr::str_detect(Korko, "(1|6|12) kk.*360")) |>
  dplyr::mutate(Korko = stringr::str_remove(Korko,"\\(.*") |> forcats::fct_inorder()) |>
  dplyr::filter(time >= "2014-01-01") |>
  dplyr::select(Interest = Korko, time, value) |>
  dplyr::mutate(Interest = stringr::str_replace(Interest, " kk", "m"))

usethis::use_data(vignette_data_bof_euribor, overwrite = T)

vignette_data_bof_db_fed_yields <-
  robonomistClient::data_get("tidy/10yield") |>
  tidyr::drop_na() |>
  dplyr::filter(time >= "2012-01-01") |>
  dplyr::select(Country = Maa, time, value) |>
  dplyr::mutate(Country = dplyr::recode(Country, "Suomi" = "Finland", "Saksa" = "Germany") |> as.factor())

usethis::use_data(vignette_data_bof_db_fed_yields, overwrite = T)

vignette_data_ecb_yield_curves <-
  robonomistClient::data_get("ecb/YC",
                                           dl_filter = "B.U2.EUR.4F.G_N_A.SV_C_YM.",
                                           query_args = list(startPeriod = "2022-01-01"),
                                           labels = FALSE
) |>
  dplyr::filter(`Financial market data type` |> stringr::str_starts("SR_")) |>
  dplyr::filter(
    time %in% c(max(time), min(time), as.Date(c("2022-02-28","2022-06-01")))
  ) |>
  dplyr::mutate(
    Year = stringr::str_match(`Financial market data type`, "([[:digit:]]*)Y")[,2],
    Month = stringr::str_match(`Financial market data type`, "([[:digit:]]*)M")[, 2]
  ) |>
  dplyr::mutate(across(c(Year, Month), as.integer)) |>
  tidyr::replace_na(list(Year = 0, Month = 0)) |>
  dplyr::mutate(Maturity = Year + Month / 12) |>
  dplyr::select(Maturity, time, value) |>
  dplyr::mutate(Time = as.character(time)) |>
  dplyr::arrange(Maturity, value)

usethis::use_data(vignette_data_ecb_yield_curves, overwrite = T)


vignette_data_es_unemployment <-
  robonomistClient::data_get("eurostat/une_rt_m") |>
  dplyr::filter(s_adj == "Seasonally adjusted data, not calendar adjusted data",
         age == "Total",
         unit == "Percentage of population in the labour force",
         sex == "Total",
         stringr::str_detect(geo, "Fin|Europ|United S|Jap|Swe"),
         time >= "2019-01-01") |>
  dplyr::mutate(
    geo = dplyr::recode(geo, "European Union - 27 countries (from 2020)" = "EU")) |>
  dplyr::select(Area = geo, time, value)

list.files("data",full.names = T) |>
  str_subset("vignette_data") |>
  file.copy(to = "inst/extdata", overwrite = T)

list.files("data",full.names = T) |>
  str_subset("vignette_data") |>
  file.remove()
