#' Energy Import and Export by Country of Origin
#'
#' A sample of energy imports and exports by country of origin from Statistics Finland's archive table.
#'
#' @format ## `energiantuonti`
#' Data frame with 723 rows and 4 columns:
#' \describe{
#'   \item{Alue}{The name of the country or country group}
#'   \item{Suunta}{Import or export}
#'   \item{time}{Quarter}
#'   \item{value}{Value (million euros)}
#'   ...
#' }
#' @source <https://pxdata.stat.fi/PxWeb/pxweb/fi/StatFin/StatFin__ehk/statfin_ehk_pxt_13j9.px/>
"energiantuonti"

#' Population by Postal Code Area
#'
#' A sample of population data by postal code area from Statistics Finland's Paavo statistics table.
#'
#' @format ## `vaesto_postinumeroittain`
#' Data frame with 886 rows and 6 columns:
#' \describe{
#'   \item{geom}{Multipolygon object}
#'   \item{Postinumero}{Postal Code}
#'   \item{Postinumeroalue}{Name of the Postal Code Area}
#'   \item{Alue}{Municipality of the Postal Code Area}
#'   \item{time}{Year}
#'   \item{value}{Population}
#'   ...
#' }
#' @source <https://pxdata.stat.fi/PxWeb/pxweb/fi/Postinumeroalueittainen_avoin_tieto/Postinumeroalueittainen_avoin_tieto__uusin/paavo_pxt_12f7.px/>
"vaesto_postinumeroittain"
