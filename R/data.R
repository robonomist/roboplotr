#' Energian tuonti ja vienti alkuperämaittain
#'
#' Otos energian tuonnista ja viennistä alkuperämaittain Tilastokeskuksen arkistotaulusta.
#'
#' @format ## `energiantuonti`
#' Kehys jossa 925 riviä and 4 saraketta:
#' \describe{
#'   \item{Alue}{Maan tai maaryhmän nimi}
#'   \item{Suunta}{Tuonti tai vienti}
#'   \item{time}{Vuosineljännes}
#'   \item{value}{Arvo (miljoonaa euroa)}
#'   ...
#' }
#' @source <https://statfin.stat.fi/PxWeb/pxweb/fi/StatFin_Passiivi/StatFin_Passiivi__ene__ehk/statfinpas_ehk_pxt_004_202100_fi.px/>
"energiantuonti"


#' Energian tuonti ja vienti alkuperämaittain
#'
#' Otos postinumeroittaisesta väkiluvusta Tilastokeskuksen Paavo-tilastotaulusta.
#'
#' @format ## `vaesto_postinumeroittain`
#' Kehys jossa 886 riviä and 5 saraketta:
#' \describe{
#'   \item{Postinumeroalue}{Postinumeroalueen nimi}
#'   \item{Alue}{Postinumeroalueen kunta}
#'   \item{geom}{Monikulmiokohde}
#'   \item{time}{Vuosi}
#'   \item{value}{Väkiluku}
#'   ...
#' }
#' @source <https://pxdata.stat.fi/PxWeb/pxweb/fi/Postinumeroalueittainen_avoin_tieto/Postinumeroalueittainen_avoin_tieto__uusin/paavo_pxt_12f7.px/>
"vaesto_postinumeroittain"
