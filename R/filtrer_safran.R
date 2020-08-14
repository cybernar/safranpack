

#' Filtrer données Safran par lieu (XY) et plage temporelle
#'
#' @param DT data.table object with Safran data
#' @param lamb_x X lambert 2 etendu
#' @param lamb_y Y lambert 2 etendu
#' @param date_d Single character with begin date (format : "YYYYMMDD")
#' @param date_f Single character with end date (format : "YYYYMMDD")
#'
#' @return data.table object with Safran data
#' @export
#'
#' @examples
#' DT_XY <- safran_filtre(DT, "640", "2200", "20180301", "20180331")
#'
safran_filtre <- function(DT, lamb_x, lamb_y, date_d, date_f) {
  seq_dates <- strftime(seq(date_d, date_f, by="days"), "%Y%m%d")
  DT[DATE %in% seq_dates & LAMBX==lamb_x & LAMBY==lamb_y]
  #DT2[, CodePar:=code_parc]
}
