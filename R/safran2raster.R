# safran 2 raster


# ##### FONCTIONS #####

# genere 1 raster à partir de DT(data.table) pour la variable et la date spécifiées
#' Title
#'
#' @param DT data.table object with Safran data
#' @param varname Single character with variable name
#' @param extr_date Single character with date (format : "YYYYMMDD")
#'
#' @return Raster layer
#' @export
#'
#' @examples
#' DT2rast(DT, "", "")
#'
DT2rast <- function(DT, varname, extr_date) {
  varlist <- c("LAMBX", "LAMBY", varname)
  DTi <- DT[DATE==extr_date, ..varlist]
  dfi <- as.data.frame(DTi)
  dfi$LAMBX <- dfi$LAMBX * 100
  dfi$LAMBY <- dfi$LAMBY * 100
  rasterFromXYZ(as.data.frame(dfi), crs=CRS("+init=EPSG:27572"), digits=0)
}

# genere 1 stack raster à partir de DT(data.table)
# pour la variable, la plage temporelle et l'étendue spécifiées
safran2rast <- function(DT, varname, d_date, f_date, etendue_l2e=NULL) {
  varlist <- c("LAMBX", "LAMBY", "DATE", varname)
  seq_dates <- strftime(seq(d_date, f_date, by="days"), "%Y%m%d")
  if (!is.null(etendue_l2e)) {
    # subset etendue Lambert2 en hm
    ethm <- round(etendue_l2e/100, 0)
    DT <- DT[LAMBX>ethm[1] & LAMBX<ethm[3] & LAMBY>ethm[2] & LAMBY<ethm[4]]
    DT <- DT[DATE %in% seq_dates,..varlist]
  } else {
    DT <- DT[DATE %in% seq_dates,..varlist]
  }
  list_raster <- lapply(seq_dates, DT2rast, DT, varname)
  stack(list_raster)
}

