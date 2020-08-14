# safran 2 raster

#' Transforme grille Safran en raster simple
#'
#' Filtre data.table avec 1 variable et 1 date
#' et génère 1 raster (toute l'étendue c'est à dire la Métropole)
#'
#' @param DT data.table object with Safran data
#' @param varname Single character with variable name
#' @param extr_date Single character with date (format : "YYYYMMDD")
#'
#' @return Raster layer
#' @export
#'
#' @examples
#' raster_rr <- DT2rast(DT_safran, "PRELIQ_Q", "20180318")
#' writeRaster(raster_rr, "output_safran/rr_france_20180318.tif")
#'
DT2rast <- function(DT, varname, extr_date) {
  varlist <- c("LAMBX", "LAMBY", varname)
  DTi <- DT[DATE==extr_date, ..varlist]
  dfi <- as.data.frame(DTi)
  dfi$LAMBX <- dfi$LAMBX * 100
  dfi$LAMBY <- dfi$LAMBY * 100
  rasterFromXYZ(as.data.frame(dfi), crs=CRS("+init=EPSG:27572"), digits=0)
}

#' Transforme grille Safran en raster multidates
#'
#' Filtre data.table avec 1 variable, 1 plage temporelle et 1 étendue spatiale
#' et genere 1 stack raster
#'
#' @param DT data.table object with Safran data
#' @param varname Single character with variable name
#' @param d_date Single character with begin date (format : "YYYYMMDD")
#' @param f_date Single character with end date (format : "YYYYMMDD")
#' @param etendue_l2e Extent Lambert2E
#'
#' @return Raster stack
#' @export
#'
#' @examples
#' raster_rr_201803 <- safran2rast(DT_safran, "PRELIQ_Q", "20180301", "20180331")
#' writeRaster(raster_rr_201803, "output_safran/rr_france_201803.tif")
#'
safran2rast <- function(DT, varname, d_date, f_date, etendue_l2e=NULL) {
  varlist <- c("LAMBX", "LAMBY", "DATE", varname)
  seq_dates <- strftime(seq(d_date, f_date, by="days"), "%Y%m%d")
  if (!is.null(etendue_l2e)) {
    # subset etendue Lambert2 en hmDDT
    ethm <- round(etendue_l2e/100, 0)
    DT_filtered <- DT[LAMBX>ethm[1] & LAMBX<ethm[3] & LAMBY>ethm[2] & LAMBY<ethm[4]]
    DT_filtered <- DT[DATE %in% seq_dates,..varlist]
  } else {
    DT_filtered <- DT[DATE %in% seq_dates,..varlist]
  }
  list_raster <- lapply(seq_dates, DT2rast, DT=DT_filtered, varname=varname)
  stack(list_raster)
}


f_test <- function(DT, extr_date) {
  varlist = c("TINF_H_Q", "TSUP_H_Q")
  DTi <- DT[DATE==extr_date, ..varlist]
  dfi <- as.data.frame(DTi)
  dfi
  #dfi$TSUP_H_Q - dfi$TINF_H_Q
}

l
