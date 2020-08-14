# safran2points


#' Transforme grille Safran en grille de points
#'
#' Filtre data.table avec 1 date
#' et génère 1 objet sf (toute l'étendue des données en entrée c'est à dire la Métropole)
#'
#' @param DT data.table object with Safran data
#' @param extr_date Single character with date (format : "YYYYMMDD")
#'
#' @return sf object
#' @import data.table sf
#' @export
#'
#' @examples
#' f_safran <- system.file("extdata", "dumdata_287009.csv.gz", package = "safranpack")
#' #DT_safran <- fread(f_safran, header = TRUE)
#' #pts_28700918 <- DT2sf("28700918", DT_safran)
#' #st_write(pts_28700918, "data_28700918.shp", delete_layer = T)
#'
DT2sf <- function(DT, extr_date) {
  DTi <- DT[DATE==extr_date]
  pts <- as.data.frame(DTi)
  pts$coordx <- pts$LAMBX * 100
  pts$coordy <- pts$LAMBY * 100
  geomdef <- paste0("POINT(", pts$coordx, " ", pts$coordy,")")
  sfc <- st_as_sfc(geomdef, crs=27572)
  st_geometry(pts) <- sfc
  pts
}

#' Transforme grille Safran en grille de points
#'
#' Filtre data.table avec 1 date
#' et génère 1 data.frame avec 2 colonnes coordx et coordy
#'
#' @param DT data.table object with Safran data
#' @param extr_date Single character with date (format : "YYYYMMDD")
#'
#' @return data.frame
#' @export
#'
#' @examples
#' f_safran <- system.file("extdata", "dumdata_287009.csv.gz", package = "safranpack")
#' #DT_safran <- fread(f_safran, header = TRUE)
#' #pts_28700918 <- DT2sf("28700918", DT_safran)
#' #st_write(pts_28700918, "data_28700918.shp", delete_layer = T)
#'
DT2csv <- function(DT, extr_date) {
  DTi <- DT[DATE==extr_date]
  pts <- as.data.frame(DTi)
  pts$coordx <- as.integer(pts$LAMBX * 100)
  pts$coordy <- as.integer(pts$LAMBY * 100)
  #geomdef <- paste0("POINT(", pts$coordx, " ", pts$coordy,")")
  pts
}
