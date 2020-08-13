# safran2points


# genere grille points à partie de DT(data.table) pour la date spécifiée
DT2sp <- function(extr_date, DT) {
  DTi <- DT[DATE==extr_date]
  pts <- as.data.frame(DTi)
  pts$coordx <- pts$LAMBX * 100
  pts$coordy <- pts$LAMBY * 100
  geomdef <- paste0("POINT(", pts$coordx, " ", pts$coordy,")")
  sfc <- st_as_sfc(geomdef, crs=27572)
  st_geometry(pts) <- sfc
  pts
}

DT2csv <- function(extr_date, DT) {
  DTi <- DT[DATE==extr_date]
  pts <- as.data.frame(DTi)
  pts$coordx <- as.integer(pts$LAMBX * 100)
  pts$coordy <- as.integer(pts$LAMBY * 100)
  geomdef <- paste0("POINT(", pts$coordx, " ", pts$coordy,")")
  pts
}
