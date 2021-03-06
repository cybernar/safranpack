% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/safran2points.R
\name{DT2sf}
\alias{DT2sf}
\title{Transforme grille Safran en grille de points}
\usage{
DT2sf(DT, extr_date)
}
\arguments{
\item{DT}{data.table object with Safran data}

\item{extr_date}{Single character with date (format : "YYYYMMDD")}
}
\value{
sf object
}
\description{
Filtre data.table avec 1 date
et génère 1 objet sf (toute l'étendue des données en entrée c'est à dire la Métropole)
}
\examples{
f_safran <- system.file("extdata", "dumdata_287009.csv.gz", package = "safranpack")
#DT_safran <- fread(f_safran, header = TRUE)
#pts_28700918 <- DT2sf("28700918", DT_safran)
#st_write(pts_28700918, "data_28700918.shp", delete_layer = T)

}
