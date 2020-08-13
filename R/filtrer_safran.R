library(data.table)

# ##### FONCTIONS #####
safran_filtre <- function(DT, lamb_x, lamb_y, date_d, date_f) {
  seq_dates <- strftime(seq(date_d, date_f, by="days"), "%Y%m%d")
  DT[DATE %in% seq_dates & LAMBX==lamb_x & LAMBY==lamb_y]
  #DT2[, CodePar:=code_parc]
}

