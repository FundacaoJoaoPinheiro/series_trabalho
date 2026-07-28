calcula_vicio <- function(serie_original, serie_modelo_sem_corr, serie_modelo){
  mrb1 = sum(serie_modelo_sem_corr-serie_original)/sum(serie_original)*100
  mrb2 = sum(serie_modelo-serie_original)/sum(serie_original)*100
  mrb = cbind(mrb1,mrb2)
  return(mrb)
}