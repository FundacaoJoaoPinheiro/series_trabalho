calcula_rrse <- function(se_original, se_modelo_sem_corr, se_modelo){
  rrse1 = mean((se_original - se_modelo_sem_corr) / se_original) * 100
  rrse2 = mean((se_original - se_modelo) / se_original) * 100
  rrse = cbind(rrse1, rrse2)
  return(rrse)
}