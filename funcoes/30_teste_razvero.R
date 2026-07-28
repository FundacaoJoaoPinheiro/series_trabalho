teste_razvero <- function(model_without_corr, model_with_corr){
  # LogLik dos modelos
  D <- -model_without_corr[["fit"]][["value"]]
  B <- -model_with_corr[["fit"]][["value"]]
  
  # Diferença no número de parâmetros
  df <- length(model_with_corr[["fit"]][["par"]]) -
    length(model_without_corr[["fit"]][["par"]])
  
  # Estatística do teste
  teststat <- -2 * (as.numeric(D) - as.numeric(B))
  
  # p-valor
  p.val <- pchisq(teststat, df = df, lower.tail = FALSE)
  
  return(cbind(teststat, df, p.val))
}