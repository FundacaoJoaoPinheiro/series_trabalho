rodar_grid_ma2 <- function(y, grid_ma2, f.estrutural_ma2) {
  resultados <- vector("list", length = nrow(grid_ma2))
  erros <- vector("list", length = nrow(grid_ma2))
  
  for (i in 1:nrow(grid_ma2)) {
    cat("Rodando grid_ma2 na linha", i, "...
")
    print(grid_ma2[i, ])
    
    resultado <- try(f.estrutural_ma2(y, as.numeric(grid_ma2[i, ])), silent = TRUE)
    
    if (inherits(resultado, "try-error")) {
      cat("Erro detectado na linha", i, "
")
      erros[[i]] <- resultado 
      resultados[[i]] <- NA
    } else {
      cat("Rodou sem erro na linha", i, "
")
      resultados[[i]] <- resultado  
    }
  }
  
  cat("
Resumo das execuções com erro:
")
  for (i in which(sapply(erros, is.character))) {
    cat("Linha", i, "erro:
", erros[[i]], "

")
  }
  
  return(list(resultados = resultados, erros = erros))
}