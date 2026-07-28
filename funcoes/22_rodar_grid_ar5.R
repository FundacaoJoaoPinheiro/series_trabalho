rodar_grid_ar5 <- function(y, grid_ar5, f.estrutural_ar5) {
  resultados <- vector("list", length = nrow(grid_ar5))
  erros <- vector("list", length = nrow(grid_ar5))
  
  for (i in 1:nrow(grid_ar5)) {
    cat("Rodando grid_ar5 na linha", i, "...
")
    print(grid_ar5[i, ])
    
    resultado <- try(f.estrutural_ar5(y, as.numeric(grid_ar5[i, ])), silent = TRUE)
    
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