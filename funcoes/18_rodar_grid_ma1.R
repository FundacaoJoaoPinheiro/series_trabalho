rodar_grid_ma1 <- function(y, grid_ma1, f.estrutural_ma1) {
  resultados <- vector("list", length = nrow(grid_ma1))
  erros <- vector("list", length = nrow(grid_ma1))
  
  for (i in 1:nrow(grid_ma1)) {
    cat("Rodando grid_ma1 na linha", i, "...
")
    print(grid_ma1[i, ])
    
    resultado <- try(f.estrutural_ma1(y, as.numeric(grid_ma1[i, ])), silent = TRUE)
    
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