rodar_grid_arma11 <- function(y, grid_arma11, f.estrutural_arma11) {
  resultados <- vector("list", length = nrow(grid_arma11))
  erros <- vector("list", length = nrow(grid_arma11))
  
  for (i in 1:nrow(grid_arma11)) {
    cat("Rodando grid_arma11 na linha", i, "...
")
    print(grid_arma11[i, ])
    
    resultado <- try(f.estrutural_arma11(y, as.numeric(grid_arma11[i, ])), silent = TRUE)
    
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