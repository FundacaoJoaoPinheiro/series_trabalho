rodar_grid_semEA <- function(y, grid_semEA, f.modelo_bsm) {
  resultados <- vector("list", length = nrow(grid_semEA))
  erros <- vector("list", length = nrow(grid_semEA))
  
  for (i in 1:nrow(grid_semEA)) {
    cat("Rodando grid_semEA na linha", i, "...
")
    print(grid_semEA[i, ])
    
    resultado <- try(f.modelo_bsm(y, as.numeric(grid_semEA[i, ])), silent = TRUE)
    
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