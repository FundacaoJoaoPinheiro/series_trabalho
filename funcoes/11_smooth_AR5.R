# Definição do modelo em função
f.smooth_ar5<-function(y,i0){
  modelo<- list("fn"=function(params){
    m <- dlmModPoly(2) + dlmModTrig(4) + dlmModReg(se_db, addInt = FALSE)
    m$FF <- cbind(m$FF, matrix(0, nrow = 1, ncol = 4))
    m$GG <- rbind(m$GG, matrix(0, nrow = 4, ncol = ncol(m$GG)))
    m$GG <- cbind(m$GG, matrix(0, nrow = nrow(m$GG), ncol = 4)) 
    m$GG[6,6] <- phi1_ar5
    m$GG[6,7] <- phi2_ar5       
    m$GG[6,8] <- phi3_ar5
    m$GG[6,9] <- phi4_ar5
    m$GG[6,10] <- phi5_ar5
    m$GG[7,6] <- 1
    m$GG[8,7] <- 1
    m$GG[9,8] <- 1
    m$GG[10,9] <- 1
    
    # Expande a matriz W para acomodar o componente MA(1)
    W <- matrix(0, 10, 10)
    W[1,1] <- 10^(-6) 
    W[2,2] <- exp(params[1])
    W[3,3] <- exp(params[2])
    W[6,6] <- exp(params[4]) # Variância do e_t
    m$W <- W
    
    V <- exp(params[3])
    m$V <- V
    m$m0<- rep(0,10)
    m$C0<- diag(x=10^7,10)
    return(m)
  })
  
  modelo$initial <- c(i0)
  
  # Estima os hiperparâmetros
  modelo$fit <- dlmMLE(y, modelo$initial, modelo$fn, hessian = T, control = list(maxit = 10^8),debug=TRUE)
  
  # reproduz o modelo em matrizes
  modelo$mod <- modelo$fn(modelo$fit$par)
  
  # aplica o filtro de Kalman para obter séries filtradas e suavizadas
  modelo$filtered <- dlmFilter(y, modelo$mod)
  modelo$smoothed <- dlmSmooth(modelo$filtered)
  modelo$m <- dropFirst(modelo$filtered$m)
  modelo$sm <- dropFirst(modelo$smoothed$s)
  modelo$res <- residuals(modelo$filtered, sd = FALSE)
  
  # definição de variável
  modelo$d <- length(modelo$mod$m0) + 1
  modelo$T <- length(y)
  
  # estatísticas de interesse
  modelo$ts.original <- y
  modelo$ts.trend <- modelo[["m"]][, 1]
  modelo$ts.slope <- modelo[["m"]][, 2]
  modelo$ts.seasonal <- modelo[["m"]][, 3] + modelo[["m"]][, 5]
  modelo$ts.sampling_error <- modelo[["m"]][, 6]
  modelo$ts.signal <- modelo$ts.trend + modelo$ts.seasonal
  modelo$ts.irregular <- modelo$ts.original - (modelo$ts.signal + modelo$ts.sampling_error)
  modelo$ts.seasonal_adj <- modelo$ts.trend + modelo$ts.irregular
  
  # calculo erro padrão
  mse.list_bsm_error <- dlmSvd2var(modelo[["filtered"]][["U.C"]], modelo[["filtered"]][["D.C"]])
  se.mat_bsm_error <- dropFirst(t(sapply(mse.list_bsm_error, FUN = function(x) sqrt(diag(x)))))
  
  # cria vetores indicadores para soma de estados
  
  c_sinal_bsm_error <- matrix(c(1,0,1,0,1,0,0,0,0,0),1,10)
  c_seasonal_bsm_error <- matrix(c(0,0,1,0,1,0,0,0,0,0),1,10)
  
  se.mat_bsm_error_sinal <- dropFirst((sapply(mse.list_bsm_error, function(i) sqrt(c_sinal_bsm_error %*% i %*% t(c_sinal_bsm_error)))))
                                      se.mat_bsm_error_seasonal <- dropFirst((sapply(mse.list_bsm_error, function(i) sqrt(c_seasonal_bsm_error %*% i %*% t(c_seasonal_bsm_error)))))
  
  modelo$se.original <- cv_db / 100 * modelo$ts.original
  modelo$se.trend <- se.mat_bsm_error[,1]
  modelo$se.slope <- se.mat_bsm_error[,2]
  modelo$se.seasonal <- se.mat_bsm_error_seasonal
  modelo$se.sampling_error <- se.mat_bsm_error[,6] # Mudar para 6 e 7?
  modelo$se.signal <- se.mat_bsm_error_sinal
  
  modelo$cv.original <- cv_db
  modelo$cv.trend <- modelo$se.trend / modelo$ts.trend * 100
  modelo$cv.signal <- modelo$se.signal / modelo$ts.signal * 100
  
  # estatísticas de interesse - suavizadas
  modelo$ts.sm.trend <- modelo[["sm"]][,1]
  modelo$ts.sm.slope <- modelo[["sm"]][,2]
  modelo$ts.sm.seasonal <- modelo[["sm"]][,3] + modelo[["sm"]][,5]
  modelo$ts.sm.sampling_error <- modelo[["sm"]][,6]
  modelo$ts.sm.signal <- modelo$ts.sm.trend + modelo$ts.sm.seasonal
  modelo$ts.sm.irregular <- modelo$ts.original - (modelo$ts.sm.signal + modelo$ts.sm.sampling_error)
  modelo$ts.sm.seasonal_adj <- modelo$ts.sm.trend + modelo$ts.sm.irregular
  
  
  # calculo erro padrão
  mse.list_bsm_error.sm <- dlmSvd2var(modelo[["smoothed"]][["U.S"]], modelo[["smoothed"]][["D.S"]])
  se.mat_bsm_error.sm <- dropFirst(t(sapply(mse.list_bsm_error.sm, FUN = function(x) sqrt(diag(x)))))
  
  # cria vetores indicadores para soma de estados
  c_sinal_bsm_error.sm <- matrix(c(1,0,1,0,1,0,0,0,0,0),1,10)
  c_seasonal_bsm_error.sm <- matrix(c(0,0,1,0,1,0,0,0,0,0),1,10)
  
  se.mat_bsm_error_sinal.sm <- dropFirst((sapply(mse.list_bsm_error.sm, function(i) sqrt(c_sinal_bsm_error.sm %*% i %*% t(c_sinal_bsm_error.sm)))))
                                         se.mat_bsm_error_seasonal.sm <- dropFirst((sapply(mse.list_bsm_error.sm, function(i) sqrt(c_seasonal_bsm_error.sm %*% i %*% t(c_seasonal_bsm_error.sm)))))
  
  se.mat_bsm_error_sinal.sm = dropFirst((sapply(mse.list_bsm_error.sm, function(i) sqrt(c_sinal_bsm_error.sm%*%i%*%t(c_sinal_bsm_error.sm)))))
  se.mat_bsm_error_seasonal.sm = dropFirst((sapply(mse.list_bsm_error.sm, function(i) sqrt(c_seasonal_bsm_error.sm%*%i%*%t(c_seasonal_bsm_error.sm)))))
  
  modelo$se.sm.trend <- se.mat_bsm_error.sm[,1]
  modelo$se.sm.slope <- se.mat_bsm_error.sm[,2]
  modelo$se.sm.seasonal <- se.mat_bsm_error_seasonal.sm
  modelo$se.sm.sampling_error <- se.mat_bsm_error.sm[,6] # para 6 e 7?
  modelo$se.sm.signal <- se.mat_bsm_error_sinal.sm
  
  modelo$cv.sm.trend <- modelo$se.sm.trend / modelo$ts.sm.trend * 100
  modelo$cv.sm.slope <- modelo$se.sm.slope / modelo$ts.sm.slope * 100
  modelo$cv.sm.signal <- modelo$se.sm.signal / modelo$ts.sm.signal * 100
  
  return(modelo)
}
