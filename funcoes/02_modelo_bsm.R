# Definição do modelo em função
f.modelo_bsm<-function(y,i0){
  modelo<- list("fn"=function(params){
    m = dlmModPoly(2) + dlmModTrig(4)
    # m = dlmModPoly(2) + dlmModSeas(4)
    W = matrix(0,5,5)
    W[1, 1] <- exp(params[1])
    W[2, 2] <- exp(params[2])
    W[3, 3] <- exp(params[3])
    m$W <- W
    # m$X <- se_db
    V =  exp(params[4])
    m$V <- V
    return(m)
  })
  
  modelo$initial<-i0
  
  # estima os hiperparâmetros
  modelo$fit <- dlmMLE(y, modelo$initial,modelo$fn, hessian=T,control = list(maxit = 10^8)) # valor inicial está em 0, porem exp(0)=1
  #method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN","Brent")
  # reproduz o modelo em matrizes
  modelo$mod <- modelo$fn(modelo$fit$par)
  
  # aplica o filtro de Kalman para obter séries filtradas e suavizadas
  modelo$filtered <- dlmFilter(y,modelo$mod)
  modelo$smoothed <- dlmSmooth(modelo$filtered)
  modelo$m <- dropFirst(modelo$filtered$m)
  modelo$sm <- dropFirst(modelo$smoothed$s)
  modelo$res <- residuals(modelo$filtered,sd=FALSE)
  
  # definição de variável
  modelo$d<-length(modelo$mod$m0)+1
  modelo$T<-length(y)
  
  # estatísticas de interesse
  modelo$ts.original<- y
  modelo$ts.trend <- modelo[["m"]][,1]
  modelo$ts.slope <- modelo[["m"]][,2]
  modelo$ts.seasonal <- modelo[["m"]][,3]+modelo[["m"]][,5]
  # modelo$ts.seasonal <- modelo[["m"]][,3]
  modelo$ts.signal <- modelo$ts.trend +modelo$ts.seasonal
  modelo$ts.irregular <- modelo$ts.original-(modelo$ts.signal)
  modelo$ts.seasonal_adj <- modelo$ts.trend+modelo$ts.irregular 
  
  # calculo erro padrão
  mse.list_bsm = dlmSvd2var(modelo[["filtered"]][["U.C"]], modelo[["filtered"]][["D.C"]])
  se.mat_bsm = dropFirst(t(sapply(mse.list_bsm, FUN=function(x) sqrt(diag(x)))))
  
  # cria vertores indicadores para soma de estados
  c_sinal_bsm <- matrix(c(1,0,1,0, # somando tendência e os seis componentes sazonais
                                1),1,5) # indica com 1 qual a coluna(estado) quer considerar*caso trigonométrico
  c_seasonal_bsm <- matrix(c(0,0,1,0, # somando os seis componentes sazonais
                                   1),1,5) # indica com 1 qual a coluna(estado) quer considerar*caso trigonométrico
  # c_sinal_bsm <- matrix(c(1,0,1,1, # somando tendência e os seis componentes sazonais
  #                       1,0),1,6) # indica com 1 qual a coluna(estado) quer considerar*caso normal
  # c_seasonal_bsm <- matrix(c(0,0,1,1, # somando os seis componentes sazonais
  #                          1,0),1,6) # indica com 1 qual a coluna(estado) quer considerar*caso normal
  
  se.mat_bsm_sinal = dropFirst((sapply(mse.list_bsm, function(i) sqrt(c_sinal_bsm%*%i%*%t(c_sinal_bsm)) )))
  se.mat_bsm_seasonal = dropFirst((sapply(mse.list_bsm, function(i) sqrt(c_seasonal_bsm%*%i%*%t(c_seasonal_bsm)) )))
  
  modelo$se.original<-cv_db/100*modelo$ts.original
  modelo$se.trend <- se.mat_bsm[,1]
  modelo$se.slope <- se.mat_bsm[,2]
  modelo$se.seasonal <- se.mat_bsm_seasonal
  modelo$se.signal <- se.mat_bsm_sinal
  # modelo$se.irregular
  # modelo$se.seasonal_adj
  
  modelo$cv.original<-cv_db
  modelo$cv.trend<- modelo$se.trend/modelo$ts.trend*100
  # modelo$cv.seasonal<- modelo$se.seasonal/modelo$ts.seasonal*100
  modelo$cv.signal<- modelo$se.signal/modelo$ts.signal*100
  # modelo$cv.irregular
  # modelo$cv.seasonal_adj
  
  # estatísticas de interesse - suavizadas
  modelo$ts.sm.trend <- modelo[["sm"]][,1]
  modelo$ts.sm.slope <- modelo[["sm"]][,2]
  modelo$ts.sm.seasonal <- modelo[["sm"]][,3]+modelo[["sm"]][,5]
  # modelo$ts.sm.seasonal <- modelo[["sm"]][,3]
  modelo$ts.sm.signal <- modelo$ts.trend +modelo$ts.seasonal
  modelo$ts.sm.irregular <- modelo$ts.original-(modelo$ts.signal)
  modelo$ts.sm.seasonal_adj <- modelo$ts.trend+modelo$ts.irregular 
  
  # calculo erro padrão
  mse.list_bsm.sm = dlmSvd2var(modelo[["smoothed"]][["U.S"]], modelo[["smoothed"]][["D.S"]])
  se.mat_bsm.sm = dropFirst(t(sapply(mse.list_bsm.sm, FUN=function(x) sqrt(diag(x)))))
  
  # cria vertores indicadores para soma de estados
   c_sinal_bsm.sm <- matrix(c(1,0,1,0, # somando tendência e os seis componentes sazonais
                         1,0),1,6) # indica com 1 qual a coluna(estado) quer considerar*caso trigonométrico
   c_seasonal_bsm.sm <- matrix(c(0,0,1,0, # somando os seis componentes sazonais
                            1,0),1,6) # indica com 1 qual a coluna(estado) quer considerar*caso trigonométrico
  #c_sinal_bsm.sm <- matrix(c(1,0,1,1, # somando tendência e os seis componentes sazonais
  #                                 1),1,5) # indica com 1 qual a coluna(estado) quer considerar*caso normal
  #c_seasonal_bsm.sm <- matrix(c(0,0,1,1, # somando os seis componentes sazonais
  #                                    1),1,5) # indica com 1 qual a coluna(estado) quer considerar*caso normal
  
  se.mat_bsm_sinal.sm = dropFirst((sapply(mse.list_bsm.sm, function(i) sqrt(c_sinal_bsm.sm%*%i%*%t(c_sinal_bsm.sm)) )))
  se.mat_bsm_seasonal.sm = dropFirst((sapply(mse.list_bsm.sm, function(i) sqrt(c_seasonal_bsm.sm%*%i%*%t(c_seasonal_bsm.sm)) )))
  
  modelo$se.sm.trend <- se.mat_bsm.sm[,1]
  modelo$se.sm.slope <- se.mat_bsm.sm[,2]
  modelo$se.sm.seasonal <- se.mat_bsm_seasonal.sm
  modelo$se.sm.signal <- se.mat_bsm_sinal.sm
  
  modelo$cv.sm.trend<- modelo$se.sm.trend/modelo$ts.sm.trend*100
  modelo$cv.sm.slope<- modelo$se.sm.slope/modelo$ts.sm.slope*100
  modelo$cv.sm.signal<- modelo$se.sm.signal/modelo$ts.sm.signal*100
  
  return(modelo)
}
