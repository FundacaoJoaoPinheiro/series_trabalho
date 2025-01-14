################################################################################
##                          MODELO BSM UNIVARIADO                             ##
################################################################################

library(dlm)
library(tidyverse)
library(beepr)
library(parallel)

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/02_modelo_bsm.R")
source("data/funcoes/03_modelo_bsm_error.R")
source("data/funcoes/04_modelo_bsm_error_1.R")
source("data/funcoes/05_teste_H.R")



### BELO HORIZONTE - OCUPADA ###################################################

  #### TESTE INICIAL - MODELO "POR EXTENSO"

## Carregando bases e definindo objeto para BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
bh<-baseestr0324$`01-Belo Horizonte`
baserot <- readRDS("D:/FJP2425/Programacao/data/baserot0324.RDS")
dtbh<-baserot$`01-Belo Horizonte` ## Arquivo "cru", saída direta da rotina da base por rotação
dbbh<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/01_params_bh.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- bh$Total.de.ocupados
se_db <- bh$sd_o
cv_db <- bh$CV.ocupados
par_ar_erro <- dbbh$parerro_o
plot(y, type="l")
plot(cv_db*100, type="l")


## Estipulando parâmetros iniciais:
  # Conforme recomendação, estipulando inicialmente seq(1)
    # Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)


## Possibilidades do modelo

grid <- expand.grid(par_1,par_2,par_3)
grid_error <- expand.grid(par_1,par_2,par_3,par_4)


## Início do estudo da função

# Definindo o modelo

modelo<- list("fn"=function(params){
  m = dlmModPoly(2) + dlmModTrig(4)
  W = matrix(0,5,5)
  W[1, 1] <- exp(params[1])
  W[2, 2] <- exp(params[2])
  W[3, 3] <- exp(params[3])
  m$W <- W
  V =  exp(params[4])
  m$V <- V
  return(m)
})

i0 <- as.numeric(grid_error[1, ])

modelo$initial<-i0

# Estimando hiperparâmetros:

modelo$fit <- dlmMLE(y, modelo$initial,modelo$fn, hessian=T,control = list(maxit = 10^8))

modelo$mod <- modelo$fn(modelo$fit$par)

# Aplicando o Filtro de Kalman:
  # Séries filtradas e suavizadas:

modelo$filtered <- dlmFilter(y,modelo$mod)
modelo$smoothed <- dlmSmooth(modelo$filtered)
modelo$m <- dropFirst(modelo$filtered$m)
modelo$sm <- dropFirst(modelo$smoothed$s)
modelo$res <- residuals(modelo$filtered,sd=FALSE)

# Definindo variável:

modelo$d<-length(modelo$mod$m0)+1
modelo$T<-length(y)

# Estatísticas de interesse:

modelo$ts.original<- y
modelo$ts.trend <- modelo[["m"]][,1]
modelo$ts.slope <- modelo[["m"]][,2]

modelo$ts.seasonal <- modelo[["m"]][,3]+modelo[["m"]][,4]+modelo[["m"]][,5]
modelo$ts.signal <- modelo$ts.trend +modelo$ts.seasonal
modelo$ts.irregular <- modelo$ts.original-(modelo$ts.signal)
modelo$ts.seasonal_adj <- modelo$ts.trend+modelo$ts.irregular

# Calculando Erro Padrão:

mse.list_bsm = dlmSvd2var(modelo[["filtered"]][["U.C"]], modelo[["filtered"]][["D.C"]])
se.mat_bsm = dropFirst(t(sapply(mse.list_bsm, FUN=function(x) sqrt(diag(x)))))

# Vetores indicadores da soma de estados:

c_sinal_bsm <- matrix(c(1,0,1,0, # somando tendência e os seis componentes sazonais
                        1),1,5) # indica com 1 qual a coluna(estado) quer considerar*caso trigonométrico
c_seasonal_bsm <- matrix(c(0,0,1,0, # somando os seis componentes sazonais
                           1),1,5) # indica com 1 qual a coluna(estado) quer considerar*caso trigonométrico

se.mat_bsm_sinal = dropFirst((sapply(mse.list_bsm, function(i) sqrt(c_sinal_bsm%*%i%*%t(c_sinal_bsm)) )))
se.mat_bsm_seasonal = dropFirst((sapply(mse.list_bsm, function(i) sqrt(c_seasonal_bsm%*%i%*%t(c_seasonal_bsm)) )))

modelo$se.original<-bh$sd_o
modelo$se.trend <- se.mat_bsm[,1]
modelo$se.slope <- se.mat_bsm[,2]
modelo$se.seasonal <- se.mat_bsm_seasonal
modelo$se.signal <- se.mat_bsm_sinal

modelo$cv.original<-bh$CV.ocupados
modelo$cv.trend<- modelo$se.trend/modelo$ts.trend*100
modelo$cv.signal<- modelo$se.signal/modelo$ts.signal*100

# Estatísticas de interesse -> suavizadas

modelo$ts.sm.trend <- modelo[["sm"]][,1]
modelo$ts.sm.slope <- modelo[["sm"]][,2]
modelo$ts.sm.seasonal <- modelo[["sm"]][,3]+modelo[["sm"]][,5]
modelo$ts.sm.signal <- modelo$ts.trend +modelo$ts.seasonal
modelo$ts.sm.irregular <- modelo$ts.original-(modelo$ts.signal)
modelo$ts.sm.seasonal_adj <- modelo$ts.trend+modelo$ts.irregular 

# Calculando o erro padrão

mse.list_bsm.sm = dlmSvd2var(modelo[["smoothed"]][["U.S"]], modelo[["smoothed"]][["D.S"]])
se.mat_bsm.sm = dropFirst(t(sapply(mse.list_bsm.sm, FUN=function(x) sqrt(diag(x)))))

# Mais vetores indicadores necessários para o state space

c_sinal_bsm.sm <- matrix(c(1,0,1,1, 
                           1),1,5) 
c_seasonal_bsm.sm <- matrix(c(0,0,1,1, 
                              1),1,5) 

se.mat_bsm_sinal.sm = dropFirst((sapply(mse.list_bsm.sm, function(i) sqrt(c_sinal_bsm.sm%*%i%*%t(c_sinal_bsm.sm)) )))
se.mat_bsm_seasonal.sm = dropFirst((sapply(mse.list_bsm.sm, function(i) sqrt(c_seasonal_bsm.sm%*%i%*%t(c_seasonal_bsm.sm)) )))

modelo$se.sm.trend <- se.mat_bsm.sm[,1]
modelo$se.sm.slope <- se.mat_bsm.sm[,2]
modelo$se.sm.seasonal <- se.mat_bsm_seasonal.sm
modelo$se.sm.signal <- se.mat_bsm_sinal.sm

modelo$cv.sm.trend<- modelo$se.sm.trend/modelo$ts.sm.trend*100
modelo$cv.sm.slope<- modelo$se.sm.slope/modelo$ts.sm.slope*100
modelo$cv.sm.signal<- modelo$se.sm.signal/modelo$ts.sm.signal*100

# Objeto para armazenar resultados:

a <- cbind(
  round(exp(grid_error), 4),  
  t(sapply(1:nrow(grid_error), function(i) {
    tryCatch({
      
      i0 <- as.numeric(grid_error[i, ])
      
      # Ajustando o modelo
      modelo$initial <- i0
      modelo$fit <- dlmMLE(y, modelo$initial, modelo$fn, hessian = TRUE, control = list(maxit = 10^8))
      
      # Coletando resultados
      c(
        round(exp(modelo$fit$par), 4),  
        modelo$fit$convergence,       
        modelo$fit$value               
      )
    }, error = function(e) {
      rep(NA, 6)  # Retorna NA se houver erro
    })
  }))
)


colnames(a) <- c(
  "level_ini", "slope_ini", "seasonal_ini", "irregular_ini",  
  "level", "slope", "seasonal", "irregular",                 
  "convergence", "log_like"                                  
)

hist(a$level)
boxplot(a$level)
summary(a$level)

hist(a$slope)
boxplot(a$slope)
summary(a$slope)

hist(a$seasonal)
boxplot(a$seasonal)
summary(a$seasonal)

hist(a$irregular)
boxplot(a$irregular)
summary(a$irregular)


modelo_bsm<- modelos_bsm_ini[[which(a$log_like==min(a$log_like,na.rm = TRUE))]]



# Estatísticas de interesse

## Processamento paralelo e criação de clusters

numCores<-detectCores()
numCores

cl<- makeCluster(numCores-1)


## Salvamento de imagem:

save.image("partial.Rdata")


## Uso da imagem nos clusters

clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})


## Start nos modelos:

start_time <- Sys.time()
modelos_bsm_ini<-parLapply(cl,1:nrow(grid), function(i)  tryCatch(f.modelo_bsm(y,grid[i,]),error=function(e) {rep(NA,3)}))
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.modelo_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
modelos_bsm_error_1_ini<-parLapply(cl,1:nrow(grid), function(i)  tryCatch(f.modelo_bsm_error_1(y,grid[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

# stop cluster
stopCluster(cl)
showConnections()


## Etapa de avaliação:
  # Hiperparâmetros iniciais vs estimados
  # Convergência

a <- cbind(round(exp(grid),4),
           t(sapply(1:nrow(grid), function(i) tryCatch( c(round(exp(modelos_bsm_ini[[i]][["fit"]][["par"]]),4),
                                                          modelos_bsm_ini[[i]][["fit"]][["convergence"]],
                                                          modelos_bsm_ini[[i]][["fit"]][["value"]]),error=function(e) {rep(NA,6)})))
)
  ## Em "a", foi necessário mudar o número de colunas para 7 para igualar o vetor de nomes

colnames(a) <- c("level_ini","slope_ini","seasonal_ini","irregual_ini",
                 "level","slope","seasonal","irregular",
                 "convergence","log_like")
hist(a$level)
boxplot(a$level)
summary(a$level)

hist(a$slope)
boxplot(a$slope)
summary(a$slope)

hist(a$seasonal)
boxplot(a$seasonal)
summary(a$seasonal)

hist(a$irregular)
boxplot(a$irregular)
summary(a$irregular)




