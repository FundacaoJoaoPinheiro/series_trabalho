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



### TESTE PARA BELO HORIZONTE ##################################################

## Carregando bases e definindo objeto para BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
bh<-baseestr0324$`01-Belo Horizonte`
baserot <- readRDS("D:/FJP2425/Programacao/data/baserot0324.RDS")
dtbh<-baserot$`01-Belo Horizonte` ## Arquivo "cru", saída direta da rotina da base por rotação
dbbh<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/01_params_bh.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros


## Fazendo para total ocupada

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




