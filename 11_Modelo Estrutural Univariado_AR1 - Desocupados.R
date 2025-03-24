################################################################################
##                          MODELO BSM UNIVARIADO                             ##
################################################################################

library(dlm)
library(tidyverse)
library(parallel)
library(R.utils)
library(callr)
options(scipen=999)

# Neste script a grande diferença é deixar variar o nível da tendência na matriz W
  # Um novo param deve ser incluído em cada grid

# A princípio, todos os modelos rodarão um AR1

### MODELO BH ##################################################################
rm(list = ls())

## UCM: 26.15096; 0.0001269569; 75.98691

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/12_estrutural_AR1.R")

## Dados:

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
bh<-baseestr0424$`01-Belo Horizonte`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtbh<-baseal0424$`01-Belo Horizonte` ## Arquivo "cru", saída direta da rotina da base por rotação
dbbh<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/01_params_bh.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- bh$Total.de.desocupados/1000
se_db<- bh$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbbh[["mod_ar1"]][["phi1_ar1_dbh"]]

### Modelo DLM com erro amostral

# Parâmetros iniciais:
par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-seq(-5,5,3)
par_5<-c(0)

# Input dos parâmetros iniciais do modelo

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)
grid_error<-grid_error[-188,] #Cortei porque não estava convergindo

# Antes de rodar o modelo com processamento paralelo, recomenda-se testar os parâmetros

source("data/funcoes/13_rodar_grid_error.R")
testegrid <- rodar_grid_error(y, grid_error, f.teste_bsm_error)
modelos_bsm_error_ini<-testegrid$resultados

# Processamento paralelo:

numCores<-detectCores()
numCores

cl<- makeCluster(numCores-1)

save.image("partial.Rdata")

clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Teste para identificar possíveis bugs

#modelo<-f.teste_bsm_error(y,grid_error[4,])

# Estimação do Modelo

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,5)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

b <- cbind(
  round(exp(grid_error), 5),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(modelos_bsm_error_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- modelos_bsm_error_ini[[i]][["fit"]][["convergence"]]
      log_like <- modelos_bsm_error_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)


colnames(b) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                 "level","slope","seasonal","irregular", "sampl_error",
                 "convergence","log_like")

# Retirando NAs:

itbh <- b[complete.cases(b), ] # 8 NAs

hist(itbh$slope)
boxplot(itbh$slope)
summary(itbh$slope)

hist(itbh$seasonal)
boxplot(itbh$seasonal)
summary(itbh$seasonal)

hist(itbh$irregular)
boxplot(itbh$irregular)
summary(itbh$irregular)

hist(itbh$sampl_error)
boxplot(itbh$sampl_error)
summary(itbh$sampl_error)

## Após iniciais, seleção do modelo:

modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),5)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

BIC<-2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T)

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],5),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultadosbh<-cbind(convergencia,parametros,testes,AIC,BIC)
resultadosbh

par(mfrow=c(1,2), mar=c(5,5,1,1), cex=0.8)
fig_1 <- window(ts.union(
  ts(modelo_bsm_error$ts.original, start = 2012, frequency = 4),
  ts(modelo_bsm_error$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("Desocupação: design-based",
                             "Sinal da desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))

mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1 <- window(ts.union(
  ts((modelo_bsm_error$cv.original) * 100, start = 2012, frequency = 4),
  ts(modelo_bsm_error$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

# Salvando o .Rdata

# save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/01_mod_bh.Rdata")

### ENTORNO METROPOLITANO ###############################################
rm(list = ls())

## UCM: 106.7323; 0.00348154; 106.8907

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/12_estrutural_AR1.R")

## Carregando bases e definindo objeto para o Entorno

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
ent<-baseestr0424$`02-Entorno metropolitano de BH`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtent<-baseal0424$`02-Entorno metropolitano de BH` ## Arquivo "cru", saída direta da rotina da base por rotação
dbent<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/02_params_ent.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- (ent$Total.de.desocupados)/1000
se_db <- (ent$sd_d)/1000
cv_db <- se_db/y
par_ar_erro <- dbent[["mod_ar1"]][["phi1_ar1_dent"]]

### Modelo DLM com erro amostral

# Parâmetros iniciais:
  # Realizei várias alterações nos iniciais, visto que alguns estavam quebrando o cálculo

par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-seq(-5,5,3)
par_5<-c(0)

# Input dos parâmetros iniciais do modelo

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)
grid_error<-grid_error[-188,] #Cortei porque não estava convergindo

# Antes de rodar o modelo com processamento paralelo, recomenda-se testar os parâmetros

source("data/funcoes/13_rodar_grid_error.R")
testegrid <- rodar_grid_error(y, grid_error, f.teste_bsm_error)
modelos_bsm_error_ini<-testegrid$resultados

# Processamento paralelo:

numCores<-detectCores()
numCores

cl<- makeCluster(numCores-1)

save.image("partial.Rdata")

clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,5)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

b <- cbind(
  round(exp(grid_error), 5),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(modelos_bsm_error_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- modelos_bsm_error_ini[[i]][["fit"]][["convergence"]]
      log_like <- modelos_bsm_error_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(b) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                 "level","slope","seasonal","irregular", "sampl_error",
                 "convergence","log_like")

# Retirando NAs:

itent <- b[complete.cases(b), ] # NAs

hist(itent$slope)
boxplot(itent$slope)
summary(itent$slope)

hist(itent$seasonal)
boxplot(itent$seasonal)
summary(itent$seasonal)

hist(itent$irregular)
boxplot(itent$irregular)
summary(itent$irregular)

hist(itent$sampl_error)
boxplot(itent$sampl_error)
summary(itent$sampl_error)

## Após iniciais, seleção do modelo:

modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),5)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

BIC<-2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T)

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],5),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultadosent<-cbind(convergencia,parametros,testes,AIC,BIC)
resultadosent

par(mfrow=c(1,2), mar=c(5,5,1,1), cex=0.8)
fig_1 <- window(ts.union(
  ts(modelo_bsm_error$ts.original, start = 2012, frequency = 4),
  ts(modelo_bsm_error$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                             "Sinal da desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))

mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1 <- window(ts.union(
  ts((modelo_bsm_error$cv.original) * 100, start = 2012, frequency = 4),
  ts(modelo_bsm_error$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

# Salvando o .Rdata

#save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/02_mod_ent.Rdata")

### COLAR METROPOLITANO DE BH ##################################################
rm(list = ls())

# UCM: 3.830586; 0.00000616888; 10.51971

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/12_estrutural_AR1.R")

## Carregando bases e definindo objeto para o Colar BH

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
col<-baseestr0424$`03-Colar metropolitano de BH`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtcol<-baseal0424$`03-Colar metropolitano de BH` ## Arquivo "cru", saída direta da rotina da base por rotação
dbcol<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/03_params_col.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- col$Total.de.desocupados/1000
se_db <- col$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbcol[["mod_ar1"]][["phi1_ar1_dcol"]]

### Modelo DLM com erro amostral

# Parâmetros iniciais:

par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-seq(-5,5,3)
par_5<-c(0)

# Input dos parâmetros iniciais do modelo

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)
grid_error<-grid_error[-188,] #Cortei porque não estava convergindo

# Antes de rodar o modelo com processamento paralelo, recomenda-se testar os parâmetros

source("data/funcoes/13_rodar_grid_error.R")
testegrid <- rodar_grid_error(y, grid_error, f.teste_bsm_error)
modelos_bsm_error_ini<-testegrid$resultados

# Processamento paralelo:

numCores<-detectCores()
numCores

cl<- makeCluster(numCores-1)

save.image("partial.Rdata")

clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,5)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

b <- cbind(
  round(exp(grid_error), 5),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(modelos_bsm_error_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- modelos_bsm_error_ini[[i]][["fit"]][["convergence"]]
      log_like <- modelos_bsm_error_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(b) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                 "level","slope","seasonal","irregular", "sampl_error",
                 "convergence","log_like")

# Retirando NAs:

itcol <- b[complete.cases(b), ] # 1 NAs

## Após iniciais, seleção do modelo:

modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),5)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

BIC<-2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T)

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],5),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultadoscol<-cbind(convergencia,parametros,testes,AIC,BIC)
resultadoscol

par(mfrow=c(1,2), mar=c(5,5,1,1), cex=0.8)
fig_1 <- window(ts.union(
  ts(modelo_bsm_error$ts.original, start = 2012, frequency = 4),
  ts(modelo_bsm_error$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))

mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1 <- window(ts.union(
  ts((modelo_bsm_error$cv.original) * 100, start = 2012, frequency = 4),
  ts(modelo_bsm_error$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

# Salvando o .Rdata

#save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/03_mod_col.Rdata")

### RIDE de Brasília em Minas ##################################################
rm(list = ls())

# UCM: 0.03001628; 0.004274386; 1.105705

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/12_estrutural_AR1.R")

## Carregando bases e definindo objeto para RIDE

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
rid<-baseestr0424$`04-RIDE de Brasília em Minas`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtrid<-baseal0424$`04-RIDE de Brasília em Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbrid<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/04_params_rid.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- rid$Total.de.desocupados/1000
se_db <- rid$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbrid[["mod_ar1"]][["phi1_ar1_drid"]]

### Modelo DLM com erro amostral

# Parâmetros iniciais:

par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-seq(-5,5,3)
par_5<-c(0)

# Input dos parâmetros iniciais do modelo

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)
grid_error<-grid_error[-188,] #Cortei porque não estava convergindo

# Antes de rodar o modelo com processamento paralelo, recomenda-se testar os parâmetros

source("data/funcoes/13_rodar_grid_error.R")
testegrid <- rodar_grid_error(y, grid_error, f.teste_bsm_error)
modelos_bsm_error_ini<-testegrid$resultados

# Processamento paralelo:

numCores<-detectCores()
numCores

cl<- makeCluster(numCores-1)

save.image("partial.Rdata")

clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,5)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

b <- cbind(
  round(exp(grid_error), 5),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(modelos_bsm_error_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- modelos_bsm_error_ini[[i]][["fit"]][["convergence"]]
      log_like <- modelos_bsm_error_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(b) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                 "level","slope","seasonal","irregular", "sampl_error",
                 "convergence","log_like")
# Retirando NAs:

itrid <- b[complete.cases(b), ] # 2 NAs

hist(itrid$slope)
boxplot(itrid$slope)
summary(itrid$slope)

hist(itrid$seasonal)
boxplot(itrid$seasonal)
summary(itrid$seasonal)

hist(itrid$irregular)
boxplot(itrid$irregular)
summary(itrid$irregular)

hist(itrid$sampl_error)
boxplot(itrid$sampl_error)
summary(itrid$sampl_error)

## Após iniciais, seleção do modelo:

modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),5)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

BIC<-2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T)

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],5),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultadosrid<-cbind(convergencia,parametros,testes,AIC,BIC)
resultadosrid

par(mfrow=c(1,2), mar=c(5,5,1,1), cex=0.8)
fig_1 <- window(ts.union(
  ts(modelo_bsm_error$ts.original, start = 2012, frequency = 4),
  ts(modelo_bsm_error$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))

mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1 <- window(ts.union(
  ts((modelo_bsm_error$cv.original) * 100, start = 2012, frequency = 4),
  ts(modelo_bsm_error$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

# Salvando o .Rdata

#save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/04_mod_rid.Rdata")

### SUL DE MINAS ###############################################################
rm(list = ls())

# UCM: 56.85534; 0.0008044187; 90.04413

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/12_estrutural_AR1.R")

## Carregando bases e definindo objeto para o Sul

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
sul<-baseestr0424$`05-Sul de Minas`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtsul<-baseal0424$`05-Sul de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbsul<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/05_params_sul.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- sul$Total.de.desocupados/1000
se_db <- sul$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbsul[["mod_ar1"]][["phi1_ar1_dsul"]]

### Modelo DLM com erro amostral

# Parâmetros iniciais:

par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-seq(-5,5,3)
par_5<-c(0)

# Input dos parâmetros iniciais do modelo

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)
grid_error<-grid_error[-c(42,182,188),] #Cortei porque não estava convergindo

# Antes de rodar o modelo com processamento paralelo, recomenda-se testar os parâmetros

source("data/funcoes/13_rodar_grid_error.R")
testegrid <- rodar_grid_error(y, grid_error, f.teste_bsm_error)
modelos_bsm_error_ini<-testegrid$resultados

# Processamento paralelo:

numCores<-detectCores()
numCores

cl<- makeCluster(numCores-1)

save.image("partial.Rdata")

clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,5)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

b <- cbind(
  round(exp(grid_error), 5),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(modelos_bsm_error_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- modelos_bsm_error_ini[[i]][["fit"]][["convergence"]]
      log_like <- modelos_bsm_error_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(b) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                 "level","slope","seasonal","irregular", "sampl_error",
                 "convergence","log_like")

# Retirando NAs:

itsul <- b[complete.cases(b), ] # 3 NAs

hist(itsul$slope)
boxplot(itsul$slope)
summary(itsul$slope)

hist(itsul$seasonal)
boxplot(itsul$seasonal)
summary(itsul$seasonal)

hist(itsul$irregular)
boxplot(itsul$irregular)
summary(itsul$irregular)

hist(itsul$sampl_error)
boxplot(itsul$sampl_error)
summary(itsul$sampl_error)

## Após iniciais, seleção do modelo:

modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),5)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

BIC<-2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T)

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0) # true

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],5),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultadossul<-cbind(convergencia,parametros,testes,AIC,BIC)
resultadossul

par(mfrow=c(1,2), mar=c(5,5,1,1), cex=0.8)
fig_1 <- window(ts.union(
  ts(modelo_bsm_error$ts.original, start = 2012, frequency = 4),
  ts(modelo_bsm_error$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))

mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1 <- window(ts.union(
  ts((modelo_bsm_error$cv.original) * 100, start = 2012, frequency = 4),
  ts(modelo_bsm_error$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

# Salvando o .Rdata

#save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/05_mod_sul.Rdata")

### TRIÂNGULO MINEIRO ##########################################################
rm(list = ls())

# UCM:  # 11.70026 # 0.00006308897 # 84.38331

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/12_estrutural_AR1.R")

## Carregando bases e definindo objeto para Triângulo Mineiro

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
trg<-baseestr0424$`06-Triângulo Mineiro`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dttrg<-baseal0424$`06-Triângulo Mineiro` ## Arquivo "cru", saída direta da rotina da base por rotação
dbtrg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/06_params_trg.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- trg$Total.de.desocupados/1000
se_db <- trg$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbtrg[["mod_ar1"]][["phi1_ar1_dtrg"]]

### Modelo DLM com erro amostral

# Parâmetros iniciais:

par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-seq(-5,5,3)
par_5<-c(0)

# Input dos parâmetros iniciais do modelo

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)
grid_error<-grid_error[-188,] #Cortei porque não estava convergindo

# Antes de rodar o modelo com processamento paralelo, recomenda-se testar os parâmetros

source("data/funcoes/13_rodar_grid_error.R")
testegrid <- rodar_grid_error(y, grid_error, f.teste_bsm_error)
modelos_bsm_error_ini<-testegrid$resultados

# Processamento paralelo:

numCores<-detectCores()
numCores

cl<- makeCluster(numCores-1)

save.image("partial.Rdata")

clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,5)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

b <- cbind(
  round(exp(grid_error), 5),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(modelos_bsm_error_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- modelos_bsm_error_ini[[i]][["fit"]][["convergence"]]
      log_like <- modelos_bsm_error_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(b) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                 "level","slope","seasonal","irregular", "sampl_error",
                 "convergence","log_like")

# Retirando NAs:

ittrg <- b[complete.cases(b), ] # 5 NAs

hist(ittrg$slope)
boxplot(ittrg$slope)
summary(ittrg$slope)

hist(ittrg$seasonal)
boxplot(ittrg$seasonal)
summary(ittrg$seasonal)

hist(ittrg$irregular)
boxplot(ittrg$irregular)
summary(ittrg$irregular)

hist(ittrg$sampl_error)
boxplot(ittrg$sampl_error)
summary(ittrg$sampl_error)

## Após iniciais, seleção do modelo:

modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),5)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

BIC<-2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T)

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],5),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultadostrg<-cbind(convergencia,parametros,testes,AIC,BIC)
resultadostrg

par(mfrow=c(1,2), mar=c(5,5,1,1), cex=0.8)
fig_1 <- window(ts.union(
  ts(modelo_bsm_error$ts.original, start = 2012, frequency = 4),
  ts(modelo_bsm_error$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))

mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1 <- window(ts.union(
  ts((modelo_bsm_error$cv.original) * 100, start = 2012, frequency = 4),
  ts(modelo_bsm_error$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

# Salvando o .Rdata

#save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/06_mod_trg.Rdata")

### ZONA DA MATA ###############################################################
rm(list = ls())

# UCM: # 16.0373 # 0.0004886218 # 82.69742

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/12_estrutural_AR1.R")

## Carregando bases e definindo objeto para a Zona da Mata

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
mat<-baseestr0424$`07-Mata de Minas Gerais`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtmat<-baseal0424$`07-Mata de Minas Gerais` ## Arquivo "cru", saída direta da rotina da base por rotação
dbmat<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/07_params_mat.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- mat$Total.de.desocupados/1000
se_db <- mat$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbmat[["mod_ar1"]][["phi1_ar1_dmat"]]

### Modelo DLM com erro amostral

# Parâmetros iniciais:
par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-seq(-5,5,3)
par_5<-c(0)

# Input dos parâmetros iniciais do modelo

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)
grid_error<-grid_error[-c(46,188,190),] #Cortei porque não estava convergindo

# Antes de rodar o modelo com processamento paralelo, recomenda-se testar os parâmetros

source("data/funcoes/13_rodar_grid_error.R")
testegrid <- rodar_grid_error(y, grid_error, f.teste_bsm_error)
modelos_bsm_error_ini<-testegrid$resultados

# Processamento paralelo:

numCores<-detectCores()
numCores

cl<- makeCluster(numCores-1)

save.image("partial.Rdata")

clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,5)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

b <- cbind(
  round(exp(grid_error), 5),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(modelos_bsm_error_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- modelos_bsm_error_ini[[i]][["fit"]][["convergence"]]
      log_like <- modelos_bsm_error_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(b) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                 "level","slope","seasonal","irregular", "sampl_error",
                 "convergence","log_like")

# Retirando NAs:

itmat <- b[complete.cases(b), ] # 4 NAs

hist(itmat$slope)
boxplot(itmat$slope)
summary(itmat$slope)

hist(itmat$seasonal)
boxplot(itmat$seasonal)
summary(itmat$seasonal)

hist(itmat$irregular)
boxplot(itmat$irregular)
summary(itmat$irregular)

hist(itmat$sampl_error)
boxplot(itmat$sampl_error)
summary(itmat$sampl_error)

## Após iniciais, seleção do modelo:

modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),5)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

BIC<-2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T)

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],5),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultadosmat<-cbind(convergencia,parametros,testes,AIC,BIC)
resultadosmat

par(mfrow=c(1,2), mar=c(5,5,1,1), cex=0.8)
fig_1 <- window(ts.union(
  ts(modelo_bsm_error$ts.original, start = 2012, frequency = 4),
  ts(modelo_bsm_error$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))

mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1 <- window(ts.union(
  ts((modelo_bsm_error$cv.original) * 100, start = 2012, frequency = 4),
  ts(modelo_bsm_error$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

# Salvando o .Rdata

#save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/07_mod_mat.Rdata")

### NORTE DE MINAS GERAIS ######################################################
rm(list = ls())

# UCM: # 43.22508 # 0.0009056865 # 142.2077

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/12_estrutural_AR1.R")

## Carregando bases e definindo objeto para o Norte

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
nrt<-baseestr0424$`08-Norte de Minas`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtnrt<-baseal0424$`08-Norte de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbnrt<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/08_params_nrt.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- nrt$Total.de.desocupados/1000
se_db <- nrt$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbnrt[["mod_ar1"]][["phi1_ar1_dnrt"]]

### Modelo DLM com erro amostral

# Parâmetros iniciais:

par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-seq(-5,5,3)
par_5<-c(0)

# Input dos parâmetros iniciais do modelo

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)
grid_error<-grid_error[-c(50,188),] #Cortei porque não estava convergindo

# Antes de rodar o modelo com processamento paralelo, recomenda-se testar os parâmetros

source("data/funcoes/13_rodar_grid_error.R")
testegrid <- rodar_grid_error(y, grid_error, f.teste_bsm_error)
modelos_bsm_error_ini<-testegrid$resultados

# Processamento paralelo:

numCores<-detectCores()
numCores

cl<- makeCluster(numCores-1)

save.image("partial.Rdata")

clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,5)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

b <- cbind(
  round(exp(grid_error), 5),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(modelos_bsm_error_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- modelos_bsm_error_ini[[i]][["fit"]][["convergence"]]
      log_like <- modelos_bsm_error_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(b) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                 "level","slope","seasonal","irregular", "sampl_error",
                 "convergence","log_like")
# Retirando NAs:

itnrt <- b[complete.cases(b), ] # 4 NAs

hist(itnrt$slope)
boxplot(itnrt$slope)
summary(itnrt$slope)

hist(itnrt$seasonal)
boxplot(itnrt$seasonal)
summary(itnrt$seasonal)

hist(itnrt$irregular)
boxplot(itnrt$irregular)
summary(itnrt$irregular)

hist(itnrt$sampl_error)
boxplot(itnrt$sampl_error)
summary(itnrt$sampl_error)

## Após iniciais, seleção do modelo:

modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),5)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

BIC<-2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T)

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],5),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultadosnrt<-cbind(convergencia,parametros,testes,AIC,BIC)
resultadosnrt

par(mfrow=c(1,2), mar=c(5,5,1,1), cex=0.8)
fig_1 <- window(ts.union(
  ts(modelo_bsm_error$ts.original, start = 2012, frequency = 4),
  ts(modelo_bsm_error$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))

mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1 <- window(ts.union(
  ts((modelo_bsm_error$cv.original) * 100, start = 2012, frequency = 4),
  ts(modelo_bsm_error$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

# Salvando o .Rdata

 #save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/08_mod_nrt.Rdata")

### VALE DO RIO DOCE ###########################################################
rm(list = ls())

# UCM: # 28.41725 # 1.194057 # 87.2518

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/12_estrutural_AR1.R")

## Carregando bases e definindo objeto para o vale

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
vl<-baseestr0424$`09-Vale do Rio Doce`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtvl<-baseal0424$`09-Vale do Rio Doce` ## Arquivo "cru", saída direta da rotina da base por rotação
dbvl<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/09_params_rio.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- vl$Total.de.desocupados/1000
se_db <- vl$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbvl[["mod_ar1"]][["phi1_ar1_drio"]]

### Modelo DLM com erro amostral

# Parâmetros iniciais:
par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-seq(-5,5,3)
par_5<-c(0)

# Input dos parâmetros iniciais do modelo

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)
grid_error<-grid_error[-c(53,86),] #Cortei porque não estava convergindo

# Antes de rodar o modelo com processamento paralelo, recomenda-se testar os parâmetros

source("data/funcoes/13_rodar_grid_error.R")
testegrid <- rodar_grid_error(y, grid_error, f.teste_bsm_error)
modelos_bsm_error_ini<-testegrid$resultados

# Processamento paralelo:

numCores<-detectCores()
numCores

cl<- makeCluster(numCores-1)

save.image("partial.Rdata")

clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,5)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

b <- cbind(
  round(exp(grid_error), 5),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(modelos_bsm_error_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- modelos_bsm_error_ini[[i]][["fit"]][["convergence"]]
      log_like <- modelos_bsm_error_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(b) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                 "level","slope","seasonal","irregular", "sampl_error",
                 "convergence","log_like")

# Retirando NAs:

itval <- b[complete.cases(b), ] # 4 NAs

hist(itval$slope)
boxplot(itval$slope)
summary(itval$slope)

hist(itval$seasonal)
boxplot(itval$seasonal)
summary(itval$seasonal)

hist(itval$irregular)
boxplot(itval$irregular)
summary(itval$irregular)

hist(itval$sampl_error)
boxplot(itval$sampl_error)
summary(itval$sampl_error)

## Após iniciais, seleção do modelo:

modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),5)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

BIC<-2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T)

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0) # true

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],5),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultadosval<-cbind(convergencia,parametros,testes,AIC,BIC)
resultadosval

par(mfrow=c(1,2), mar=c(5,5,1,1), cex=0.8)
fig_1 <- window(ts.union(
  ts(modelo_bsm_error$ts.original, start = 2012, frequency = 4),
  ts(modelo_bsm_error$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))

mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1 <- window(ts.union(
  ts((modelo_bsm_error$cv.original) * 100, start = 2012, frequency = 4),
  ts(modelo_bsm_error$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/09_mod_val.Rdata")

### CENTRAL ####################################################################
rm(list = ls())

# UCM: # 53.27499 # 0.001126252 # 83.0792

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/12_estrutural_AR1.R")

## Carregando bases e definindo objeto para central 

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
cen<-baseestr0424$`10-Central`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtcen<-baseal0424$`10-Central` ## Arquivo "cru", saída direta da rotina da base por rotação
dbcen<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/10_params_cen.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- cen$Total.de.desocupados/1000
se_db <- cen$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbcen[["mod_ar1"]][["phi1_ar1_dcen"]]

### Modelo DLM com erro amostral

# Parâmetros iniciais:

par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-seq(-5,5,3)
par_5<-c(0)

# Input dos parâmetros iniciais do modelo

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)
grid_error<-grid_error[-c(150,166,194,214,226),] #Cortei porque não estava convergindo

# Antes de rodar o modelo com processamento paralelo, recomenda-se testar os parâmetros

source("data/funcoes/13_rodar_grid_error.R")
testegrid <- rodar_grid_error(y, grid_error, f.teste_bsm_error)
modelos_bsm_error_ini<-testegrid$resultados

# Processamento paralelo:

numCores<-detectCores()
numCores

cl<- makeCluster(numCores-1)

save.image("partial.Rdata")

clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,5)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

b <- cbind(
  round(exp(grid_error), 5),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(modelos_bsm_error_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- modelos_bsm_error_ini[[i]][["fit"]][["convergence"]]
      log_like <- modelos_bsm_error_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(b) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                 "level","slope","seasonal","irregular", "sampl_error",
                 "convergence","log_like")

# Retirando NAs:

itcen <- b[complete.cases(b), ] # 24 NAs

hist(itcen$slope)
boxplot(itcen$slope)
summary(itcen$slope)

hist(itcen$seasonal)
boxplot(itcen$seasonal)
summary(itcen$seasonal)

hist(itcen$irregular)
boxplot(itcen$irregular)
summary(itcen$irregular)

hist(itcen$sampl_error)
boxplot(itcen$sampl_error)
summary(itcen$sampl_error)

## Após iniciais, seleção do modelo:

modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),5)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

BIC<-2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T)

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],5),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultadoscen<-cbind(convergencia,parametros,testes,AIC,BIC)
resultadoscen

par(mfrow=c(1,2), mar=c(5,5,1,1), cex=0.8)
fig_1 <- window(ts.union(
  ts(modelo_bsm_error$ts.original, start = 2012, frequency = 4),
  ts(modelo_bsm_error$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))

mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1 <- window(ts.union(
  ts((modelo_bsm_error$cv.original) * 100, start = 2012, frequency = 4),
  ts(modelo_bsm_error$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

# Salvando o .Rdata

#save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/10_mod_cen.Rdata")
