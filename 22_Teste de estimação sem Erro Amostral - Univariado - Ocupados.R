################################################################################
##            TESTE DA ESTIMAÇÃO SEM ERRO AMOSTRAL - OCUPADOS                 ##
################################################################################

## Script para testar a estimação sem erro amostral para o total de ocupados
  ## Como tiramos o EA, não será necessário incorporar diferentes modelos

library(dlm)
library(tidyverse)
library(beepr)
library(parallel)

options(scipen=999)

### BELO HORIZONTE #############################################################
rm(list = ls())

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
bh<-baseestr8reg$`01-Belo Horizonte`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtbh<-baseal8reg$`01-Belo Horizonte` ## Arquivo "cru", saída direta da rotina da base por rotação
dbbh<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/01_params_bh.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- bh$Total.de.ocupados/1000
se_db<- bh$sd_o/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO SEM ERRO AMOSTRAL

source("data/funcoes/02_modelo_bsm.R")

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
run_bh<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.modelo_bsm(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_bh <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_bh[[i]][["fit"]][["par"]]), 4)
      convergence <- run_bh[[i]][["fit"]][["convergence"]]
      log_like <- run_bh[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)


colnames(ini_bh) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini",
                          "level","slope","seasonal","irregular",
                          "convergence","log_like")

## Após iniciais, seleção do modelo:

estimacao_bh <- run_bh[[which(
  ini_bh$log_like == min(ini_bh$log_like[ini_bh$convergence == 0], na.rm = TRUE) & 
    ini_bh$convergence == 0
)]]

# Verificando a convergência

conver_bh<-rbind(estimacao_bh$fit$convergence)
colnames(conver_bh)<-c("convergence") 

# Parâmetros estimados:

parametros_bh<-rbind(c(round(exp(estimacao_bh$fit$par),4)))
row.names(parametros_bh)<-c("BSM")
colnames(parametros_bh)<-c("Level","Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AIC_bh<-rbind(2*(estimacao_bh$fit$value)+2*5)
colnames(AIC_bh)<-"AIC"

BIC_bh<-2*(estimacao_bh$fit$value)+2*5*log(estimacao_bh$T)

# Matriz Hessiana

all(eigen(estimacao_bh$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_bh<-list(estimacao_bh)
testes_bh<-sapply(lista_bh, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_bh<-t(testes_bh)
row.names(testes_bh)<-c("BSM")
colnames(testes_bh)<-c("Shapiro","Box","H")
resultadosbh<-cbind(conver_bh, parametros_bh, testes_bh, AIC_bh, BIC_bh)
resultadosbh

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_bh <- window(ts.union(
  ts(estimacao_bh$ts.original, start = 2012, frequency = 4),
  ts(estimacao_bh$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_bh, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Desocupação: design-based",
                                "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_bh.cv <- window(ts.union(
  ts((estimacao_bh$cv.original) * 100, start = 2012, frequency = 4),
  ts(estimacao_bh$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_bh.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE

figtend_bh<-window(ts.union(ts(estimacao_bh$ts.original, start = 2012, frequency = 4),ts(estimacao_bh$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_bh<-window(ts.union(ts(estimacao_bh$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_bh<-window(ts.union(ts(estimacao_bh$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(3, 1), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_bh, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Ocupação: design-based",
                             "Tendência da ocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_bh, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_bh, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/9_semEAocup_8reg/01_mod_bh.Rdata")

### COLAR e ENTORNO METROPOLITANO ###############################################
rm(list = ls())

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
ent<-baseestr8reg$`02-Colar e Entorno metropolitano de BH`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtent<-baseal8reg$`02-Colar e Entorno Metropolitano de BH` ## Arquivo "cru", saída direta da rotina da base por rotação
dbent<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/02_params_ent.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- (ent$Total.de.ocupados)/1000
se_db <- (ent$sd_o)/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO SEM ERRO AMOSTRAL

source("data/funcoes/02_modelo_bsm.R")
source("data/funcoes/23_rodar_grid_semEA.R")

grid_semEA<-grid_error

# Estimação do Modelo

start_time <- Sys.time()
run_ent <- rodar_grid_semEA(y, grid_semEA, f.modelo_bsm)
end_time <- Sys.time()
end_time - start_time

mod_ent_ini <- run_ent$resultados

# Avaliação das iterações:

ini_ent <- cbind(
  round(exp(grid_semEA), 4),
  do.call(rbind, lapply(1:nrow(grid_semEA), function(i) {
    tryCatch({
      params <- round(exp(mod_ent_ini[[i]][["fit"]][["par"]]), 4)
      convergence <- mod_ent_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ent_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)


colnames(ini_ent) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini",
                       "level","slope","seasonal","irregular",
                       "convergence","log_like")

## Após iniciais, seleção do modelo:

estimacao_ent <- mod_ent_ini[[which(
  ini_ent$log_like == min(ini_ent$log_like[ini_ent$convergence == 0], na.rm = TRUE) &
    ini_ent$convergence == 0
)]]

# Verificando a convergência

conver_ent<-rbind(estimacao_ent$fit$convergence)
colnames(conver_ent)<-c("convergence")

# Parâmetros estimados:

parametros_ent<-rbind(c(round(exp(estimacao_ent$fit$par),4)))
row.names(parametros_ent)<-c("BSM")
colnames(parametros_ent)<-c("Level","Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AIC_ent<-rbind(2*(estimacao_ent$fit$value)+2*5)
colnames(AIC_ent)<-"AIC"

BIC_ent<-2*(estimacao_ent$fit$value)+2*5*log(estimacao_ent$T)

# Matriz Hessiana

all(eigen(estimacao_ent$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ent<-list(estimacao_ent)
testes_ent<-sapply(lista_ent, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ent<-t(testes_ent)
row.names(testes_ent)<-c("BSM")
colnames(testes_ent)<-c("Shapiro","Box","H")
resultadosent<-cbind(conver_ent, parametros_ent, testes_ent, AIC_ent, BIC_ent)
resultadosent

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ent <- window(ts.union(
  ts(estimacao_ent$ts.original, start = 2012, frequency = 4),
  ts(estimacao_ent$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ent, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Desocupação: design-based",
                                "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ent.cv <- window(ts.union(
  ts((estimacao_ent$cv.original) * 100, start = 2012, frequency = 4),
  ts(estimacao_ent$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ent.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE

figtend_ent<-window(ts.union(ts(estimacao_ent$ts.original, start = 2012, frequency = 4),ts(estimacao_ent$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ent<-window(ts.union(ts(estimacao_ent$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ent<-window(ts.union(ts(estimacao_ent$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(3, 1), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ent, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Ocupação: design-based",
                                "Tendência da ocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ent, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ent, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/9_semEAocup_8reg/02_mod_ent.Rdata")

### SUL DE MINAS ###############################################################

rm(list = ls())

# UCM: 56.85534; 0.0008044187; 90.04413

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
sul<-baseestr8reg$`03-Sul de Minas`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtsul<-baseal8reg$`03-Sul de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbsul<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/03_params_sul.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- sul$Total.de.ocupados/1000
se_db <- sul$sd_o/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO SEM ERRO AMOSTRAL

source("data/funcoes/02_modelo_bsm.R")

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
run_sul<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.modelo_bsm(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_sul <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_sul[[i]][["fit"]][["par"]]), 4)
      convergence <- run_sul[[i]][["fit"]][["convergence"]]
      log_like <- run_sul[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)


colnames(ini_sul) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini",
                       "level","slope","seasonal","irregular",
                       "convergence","log_like")

## Após iniciais, seleção do modelo:

estimacao_sul <- run_sul[[which(
  ini_sul$log_like == min(ini_sul$log_like[ini_sul$convergence == 0], na.rm = TRUE) &
    ini_sul$convergence == 0
)]]

# Verificando a convergência

conver_sul<-rbind(estimacao_sul$fit$convergence)
colnames(conver_sul)<-c("convergence")

# Parâmetros estimados:

parametros_sul<-rbind(c(round(exp(estimacao_sul$fit$par),4)))
row.names(parametros_sul)<-c("BSM")
colnames(parametros_sul)<-c("Level","Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AIC_sul<-rbind(2*(estimacao_sul$fit$value)+2*5)
colnames(AIC_sul)<-"AIC"

BIC_sul<-2*(estimacao_sul$fit$value)+2*5*log(estimacao_sul$T)

# Matriz Hessiana

all(eigen(estimacao_sul$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_sul<-list(estimacao_sul)
testes_sul<-sapply(lista_sul, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_sul<-t(testes_sul)
row.names(testes_sul)<-c("BSM")
colnames(testes_sul)<-c("Shapiro","Box","H")
resultadosul<-cbind(conver_sul, parametros_sul, testes_sul, AIC_sul, BIC_sul)
resultadosul

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_sul <- window(ts.union(
  ts(estimacao_sul$ts.original, start = 2012, frequency = 4),
  ts(estimacao_sul$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_sul, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("Desocupação: design-based",
                                "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_sul.cv <- window(ts.union(
  ts((estimacao_sul$cv.original) * 100, start = 2012, frequency = 4),
  ts(estimacao_sul$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_sul.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Sul de Minas (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE

figtend_sul<-window(ts.union(ts(estimacao_sul$ts.original, start = 2012, frequency = 4),ts(estimacao_sul$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_sul<-window(ts.union(ts(estimacao_sul$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_sul<-window(ts.union(ts(estimacao_sul$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(3, 1), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_sul, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("Ocupação: design-based",
                                "Tendência da ocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_sul, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_sul, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Sul de Minas (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/9_semEAocup_8reg/03_mod_sul.Rdata")

### TRIÂNGULO MINEIRO ##########################################################
# Modelos para Triângulo: AR(1); MA(1)

rm(list = ls())

# UCM:  # 11.70026 # 0.00006308897 # 84.38331

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
trg<-baseestr8reg$`04-Triângulo Mineiro`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dttrg<-baseal8reg$`04-Triângulo Mineiro` ## Arquivo "cru", saída direta da rotina da base por rotação
dbtrg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/04_params_trg.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- trg$Total.de.ocupados/1000
se_db <- trg$sd_o/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO SEM ERRO AMOSTRAL

source("data/funcoes/02_modelo_bsm.R")

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
run_trg<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.modelo_bsm(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_trg <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_trg[[i]][["fit"]][["par"]]), 4)
      convergence <- run_trg[[i]][["fit"]][["convergence"]]
      log_like <- run_trg[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)


colnames(ini_trg) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini",
                       "level","slope","seasonal","irregular",
                       "convergence","log_like")

## Após iniciais, seleção do modelo:

estimacao_trg <- run_trg[[which(
  ini_trg$log_like == min(ini_trg$log_like[ini_trg$convergence == 0], na.rm = TRUE) &
    ini_trg$convergence == 0
)]]

# Verificando a convergência

conver_trg<-rbind(estimacao_trg$fit$convergence)
colnames(conver_trg)<-c("convergence")

# Parâmetros estimados:

parametros_trg<-rbind(c(round(exp(estimacao_trg$fit$par),4)))
row.names(parametros_trg)<-c("BSM")
colnames(parametros_trg)<-c("Level","Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AIC_trg<-rbind(2*(estimacao_trg$fit$value)+2*5)
colnames(AIC_trg)<-"AIC"

BIC_trg<-2*(estimacao_trg$fit$value)+2*5*log(estimacao_trg$T)

# Matriz Hessiana

all(eigen(estimacao_trg$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_trg<-list(estimacao_trg)
testes_trg<-sapply(lista_trg, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_trg<-t(testes_trg)
row.names(testes_trg)<-c("BSM")
colnames(testes_trg)<-c("Shapiro","Box","H")
resultadostrg<-cbind(conver_trg, parametros_trg, testes_trg, AIC_trg, BIC_trg)
resultadostrg

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_trg <- window(ts.union(
  ts(estimacao_trg$ts.original, start = 2012, frequency = 4),
  ts(estimacao_trg$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_trg, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Desocupação: design-based",
                                "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_trg.cv <- window(ts.union(
  ts((estimacao_trg$cv.original) * 100, start = 2012, frequency = 4),
  ts(estimacao_trg$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_trg.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE

figtend_trg<-window(ts.union(ts(estimacao_trg$ts.original, start = 2012, frequency = 4),ts(estimacao_trg$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_trg<-window(ts.union(ts(estimacao_trg$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_trg<-window(ts.union(ts(estimacao_trg$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(3, 1), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_trg, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Ocupação: design-based",
                                "Tendência da ocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_trg, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_trg, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/9_semEAocup_8reg/04_mod_trg.Rdata")

### ZONA DA MATA ###############################################################

rm(list = ls())

# UCM: # 16.0373 # 0.0004886218 # 82.69742

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
mat<-baseestr8reg$`05-Mata de Minas Gerais`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtmat<-baseal8reg$`05-Mata de Minas Gerais` ## Arquivo "cru", saída direta da rotina da base por rotação
dbmat<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/05_params_mat.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- mat$Total.de.ocupados/1000
se_db <- mat$sd_o/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO SEM ERRO AMOSTRAL

source("data/funcoes/02_modelo_bsm.R")

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
run_mat<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.modelo_bsm(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_mat <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_mat[[i]][["fit"]][["par"]]), 4)
      convergence <- run_mat[[i]][["fit"]][["convergence"]]
      log_like <- run_mat[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)


colnames(ini_mat) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini",
                       "level","slope","seasonal","irregular",
                       "convergence","log_like")

## Após iniciais, seleção do modelo:

estimacao_mat <- run_mat[[which(
  ini_mat$log_like == min(ini_mat$log_like[ini_mat$convergence == 0], na.rm = TRUE) &
    ini_mat$convergence == 0
)]]

# Verificando a convergência

conver_mat<-rbind(estimacao_mat$fit$convergence)
colnames(conver_mat)<-c("convergence")

# Parâmetros estimados:

parametros_mat<-rbind(c(round(exp(estimacao_mat$fit$par),4)))
row.names(parametros_mat)<-c("BSM")
colnames(parametros_mat)<-c("Level","Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AIC_mat<-rbind(2*(estimacao_mat$fit$value)+2*5)
colnames(AIC_mat)<-"AIC"

BIC_mat<-2*(estimacao_mat$fit$value)+2*5*log(estimacao_mat$T)

# Matriz Hessiana

all(eigen(estimacao_mat$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_mat<-list(estimacao_mat)
testes_mat<-sapply(lista_mat, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_mat<-t(testes_mat)
row.names(testes_mat)<-c("BSM")
colnames(testes_mat)<-c("Shapiro","Box","H")
resultadosmat<-cbind(conver_mat, parametros_mat, testes_mat, AIC_mat, BIC_mat)
resultadosmat

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_mat <- window(ts.union(
  ts(estimacao_mat$ts.original, start = 2012, frequency = 4),
  ts(estimacao_mat$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_mat, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Desocupação: design-based",
                                "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_mat.cv <- window(ts.union(
  ts((estimacao_mat$cv.original) * 100, start = 2012, frequency = 4),
  ts(estimacao_mat$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_mat.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05 - Zona da Mata (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE

figtend_mat<-window(ts.union(ts(estimacao_mat$ts.original, start = 2012, frequency = 4),ts(estimacao_mat$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_mat<-window(ts.union(ts(estimacao_mat$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_mat<-window(ts.union(ts(estimacao_mat$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(3, 1), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_mat, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Ocupação: design-based",
                                "Tendência da ocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_mat, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_mat, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05 - Zona da Mata (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/9_semEAocup_8reg/05_mod_mat.Rdata")

### NORTE DE MINAS GERAIS ######################################################
# Modelos para Norte: AR(1);AR(5)

rm(list = ls())

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
nrt<-baseestr8reg$`06-Norte de Minas`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtnrt<-baseal8reg$`06-Norte de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbnrt<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/06_params_nrt.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- nrt$Total.de.ocupados/1000
se_db <- nrt$sd_o/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO SEM ERRO AMOSTRAL

source("data/funcoes/02_modelo_bsm.R")

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
run_nrt<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.modelo_bsm(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_nrt <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_nrt[[i]][["fit"]][["par"]]), 4)
      convergence <- run_nrt[[i]][["fit"]][["convergence"]]
      log_like <- run_nrt[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)


colnames(ini_nrt) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","level","slope","seasonal","irregular",
                       "convergence","log_like")

## Após iniciais, seleção do modelo:

estimacao_nrt <- run_nrt[[which(
  ini_nrt$log_like == min(ini_nrt$log_like[ini_nrt$convergence == 0], na.rm = TRUE) &
    ini_nrt$convergence == 0
)]]

# Verificando a convergência

conver_nrt<-rbind(estimacao_nrt$fit$convergence)
colnames(conver_nrt)<-c("convergence")

# Parâmetros estimados:

parametros_nrt<-rbind(c(round(exp(estimacao_nrt$fit$par),4)))
row.names(parametros_nrt)<-c("BSM")
colnames(parametros_nrt)<-c("Level","Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AIC_nrt<-rbind(2*(estimacao_nrt$fit$value)+2*5)
colnames(AIC_nrt)<-"AIC"

BIC_nrt<-2*(estimacao_nrt$fit$value)+2*5*log(estimacao_nrt$T)

# Matriz Hessiana

all(eigen(estimacao_nrt$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_nrt<-list(estimacao_nrt)
testes_nrt<-sapply(lista_nrt, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_nrt<-t(testes_nrt)
row.names(testes_nrt)<-c("BSM")
colnames(testes_nrt)<-c("Shapiro","Box","H")
resultadosnrt<-cbind(conver_nrt, parametros_nrt, testes_nrt, AIC_nrt, BIC_nrt)
resultadosnrt

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_nrt <- window(ts.union(
  ts(estimacao_nrt$ts.original, start = 2012, frequency = 4),
  ts(estimacao_nrt$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_nrt, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Desocupação: design-based",
                                "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_nrt.cv <- window(ts.union(
  ts((estimacao_nrt$cv.original) * 100, start = 2012, frequency = 4),
  ts(estimacao_nrt$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_nrt.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06 - Norte de Minas (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE

figtend_nrt<-window(ts.union(ts(estimacao_nrt$ts.original, start = 2012, frequency = 4),ts(estimacao_nrt$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_nrt<-window(ts.union(ts(estimacao_nrt$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_nrt<-window(ts.union(ts(estimacao_nrt$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(3, 1), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_nrt, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Ocupação: design-based",
                                "Tendência da ocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_nrt, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_nrt, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06 - Norte de Minas (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/9_semEAocup_8reg/06_mod_nrt.Rdata")

### VALE DO RIO DOCE ###########################################################
# Modelos para Vale: AR(1);

rm(list = ls())

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
vl<-baseestr8reg$`07-Vale do Rio Doce`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtvl<-baseal8reg$`07-Vale do Rio Doce` ## Arquivo "cru", saída direta da rotina da base por rotação
dbvl<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/07_params_rio.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- vl$Total.de.ocupados/1000
se_db <- vl$sd_o/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO SEM ERRO AMOSTRAL

source("data/funcoes/02_modelo_bsm.R")

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
run_val<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.modelo_bsm(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_val <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_val[[i]][["fit"]][["par"]]), 4)
      convergence <- run_val[[i]][["fit"]][["convergence"]]
      log_like <- run_val[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)


colnames(ini_val) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini",
                       "level","slope","seasonal","irregular",
                       "convergence","log_like")

## Após iniciais, seleção do modelo:

estimacao_val <- run_val[[which(
  ini_val$log_like == min(ini_val$log_like[ini_val$convergence == 0], na.rm = TRUE) &
    ini_val$convergence == 0
)]]

# Verificando a convergência

conver_val<-rbind(estimacao_val$fit$convergence)
colnames(conver_val)<-c("convergence")

# Parâmetros estimados:

parametros_val<-rbind(c(round(exp(estimacao_val$fit$par),4)))
row.names(parametros_val)<-c("BSM")
colnames(parametros_val)<-c("Level","Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AIC_val<-rbind(2*(estimacao_val$fit$value)+2*5)
colnames(AIC_val)<-"AIC"

BIC_val<-2*(estimacao_val$fit$value)+2*5*log(estimacao_val$T)

# Matriz Hessiana

all(eigen(estimacao_val$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_val<-list(estimacao_val)
testes_val<-sapply(lista_val, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_val<-t(testes_val)
row.names(testes_val)<-c("BSM")
colnames(testes_val)<-c("Shapiro","Box","H")
resultadosval<-cbind(conver_val, parametros_val, testes_val, AIC_val, BIC_val)
resultadosval

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_val <- window(ts.union(
  ts(estimacao_val$ts.original, start = 2012, frequency = 4),
  ts(estimacao_val$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_val, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Desocupação: design-based",
                                "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_val.cv <- window(ts.union(
  ts((estimacao_val$cv.original) * 100, start = 2012, frequency = 4),
  ts(estimacao_val$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_val.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07 - Vale do Rio Doce (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE

figtend_val<-window(ts.union(ts(estimacao_val$ts.original, start = 2012, frequency = 4),ts(estimacao_val$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_val<-window(ts.union(ts(estimacao_val$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_val<-window(ts.union(ts(estimacao_val$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(3, 1), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_val, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Ocupação: design-based",
                                "Tendência da ocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_val, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_val, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07 - Vale do Rio Doce (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/9_semEAocup_8reg/07_mod_val.Rdata")

### CENTRAL ####################################################################
rm(list = ls())

# UCM: # 53.27499 # 0.001126252 # 83.0792

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
cen<-baseestr8reg$`08-Central`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtcen<-baseal8reg$`08-Central` ## Arquivo "cru", saída direta da rotina da base por rotação
dbcen<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/08_params_cen.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- cen$Total.de.ocupados/1000
se_db <- cen$sd_o/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO SEM ERRO AMOSTRAL

source("data/funcoes/02_modelo_bsm.R")

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
run_cen<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.modelo_bsm(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_cen <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_cen[[i]][["fit"]][["par"]]), 4)
      convergence <- run_cen[[i]][["fit"]][["convergence"]]
      log_like <- run_cen[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)


colnames(ini_cen) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini",
                       "level","slope","seasonal","irregular",
                       "convergence","log_like")

## Após iniciais, seleção do modelo:

estimacao_cen <- run_cen[[which(
  ini_cen$log_like == min(ini_cen$log_like[ini_cen$convergence == 0], na.rm = TRUE) &
    ini_cen$convergence == 0
)]]

# Verificando a convergência

conver_cen<-rbind(estimacao_cen$fit$convergence)
colnames(conver_cen)<-c("convergence")

# Parâmetros estimados:

parametros_cen<-rbind(c(round(exp(estimacao_cen$fit$par),4)))
row.names(parametros_cen)<-c("BSM")
colnames(parametros_cen)<-c("Level","Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AIC_cen<-rbind(2*(estimacao_cen$fit$value)+2*5)
colnames(AIC_cen)<-"AIC"

BIC_cen<-2*(estimacao_cen$fit$value)+2*5*log(estimacao_cen$T)

# Matriz Hessiana

all(eigen(estimacao_cen$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_cen<-list(estimacao_cen)
testes_cen<-sapply(lista_cen, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_cen<-t(testes_cen)
row.names(testes_cen)<-c("BSM")
colnames(testes_cen)<-c("Shapiro","Box","H")
resultadoscen<-cbind(conver_cen, parametros_cen, testes_cen, AIC_cen, BIC_cen)
resultadoscen

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_cen <- window(ts.union(
  ts(estimacao_cen$ts.original, start = 2012, frequency = 4),
  ts(estimacao_cen$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_cen, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Desocupação: design-based",
                                "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_cen.cv <- window(ts.union(
  ts((estimacao_cen$cv.original) * 100, start = 2012, frequency = 4),
  ts(estimacao_cen$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_cen.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Central (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE

figtend_cen<-window(ts.union(ts(estimacao_cen$ts.original, start = 2012, frequency = 4),ts(estimacao_cen$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_cen<-window(ts.union(ts(estimacao_cen$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_cen<-window(ts.union(ts(estimacao_cen$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(3, 1), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_cen, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Ocupação: design-based",
                                "Tendência da ocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_cen, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_cen, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Central (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/9_semEAocup_8reg/08_mod_cen.Rdata")


### MINAS GERAIS ###############################################################

rm(list = ls())

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
mg<-baseestr8reg$`09 - Minas Gerais`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtmg<-baseal8reg$`09 - Minas Gerais` ## Arquivo "cru", saída direta da rotina da base por rotação
dbmg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/09_params_mg.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- mg$Total.de.ocupados/1000
se_db <- mg$sd_o/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO SEM ERRO AMOSTRAL

source("data/funcoes/02_modelo_bsm.R")

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
run_mg<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.modelo_bsm(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_mg <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_mg[[i]][["fit"]][["par"]]), 4)
      convergence <- run_mg[[i]][["fit"]][["convergence"]]
      log_like <- run_mg[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)


colnames(ini_mg) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini",
                      "level","slope","seasonal","irregular",
                      "convergence","log_like")

## Após iniciais, seleção do modelo:

estimacao_mg <- run_mg[[which(
  ini_mg$log_like == min(ini_mg$log_like[ini_mg$convergence == 0], na.rm = TRUE) &
    ini_mg$convergence == 0
)]]

# Verificando a convergência

conver_mg<-rbind(estimacao_mg$fit$convergence)
colnames(conver_mg)<-c("convergence")

# Parâmetros estimados:

parametros_mg<-rbind(c(round(exp(estimacao_mg$fit$par),4)))
row.names(parametros_mg)<-c("BSM")
colnames(parametros_mg)<-c("Level","Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AIC_mg<-rbind(2*(estimacao_mg$fit$value)+2*5)
colnames(AIC_mg)<-"AIC"

BIC_mg<-2*(estimacao_mg$fit$value)+2*5*log(estimacao_mg$T)

# Matriz Hessiana

all(eigen(estimacao_mg$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_mg<-list(estimacao_mg)
testes_mg<-sapply(lista_mg, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                               round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                               teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_mg<-t(testes_mg)
row.names(testes_mg)<-c("BSM")
colnames(testes_mg)<-c("Shapiro","Box","H")
resultadosmg<-cbind(conver_mg, parametros_mg, testes_mg, AIC_mg, BIC_mg)
resultadosmg

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_mg <- window(ts.union(
  ts(estimacao_mg$ts.original, start = 2012, frequency = 4),
  ts(estimacao_mg$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_mg, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Desocupação: design-based",
                                "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_mg.cv <- window(ts.union(
  ts((estimacao_mg$cv.original) * 100, start = 2012, frequency = 4),
  ts(estimacao_mg$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_mg.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE

figtend_mg<-window(ts.union(ts(estimacao_mg$ts.original, start = 2012, frequency = 4),ts(estimacao_mg$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_mg<-window(ts.union(ts(estimacao_mg$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_mg<-window(ts.union(ts(estimacao_mg$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(3, 1), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_mg, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Ocupação: design-based",
                                "Tendência da ocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_mg, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_mg, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Ocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte (Sem Erro Amostral)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/9_semEAocup_8reg/09_mod_mg.Rdata")




