################################################################################
##                          MODELO BSM UNIVARIADO                             ##
################################################################################

library(dlm)
library(tidyverse)
library(parallel)
options(scipen=999)

# Neste script a grande diferença é deixar variar o nível da tendência na matriz W
  # Um novo param deve ser incluído em cada grid

### MODELO BH ##################################################################
rm(list = ls())

## Parâmetros do modelo UCM (referência para o grid): 26.15096; 0.0001269569; 75.98691

# Modelos para BH: AR(1) e MA(1)

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

## Dados:

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
bh<-baseestr0424$`01-Belo Horizonte`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtbh<-baseal0424$`01-Belo Horizonte` ## Arquivo "cru", saída direta da rotina da base por rotação
dbbh<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/01_params_bh.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- bh$Total.de.desocupados/1000
se_db<- bh$sd_d/1000
cv_db <- se_db/y

# Parâmetros iniciais:
par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-seq(-5,5,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

##### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")

phi1_ar1 <- dbbh[["mod_ar1"]][["phi1_ar1_dbh"]]

# Input dos parâmetros iniciais do modelo

grid_ar1<-grid_error[-188,] #Cortei porque não estava convergindo

# Antes de rodar o modelo com processamento paralelo, recomenda-se testar os parâmetros

source("data/funcoes/17_rodar_grid_ar1.R")
start_time <- Sys.time()
run_ar1bh <- rodar_grid_ar1(y, grid_ar1, f.estrutural_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1bh_ini<-run_ar1bh$resultados

# Avaliação das iterações:

ini_ar1_bh <- cbind(
  round(exp(grid_ar1), 5),
  do.call(rbind, lapply(1:nrow(grid_ar1), function(i) {
    tryCatch({
      params <- round(exp(mod_ar1bh_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ar1bh_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ar1bh_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ar1_bh) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                 "level","slope","seasonal","irregular", "sampl_error",
                 "convergence","log_like")

ar1_itbh <- ini_ar1_bh[complete.cases(ini_ar1_bh), ]

## Seleção do modelo:

ar1_bh<- mod_ar1bh_ini[[which(ini_ar1_bh$log_like==min(ini_ar1_bh$log_like,na.rm = TRUE))]]

# Verificando a convergência

conver_ar1<-rbind(ar1_bh$fit$convergence)
colnames(conver_ar1)<-c("convergence") 

# Parâmetros estimados:

parametros_ar1<-rbind(c(round(exp(ar1_bh$fit$par),5)))
row.names(parametros_ar1)<-c("BSM_error")
colnames(parametros_ar1)<-c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1<-rbind(2*(ar1_bh$fit$value)+2*5)
colnames(AIC_ar1)<-"AIC"

BIC_ar1<-2*(ar1_bh$fit$value)+2*5*log(ar1_bh$T)

# Matriz Hessiana

all(eigen(ar1_bh$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ar1<-list(ar1_bh)
testes_ar1<-sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],5),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1<-t(testes_ar1)
row.names(testes_ar1)<-c("BSM_error")
colnames(testes_ar1)<-c("Shapiro","Box","H")

resultadosbh_ar1<-cbind(conver_ar1,parametros_ar1,testes_ar1,AIC_ar1,BIC_ar1)
resultadosbh_ar1

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_bh$ts.original, start = 2012, frequency = 4),
  ts(ar1_bh$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_ar1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("Desocupação: design-based",
                             "Sinal da desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_bh$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_bh$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_ar1.cv, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)



##### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")

theta1_ma1 <- dbbh[["mod_ar1"]][["phi1_ar1_dbh"]]

# Input dos parâmetros iniciais do modelo

grid_ma1<-grid_error

# Rodando os parâmetros iniciais do modelo:

source("data/funcoes/18_rodar_grid_ma1.R")

start_time <- Sys.time()
run_ma1bh <- rodar_grid_ma1(y, grid_ma1, f.estrutural_ma1)
end_time <- Sys.time()
end_time - start_time

mod_ma1bh_ini<-run_ma1bh$resultados

# Avaliação das iterações:

ini_ma1_bh <- cbind(
  round(exp(grid_ma1), 5),
  do.call(rbind, lapply(1:nrow(grid_ma1), function(i) {
    tryCatch({
      params <- round(exp(mod_ma1bh_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ma1bh_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ma1bh_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ma1_bh) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                          "level","slope","seasonal","irregular", "sampl_error",
                          "convergence","log_like")

ma1_itbh <- ini_ma1_bh[complete.cases(ini_ma1_bh), ]

## Seleção do modelo:

ma1_bh<- mod_ma1bh_ini[[which(ini_ma1_bh$log_like==min(ini_ma1_bh$log_like,na.rm = TRUE))]]

# Verificando a convergência

conver_ma1<-rbind(ma1_bh$fit$convergence)
colnames(conver_ma1)<-c("convergence") 

# Parâmetros estimados:

parametros_ma1<-rbind(c(round(exp(ma1_bh$fit$par),5)))
row.names(parametros_ma1)<-c("BSM_error")
colnames(parametros_ma1)<-c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1<-rbind(2*(ma1_bh$fit$value)+2*5)
colnames(AIC_ma1)<-"AIC"

BIC_ma1<-2*(ma1_bh$fit$value)+2*5*log(ma1_bh$T)

# Matriz Hessiana

all(eigen(ma1_bh$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ma1<-list(ma1_bh)
testes_ma1<-sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],5),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1<-t(testes_ma1)
row.names(testes_ma1)<-c("BSM_error")
colnames(testes_ma1)<-c("Shapiro","Box","H")

resultadosbh_ma1<-cbind(conver_ma1,parametros_ma1,testes_ma1,AIC_ma1,BIC_ma1)
resultadosbh_ma1

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_ma1 <- window(ts.union(
  ts(ma1_bh$ts.original, start = 2012, frequency = 4),
  ts(ma1_bh$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_ma1, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("Desocupação: design-based",
                             "Sinal da desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_bh$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_bh$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_ma1.cv, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte (MA1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# Gráfico conjunto:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_bh <- window(ts.union(
  ts(ma1_bh$ts.original, start = 2012, frequency = 4),
  ts(ar1_bh$ts.signal, start = 2012, frequency = 4),
  ts(ma1_bh$ts.signal, start = 2012, frequency = 4) 
), start=c(2013,3))
plot(fig_bh, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("Desocupação: design-based",
                             "Sinal da desocupação AR(1): model-based",
                             "Sinal da Desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_bh.cv <- window(ts.union(
  ts((ma1_bh$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_bh$cv.signal, start = 2012, frequency = 4),
  ts(ma1_bh$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_bh.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados AR(1): model-based",
                             "Sinal CV desocupados MA(1): model-based"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte (todos os modelos)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

# save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/01_mod_bh.Rdata")

### ENTORNO METROPOLITANO ###############################################
rm(list = ls())

## Parâmetros do modelo UCM (referência para o grid): 106.7323; 0.00348154; 106.8907

# Modelos para Entorno: AR(1); MA(1), ARMA(1,1)

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

## Carregando bases e definindo objeto para o Entorno

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
ent<-baseestr0424$`02-Entorno metropolitano de BH`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtent<-baseal0424$`02-Entorno metropolitano de BH` ## Arquivo "cru", saída direta da rotina da base por rotação
dbent<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/02_params_ent.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- (ent$Total.de.desocupados)/1000
se_db <- (ent$sd_d)/1000
cv_db <- se_db/y

# Parâmetros iniciais:
  # Realizei várias alterações nos iniciais, visto que alguns estavam quebrando o cálculo

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-seq(-3,6,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbent[["mod_ar1"]][["phi1_ar1_dent"]]
grid_ar1 <- grid_error[-c(91,158,254),]

# Rodando o modelo

source("data/funcoes/17_rodar_grid_ar1.R")
start_time <- Sys.time()
run_ar1ent <- rodar_grid_ar1(y, grid_ar1, f.estrutural_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1ent_ini <- run_ar1ent$resultados

# Avaliação das iterações:
ini_ar1_ent <- cbind(
  round(exp(grid_ar1), 5),
  do.call(rbind, lapply(1:nrow(grid_ar1), function(i) {
    tryCatch({
      params <- round(exp(mod_ar1ent_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ar1ent_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ar1ent_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ar1_ent) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ar1_ent <- mod_ar1ent_ini[[which(
  ini_ar1_ent$log_like == min(ini_ar1_ent$log_like[ini_ar1_ent$convergence == 0], na.rm = TRUE) & 
    ini_ar1_ent$convergence == 0
)]]

# Verificando a convergência

conver_ar1 <- rbind(ar1_ent$fit$convergence)
colnames(conver_ar1) <- c("convergence")

# Parâmetros estimados:

parametros_ar1 <- rbind(c(round(exp(ar1_ent$fit$par), 5)))
row.names(parametros_ar1) <- c("BSM_error")
colnames(parametros_ar1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1 <- rbind(2*(ar1_ent$fit$value) + 2*5)
colnames(AIC_ar1) <- "AIC"

BIC_ar1 <- 2*(ar1_ent$fit$value) + 2*5*log(ar1_ent$T)

# Matriz Hessiana

all(eigen(ar1_ent$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ar1 <- list(ar1_ent)
testes_ar1 <- sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1 <- t(testes_ar1)
row.names(testes_ar1) <- c("BSM_error")
colnames(testes_ar1) <- c("Shapiro", "Box", "H")

resultadosent_ar1 <- cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
resultadosent_ar1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_ent$ts.original, start = 2012, frequency = 4),
  ts(ar1_ent$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("Desocupação: design-based",
                             "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_ent$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_ent$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("02 - Entorno Metropolitano de Belo Horizonte (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)



#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbent[["mod_ma1"]][["theta1_ma1_dent"]]
grid_ma1 <- grid_error[-c(91,158,254),]

# Rodando o modelo

source("data/funcoes/18_rodar_grid_ma1.R")
start_time <- Sys.time()
run_ma1ent <- rodar_grid_ma1(y, grid_ma1, f.estrutural_ma1)
end_time <- Sys.time()
end_time - start_time

mod_ma1ent_ini <- run_ma1ent$resultados

# Avaliação das iterações:
ini_ma1_ent <- cbind(
  round(exp(grid_ma1), 5),
  do.call(rbind, lapply(1:nrow(grid_ma1), function(i) {
    tryCatch({
      params <- round(exp(mod_ma1ent_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ma1ent_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ma1ent_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ma1_ent) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

ma1_itent <- ini_ma1_ent[complete.cases(ini_ma1_ent), ]

## Seleção do modelo:

ma1_ent <- mod_ma1ent_ini[[which(ini_ma1_ent$log_like == min(ini_ma1_ent$log_like, na.rm = TRUE))]]

# Verificando a convergência

conver_ma1 <- rbind(ma1_ent$fit$convergence)
colnames(conver_ma1) <- c("convergence")

# Parâmetros estimados:

parametros_ma1 <- rbind(c(round(exp(ma1_ent$fit$par), 5)))
row.names(parametros_ma1) <- c("BSM_error")
colnames(parametros_ma1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1 <- rbind(2*(ma1_ent$fit$value) + 2*5)
colnames(AIC_ma1) <- "AIC"

BIC_ma1 <- 2*(ma1_ent$fit$value) + 2*5*log(ma1_ent$T)

# Matriz Hessiana

all(eigen(ma1_ent$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ma1 <- list(ma1_ent)
testes_ma1 <- sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1 <- t(testes_ma1)
row.names(testes_ma1) <- c("BSM_error")
colnames(testes_ma1) <- c("Shapiro", "Box", "H")

resultadosent_ma1 <- cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
resultadosent_ma1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ma1 <- window(ts.union(
  ts(ma1_ent$ts.original, start = 2012, frequency = 4),
  ts(ma1_ent$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("Desocupação: design-based",
                             "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_ent$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_ent$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("02 - Entorno Metropolitano de Belo Horizonte (MA1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)



#### MODELO ARMA(1,1)

source("data/funcoes/15_estrutural_ARMA11.R")
phi1_arma11<- dbent[["mod_arma11"]][["phi1_arma11_dent"]]
theta1_arma11 <- dbent[["mod_arma11"]][["theta1_arma11_dent"]]
grid_arma11 <- grid_error[-c(91,158,254),]

# Rodando o modelo

source("data/funcoes/19_rodar_grid_arma11.R")
start_time <- Sys.time()
run_arma11ent <- rodar_grid_arma11(y, grid_arma11, f.estrutural_arma11)
end_time <- Sys.time()
end_time - start_time

mod_arma11ent_ini <- run_arma11ent$resultados

# Avaliação das iterações:
ini_arma11_ent <- cbind(
  round(exp(grid_arma11), 5),
  do.call(rbind, lapply(1:nrow(grid_arma11), function(i) {
    tryCatch({
      params <- round(exp(mod_arma11ent_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_arma11ent_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_arma11ent_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_arma11_ent) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                              "level","slope","seasonal","irregular", "sampl_error",
                              "convergence","log_like")

arma11_itent <- ini_arma11_ent[complete.cases(ini_arma11_ent), ]

## Seleção do modelo:

arma11_ent <- mod_arma11ent_ini[[which(ini_arma11_ent$log_like == min(ini_arma11_ent$log_like, na.rm = TRUE))]]

# Verificando a convergência

conver_arma11 <- rbind(arma11_ent$fit$convergence)
colnames(conver_arma11) <- c("convergence")

# Parâmetros estimados:

parametros_arma11 <- rbind(c(round(exp(arma11_ent$fit$par), 5)))
row.names(parametros_arma11) <- c("BSM_error")
colnames(parametros_arma11) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_arma11 <- rbind(2*(arma11_ent$fit$value) + 2*5)
colnames(AIC_arma11) <- "AIC"

BIC_arma11 <- 2*(arma11_ent$fit$value) + 2*5*log(arma11_ent$T)

# Matriz Hessiana

all(eigen(arma11_ent$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_arma11 <- list(arma11_ent)
testes_arma11 <- sapply(lista_arma11, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_arma11 <- t(testes_arma11)
row.names(testes_arma11) <- c("BSM_error")
colnames(testes_arma11) <- c("Shapiro", "Box", "H")

resultadosent_arma11 <- cbind(conver_arma11, parametros_arma11, testes_arma11, AIC_arma11, BIC_arma11)
resultadosent_arma11

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_arma11 <- window(ts.union(
  ts(arma11_ent$ts.original, start = 2012, frequency = 4),
  ts(arma11_ent$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_arma11, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                             "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_arma11.cv <- window(ts.union(
  ts((arma11_ent$cv.original) * 100, start = 2012, frequency = 4),
  ts(arma11_ent$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_arma11.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("02 - Entorno Metropolitano de Belo Horizonte (ARMA11)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

#### GRÁFICO CONJUNTO

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_ent <- window(ts.union(
  ts(ma1_ent$ts.original, start = 2012, frequency = 4),
  ts(ar1_ent$ts.signal, start = 2012, frequency = 4),
  ts(ma1_ent$ts.signal, start = 2012, frequency = 4) 
), start=c(2013,3))
plot(fig_ent, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("Desocupação: design-based",
                             "Sinal da desocupação AR(1): model-based",
                             "Sinal da Desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ent.cv <- window(ts.union(
  ts((ma1_ent$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_ent$cv.signal, start = 2012, frequency = 4),
  ts(ma1_ent$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_ent.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados AR(1): model-based",
                             "Sinal CV desocupados MA(1): model-based"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("02 - Entorno Metropolitano de Belo Horizonte (AR e MA)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

#save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/02_mod_ent.Rdata")

### COLAR METROPOLITANO DE BH ##################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid): 3.830586; 0.00000616888; 10.51971
# Modelos para Colar: AR(1); ARMA(1,1)

## Funções e dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
col<-baseestr0424$`03-Colar metropolitano de BH`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtcol<-baseal0424$`03-Colar metropolitano de BH` ## Arquivo "cru", saída direta da rotina da base por rotação
dbcol<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/03_params_col.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- col$Total.de.desocupados/1000
se_db <- col$sd_d/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-seq(-5,5,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbcol[["mod_ar1"]][["phi1_ar1_dcol"]]
grid_ar1 <- grid_error[-188,]

# Rodando o modelo

source("data/funcoes/17_rodar_grid_ar1.R")
start_time <- Sys.time()
run_ar1col <- rodar_grid_ar1(y, grid_ar1, f.estrutural_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1col_ini <- run_ar1col$resultados

# Avaliação das iterações:
ini_ar1_col <- cbind(
  round(exp(grid_ar1), 5),
  do.call(rbind, lapply(1:nrow(grid_ar1), function(i) {
    tryCatch({
      params <- round(exp(mod_ar1col_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ar1col_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ar1col_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ar1_col) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

ar1_itcol <- subset(ini_ar1_col, convergence == 0)

## Seleção do modelo:

ar1_col <- mod_ar1col_ini[[which(ini_ar1_col$log_like == min(ini_ar1_col$log_like, na.rm = TRUE))]]

# Verificando a convergência

conver_ar1 <- rbind(ar1_col$fit$convergence)
colnames(conver_ar1) <- c("convergence")

# Parâmetros estimados:

parametros_ar1 <- rbind(c(round(exp(ar1_col$fit$par), 5)))
row.names(parametros_ar1) <- c("BSM_error")
colnames(parametros_ar1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1 <- rbind(2*(ar1_col$fit$value) + 2*5)
colnames(AIC_ar1) <- "AIC"

BIC_ar1 <- 2*(ar1_col$fit$value) + 2*5*log(ar1_col$T)

# Matriz Hessiana

all(eigen(ar1_col$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ar1 <- list(ar1_col)
testes_ar1 <- sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1 <- t(testes_ar1)
row.names(testes_ar1) <- c("BSM_error")
colnames(testes_ar1) <- c("Shapiro", "Box", "H")

resultadoscol_ar1 <- cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
resultadoscol_ar1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_col$ts.original, start = 2012, frequency = 4),
  ts(ar1_col$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                             "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_col$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_col$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Colar Metropolitano de Belo Horizonte (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)



#### MODELO ARMA (1,1)

source("data/funcoes/15_estrutural_ARMA11.R")
phi1_arma11 <- dbcol[["mod_arma11"]][["phi1_arma11_dcol"]]
theta1_arma11 <- dbcol[["mod_arma11"]][["theta1_arma11_dcol"]]
grid_arma11 <- grid_error[-188,]

# Rodando o modelo

source("data/funcoes/19_rodar_grid_arma11.R")
start_time <- Sys.time()
run_arma11col <- rodar_grid_arma11(y, grid_arma11, f.estrutural_arma11)
end_time <- Sys.time()
end_time - start_time

mod_arma11col_ini <- run_arma11col$resultados

# Avaliação das iterações:
ini_arma11_col <- cbind(
  round(exp(grid_arma11), 5),
  do.call(rbind, lapply(1:nrow(grid_arma11), function(i) {
    tryCatch({
      params <- round(exp(mod_arma11col_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_arma11col_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_arma11col_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_arma11_col) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                              "level","slope","seasonal","irregular", "sampl_error",
                              "convergence","log_like")

arma11_itcol <- ini_arma11_col[complete.cases(ini_arma11_col), ]

## Seleção do modelo:

arma11_col <- mod_arma11col_ini[[which(ini_arma11_col$log_like == min(ini_arma11_col$log_like, na.rm = TRUE))]]

# Verificando a convergência

conver_arma11 <- rbind(arma11_col$fit$convergence)
colnames(conver_arma11) <- c("convergence")

# Parâmetros estimados:

parametros_arma11 <- rbind(c(round(exp(arma11_col$fit$par), 5)))
row.names(parametros_arma11) <- c("BSM_error")
colnames(parametros_arma11) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_arma11 <- rbind(2*(arma11_col$fit$value) + 2*5)
colnames(AIC_arma11) <- "AIC"

BIC_arma11 <- 2*(arma11_col$fit$value) + 2*5*log(arma11_col$T)

# Matriz Hessiana

all(eigen(arma11_col$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_arma11 <- list(arma11_col)
testes_arma11 <- sapply(lista_arma11, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_arma11 <- t(testes_arma11)
row.names(testes_arma11) <- c("BSM_error")
colnames(testes_arma11) <- c("Shapiro", "Box", "H")

resultadoscol_arma11 <- cbind(conver_arma11, parametros_arma11, testes_arma11, AIC_arma11, BIC_arma11)
resultadoscol_arma11

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_arma11 <- window(ts.union(
  ts(arma11_col$ts.original, start = 2012, frequency = 4),
  ts(arma11_col$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_arma11, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_arma11.cv <- window(ts.union(
  ts((arma11_col$cv.original) * 100, start = 2012, frequency = 4),
  ts(arma11_col$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_arma11.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                                "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Entorno Metropolitano de Belo Horizonte (ARMA11)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


#### GRÁFICO CONJUNTO:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_col <- window(ts.union(
  ts(ar1_col$ts.original, start = 2012, frequency = 4),
  ts(ar1_col$ts.signal, start = 2012, frequency = 4),
  ts(arma11_col$ts.signal, start = 2012, frequency = 4) 
), start=c(2013,3))
plot(fig_col, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                             "Sinal da desocupação AR(1): model-based",
                             "Sinal da Desocupação ARMA(1,1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_col.cv <- window(ts.union(
  ts((ar1_col$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_col$cv.signal, start = 2012, frequency = 4),
  ts(arma11_col$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_col.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados AR(1): model-based",
                             "Sinal CV desocupados ARMA(1,1): model-based"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Entorno Metropolitano de Belo Horizonte (AR(1) e ARMA(11))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/03_mod_col.Rdata")

### RIDE de Brasília em Minas ##################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid): 0.03001628; 0.004274386; 1.105705
# Modelos para Ride: AR(1); MA(1), ARMA(1,1)

## Funções e upload dos dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

## Carregando bases e definindo objeto para RIDE

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
rid<-baseestr0424$`04-RIDE de Brasília em Minas`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtrid<-baseal0424$`04-RIDE de Brasília em Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbrid<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/04_params_rid.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- rid$Total.de.desocupados/1000
se_db <- rid$sd_d/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-seq(-5,5,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbrid[["mod_ar1"]][["phi1_ar1_drid"]]
grid_ar1<-grid_error[-188,]

# Rodando o modelo

source("data/funcoes/17_rodar_grid_ar1.R")
start_time <- Sys.time()
run_ar1rid <- rodar_grid_ar1(y, grid_ar1, f.estrutural_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1rid_ini <- run_ar1rid$resultados

# Avaliação das iterações:
ini_ar1_rid <- cbind(
  round(exp(grid_ar1), 5),
  do.call(rbind, lapply(1:nrow(grid_ar1), function(i) {
    tryCatch({
      params <- round(exp(mod_ar1rid_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ar1rid_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ar1rid_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ar1_rid) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

ar1_itrid <- subset(ini_ar1_rid, convergence == 0)

## Seleção do modelo:

ar1_rid <- mod_ar1rid_ini[[which(ini_ar1_rid$log_like == min(ini_ar1_rid$log_like, na.rm = TRUE))]]

# Verificando a convergência

conver_ar1 <- rbind(ar1_rid$fit$convergence)
colnames(conver_ar1) <- c("convergence")

# Parâmetros estimados:

parametros_ar1 <- rbind(c(round(exp(ar1_rid$fit$par), 5)))
row.names(parametros_ar1) <- c("BSM_error")
colnames(parametros_ar1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1 <- rbind(2*(ar1_rid$fit$value) + 2*5)
colnames(AIC_ar1) <- "AIC"

BIC_ar1 <- 2*(ar1_rid$fit$value) + 2*5*log(ar1_rid$T)

# Matriz Hessiana

all(eigen(ar1_rid$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ar1 <- list(ar1_rid)
testes_ar1 <- sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1 <- t(testes_ar1)
row.names(testes_ar1) <- c("BSM_error")
colnames(testes_ar1) <- c("Shapiro", "Box", "H")  
resultadosrid_ar1 <- cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
resultadosrid_ar1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_rid$ts.original, start = 2012, frequency = 4),
  ts(ar1_rid$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_rid$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_rid$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("04 - RIDE de Brasília em Minas (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)



#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbrid[["mod_ma1"]][["theta1_ma1_drid"]]
grid_ma1<-grid_error[-188,]

# Rodando o modelo

source("data/funcoes/18_rodar_grid_ma1.R")
start_time <- Sys.time()
run_ma1rid <- rodar_grid_ma1(y, grid_ma1, f.estrutural_ma1)
end_time <- Sys.time()
end_time - start_time

mod_ma1rid_ini <- run_ma1rid$resultados

# Avaliação das iterações:
ini_ma1_rid <- cbind(
  round(exp(grid_ma1), 5),
  do.call(rbind, lapply(1:nrow(grid_ma1), function(i) {
    tryCatch({
      params <- round(exp(mod_ma1rid_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ma1rid_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ma1rid_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ma1_rid) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

ma1_itrid <- subset(ini_ma1_rid, convergence == 0)

## Seleção do modelo:

ma1_rid <- mod_ma1rid_ini[[which(ini_ma1_rid$log_like == min(ini_ma1_rid$log_like, na.rm = TRUE))]]

# Verificando a convergência

conver_ma1 <- rbind(ma1_rid$fit$convergence)
colnames(conver_ma1) <- c("convergence")

# Parâmetros estimados:

parametros_ma1 <- rbind(c(round(exp(ma1_rid$fit$par), 5)))
row.names(parametros_ma1) <- c("BSM_error")
colnames(parametros_ma1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1 <- rbind(2*(ma1_rid$fit$value) + 2*5)
colnames(AIC_ma1) <- "AIC"

BIC_ma1 <- 2*(ma1_rid$fit$value) + 2*5*log(ma1_rid$T)

# Matriz Hessiana

all(eigen(ma1_rid$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ma1 <- list(ma1_rid)
testes_ma1 <- sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1 <- t(testes_ma1)
row.names(testes_ma1) <- c("BSM_error")
colnames(testes_ma1) <- c("Shapiro", "Box", "H")  
resultadosrid_ma1 <- cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
resultadosrid_ma1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ma1 <- window(ts.union(
  ts(ma1_rid$ts.original, start = 2012, frequency = 4),
  ts(ma1_rid$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_rid$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_rid$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("04 - RIDE de Brasília em Minas (MA1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)



#### MODELO ARMA(1,1)

source("data/funcoes/15_estrutural_ARMA11.R")
phi1_arma11 <- dbrid[["mod_arma11"]][["phi1_arma11_drid"]]
theta1_arma11 <- dbrid[["mod_arma11"]][["theta1_arma11_drid"]]
grid_arma11 <- grid_error[-188,]

# Rodando o modelo

source("data/funcoes/19_rodar_grid_arma11.R")
start_time <- Sys.time()
run_arma11rid <- rodar_grid_arma11(y, grid_arma11, f.estrutural_arma11)
end_time <- Sys.time()
end_time - start_time

mod_arma11rid_ini <- run_arma11rid$resultados

# Avaliação das iterações:
ini_arma11_rid <- cbind(
  round(exp(grid_arma11), 5),
  do.call(rbind, lapply(1:nrow(grid_arma11), function(i) {
    tryCatch({
      params <- round(exp(mod_arma11rid_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_arma11rid_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_arma11rid_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_arma11_rid) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                              "level","slope","seasonal","irregular", "sampl_error",
                              "convergence","log_like")

arma11_itrid <- subset(ini_arma11_rid, convergence == 0)

## Seleção do modelo:

arma11_rid <- mod_arma11rid_ini[[which(ini_arma11_rid$log_like == min(ini_arma11_rid$log_like, na.rm = TRUE))]]

# Verificando a convergência

conver_arma11 <- rbind(arma11_rid$fit$convergence)
colnames(conver_arma11) <- c("convergence")

# Parâmetros estimados:

parametros_arma11 <- rbind(c(round(exp(arma11_rid$fit$par), 5)))
row.names(parametros_arma11) <- c("BSM_error")
colnames(parametros_arma11) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_arma11 <- rbind(2*(arma11_rid$fit$value) + 2*5)
colnames(AIC_arma11) <- "AIC"

BIC_arma11 <- 2*(arma11_rid$fit$value) + 2*5*log(arma11_rid$T)

# Matriz Hessiana

all(eigen(arma11_rid$fit$hessian, only.values = TRUE)$values > 0) # true

# Diagnosticando os resíduos

lista_arma11 <- list(arma11_rid)
testes_arma11 <- sapply(lista_arma11, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_arma11 <- t(testes_arma11)
row.names(testes_arma11) <- c("BSM_error")
colnames(testes_arma11) <- c("Shapiro", "Box", "H")  
resultadosrid_arma11 <- cbind(conver_arma11, parametros_arma11, testes_arma11, AIC_arma11, BIC_arma11)
resultadosrid_arma11

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_arma11 <- window(ts.union(
  ts(arma11_rid$ts.original, start = 2012, frequency = 4),
  ts(arma11_rid$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_arma11, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Desocupação: design-based",
                                "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_arma11.cv <- window(ts.union(
  ts((arma11_rid$cv.original) * 100, start = 2012, frequency = 4),
  ts(arma11_rid$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_arma11.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("04 - RIDE de Brasília em Minas (ARMA11)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# GRÁFICO CONFUNTO COM TODAS AS VARIÁVEIS

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_rid <- window(ts.union(
  ts(ma1_rid$ts.original, start = 2012, frequency = 4),
  ts(ar1_rid$ts.signal, start = 2012, frequency = 4),
  ts(ma1_rid$ts.signal, start = 2012, frequency = 4),
  ts(arma11_rid$ts.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_rid, plot.type = "single", col = c(1,4,2,3), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("Desocupação: design-based",
                             "Sinal da desocupação AR(1): model-based",
                             "Sinal da Desocupação MA(1): model-based",
                             "Sinal da Desocupação ARMA(1,1): model-based"),
       lty = c(1,1,1,1), col = c(1,4,2,3), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_rid.cv <- window(ts.union(
  ts((ma1_rid$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_rid$cv.signal, start = 2012, frequency = 4),
  ts(ma1_rid$cv.signal, start = 2012, frequency = 4),
  ts(arma11_rid$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_rid.cv, plot.type = "single", col = c(1,4,2,3), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados AR(1): model-based",
                             "Sinal CV desocupados MA(1): model-based",
                             "Sinal CV desocupados ARMA(1,1): model-based"),
       lty = c(1,1,1,1), col = c(1,4,2,3), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("04 - RIDE de Brasília em Minas (AR(1), MA(1) e ARMA(1,1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# Salvando o .Rdata

# save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/04_mod_rid.Rdata")

### SUL DE MINAS ###############################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid): 56.85534; 0.0008044187; 90.04413
# Modelos para Sul: AR(1); MA(1), ARMA(1,1)

## Funções e base de dados:

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
sul<-baseestr0424$`05-Sul de Minas`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtsul<-baseal0424$`05-Sul de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbsul<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/05_params_sul.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- sul$Total.de.desocupados/1000
se_db <- sul$sd_d/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-seq(-3,6,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbsul[["mod_ar1"]][["phi1_ar1_dsul"]]
grid_ar1<-grid_error[-c(33),]

# Rodando o modelo

source("data/funcoes/17_rodar_grid_ar1.R")
start_time <- Sys.time()
run_ar1sul <- rodar_grid_ar1(y, grid_ar1, f.estrutural_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1sul_ini <- run_ar1sul$resultados

# Avaliação das iterações:
ini_ar1_sul <- cbind(
  round(exp(grid_ar1), 5),
  do.call(rbind, lapply(1:nrow(grid_ar1), function(i) {
    tryCatch({
      params <- round(exp(mod_ar1sul_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ar1sul_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ar1sul_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ar1_sul) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ar1_sul <- mod_ar1sul_ini[[which(
  ini_ar1_sul$log_like == min(ini_ar1_sul$log_like[ini_ar1_sul$convergence == 0], na.rm = TRUE) & 
    ini_ar1_sul$convergence == 0
)]]

# Verificando a convergência

conver_ar1 <- rbind(ar1_sul$fit$convergence)
colnames(conver_ar1) <- c("convergence")

# Parâmetros estimados:

parametros_ar1 <- rbind(c(round(exp(ar1_sul$fit$par), 5)))
row.names(parametros_ar1) <- c("BSM_error")
colnames(parametros_ar1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1 <- rbind(2*(ar1_sul$fit$value) + 2*5)
colnames(AIC_ar1) <- "AIC"

BIC_ar1 <- 2*(ar1_sul$fit$value) + 2*5*log(ar1_sul$T)

# Matriz Hessiana

all(eigen(ar1_sul$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ar1 <- list(ar1_sul)
testes_ar1 <- sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1 <- t(testes_ar1)
row.names(testes_ar1) <- c("BSM_error")
colnames(testes_ar1) <- c("Shapiro", "Box", "H")  
resultadossul_ar1 <- cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
resultadossul_ar1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_sul$ts.original, start = 2012, frequency = 4),
  ts(ar1_sul$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_sul$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_sul$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05 - Sul de Minas (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)



#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbsul[["mod_ma1"]][["theta1_ma1_dsul"]]
grid_ma1<-grid_error[-c(33),]

# Rodando o modelo

source("data/funcoes/18_rodar_grid_ma1.R")
start_time <- Sys.time()
run_ma1sul <- rodar_grid_ma1(y, grid_ma1, f.estrutural_ma1)
end_time <- Sys.time()
end_time - start_time

mod_ma1sul_ini <- run_ma1sul$resultados

# Avaliação das iterações:
ini_ma1_sul <- cbind(
  round(exp(grid_ma1), 5),
  do.call(rbind, lapply(1:nrow(grid_ma1), function(i) {
    tryCatch({
      params <- round(exp(mod_ma1sul_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ma1sul_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ma1sul_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ma1_sul) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ma1_sul <- mod_ma1sul_ini[[which(
  ini_ma1_sul$log_like == min(ini_ma1_sul$log_like[ini_ma1_sul$convergence == 0], na.rm = TRUE) & 
    ini_ma1_sul$convergence == 0
)]]

# Verificando a convergência

conver_ma1 <- rbind(ma1_sul$fit$convergence)
colnames(conver_ma1) <- c("convergence")

# Parâmetros estimados:

parametros_ma1 <- rbind(c(round(exp(ma1_sul$fit$par), 5)))
row.names(parametros_ma1) <- c("BSM_error")
colnames(parametros_ma1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1 <- rbind(2*(ma1_sul$fit$value) + 2*5)
colnames(AIC_ma1) <- "AIC"

BIC_ma1 <- 2*(ma1_sul$fit$value) + 2*5*log(ma1_sul$T)

# Matriz Hessiana

all(eigen(ma1_sul$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ma1 <- list(ma1_sul)
testes_ma1 <- sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1 <- t(testes_ma1)
row.names(testes_ma1) <- c("BSM_error")
colnames(testes_ma1) <- c("Shapiro", "Box", "H")  
resultadossul_ma1 <- cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
resultadossul_ma1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ma1 <- window(ts.union(
  ts(ma1_sul$ts.original, start = 2012, frequency = 4),
  ts(ma1_sul$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_sul$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_sul$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05 - Sul de Minas (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)



#### MODELO ARMA(1,1):

source("data/funcoes/15_estrutural_ARMA11.R")
phi1_arma11 <- dbsul[["mod_arma11"]][["phi1_arma11_dsul"]]
theta1_arma11 <- dbsul[["mod_arma11"]][["theta1_arma11_dsul"]]
grid_arma11<-grid_error[-c(33),]

# Rodando o modelo

source("data/funcoes/19_rodar_grid_arma11.R")
start_time <- Sys.time()
run_arma11sul <- rodar_grid_arma11(y, grid_arma11, f.estrutural_arma11)
end_time <- Sys.time()
end_time - start_time

mod_arma11sul_ini <- run_arma11sul$resultados

# Avaliação das iterações:
ini_arma11_sul <- cbind(
  round(exp(grid_arma11), 5),
  do.call(rbind, lapply(1:nrow(grid_arma11), function(i) {
    tryCatch({
      params <- round(exp(mod_arma11sul_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_arma11sul_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_arma11sul_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_arma11_sul) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                              "level","slope","seasonal","irregular", "sampl_error",
                              "convergence","log_like")

arma11_itsul <- subset(ini_arma11_sul, convergence == 0)

## Seleção do modelo:

arma11_sul <- mod_arma11sul_ini[[which(
  ini_arma11_sul$log_like == min(ini_arma11_sul$log_like[ini_arma11_sul$convergence == 0], na.rm = TRUE) & 
    ini_arma11_sul$convergence == 0
)]]

# Verificando a convergência

conver_arma11 <- rbind(arma11_sul$fit$convergence)
colnames(conver_arma11) <- c("convergence")

# Parâmetros estimados:

parametros_arma11 <- rbind(c(round(exp(arma11_sul$fit$par), 5)))
row.names(parametros_arma11) <- c("BSM_error")
colnames(parametros_arma11) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_arma11 <- rbind(2*(arma11_sul$fit$value) + 2*5)
colnames(AIC_arma11) <- "AIC"

BIC_arma11 <- 2*(arma11_sul$fit$value) + 2*5*log(arma11_sul$T)

# Matriz Hessiana

all(eigen(arma11_sul$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_arma11 <- list(arma11_sul)
testes_arma11 <- sapply(lista_arma11, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_arma11 <- t(testes_arma11)
row.names(testes_arma11) <- c("BSM_error")
colnames(testes_arma11) <- c("Shapiro", "Box", "H")  
resultadossul_arma11 <- cbind(conver_arma11, parametros_arma11, testes_arma11, AIC_arma11, BIC_arma11)
resultadossul_arma11

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_arma11 <- window(ts.union(
  ts(arma11_sul$ts.original, start = 2012, frequency = 4),
  ts(arma11_sul$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_arma11, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_arma11.cv <- window(ts.union(
  ts((arma11_sul$cv.original) * 100, start = 2012, frequency = 4),
  ts(arma11_sul$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_arma11.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05 - Sul de Minas (ARMA(1,1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


## Gráfico conjunto:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_sul <- window(ts.union(
  ts(ma1_sul$ts.original, start = 2012, frequency = 4),
  ts(ar1_sul$ts.signal, start = 2012, frequency = 4),
  ts(ma1_sul$ts.signal, start = 2012, frequency = 4),
  ts(arma11_sul$ts.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_sul, plot.type = "single", col = c(1,4,2,7), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("Desocupação: design-based",
                             "Sinal da desocupação AR(1): model-based",
                             "Sinal da Desocupação MA(1): model-based",
                             "Sinal da Desocupação ARMA(1,1): model-based"),
       lty = c(1,1,1,1), col = c(1,4,2,7), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_sul.cv <- window(ts.union(
  ts((ma1_sul$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_sul$cv.signal, start = 2012, frequency = 4),
  ts(ma1_sul$cv.signal, start = 2012, frequency = 4),
  ts(arma11_sul$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_sul.cv, plot.type = "single", col = c(1,4,2,7), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados AR(1): model-based",
                             "Sinal CV desocupados MA(1): model-based",
                             "Sinal CV desocupados ARMA(1,1): model-based"),
       lty = c(1,1,1,1), col = c(1,4,2,7), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05 - Sul de Minas (AR(1), MA(1) e ARMA(1,1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/05_mod_sul.Rdata")

### TRIÂNGULO MINEIRO ##########################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid):  # 11.70026 # 0.00006308897 # 84.38331
# Modelos para Triângulo: AR(1); MA(1)

## Funções e base de dados:

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
trg<-baseestr0424$`06-Triângulo Mineiro`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dttrg<-baseal0424$`06-Triângulo Mineiro` ## Arquivo "cru", saída direta da rotina da base por rotação
dbtrg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/06_params_trg.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- trg$Total.de.desocupados/1000
se_db <- trg$sd_d/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-seq(-3,6,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbtrg[["mod_ar1"]][["phi1_ar1_dtrg"]]
grid_ar1 <- grid_error

# Rodando o modelo

source("data/funcoes/17_rodar_grid_ar1.R")
start_time <- Sys.time()
run_ar1trg <- rodar_grid_ar1(y, grid_ar1, f.estrutural_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1trg_ini <- run_ar1trg$resultados

# Avaliação das iterações:
ini_ar1_trg <- cbind(
  round(exp(grid_ar1), 5),
  do.call(rbind, lapply(1:nrow(grid_ar1), function(i) {
    tryCatch({
      params <- round(exp(mod_ar1trg_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ar1trg_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ar1trg_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ar1_trg) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ar1_trg <- mod_ar1trg_ini[[which(
  ini_ar1_trg$log_like == min(ini_ar1_trg$log_like[ini_ar1_trg$convergence == 0], na.rm = TRUE) & 
    ini_ar1_trg$convergence == 0
)]]

# Verificando a convergência

conver_ar1 <- rbind(ar1_trg$fit$convergence)
colnames(conver_ar1) <- c("convergence")

# Parâmetros estimados:

parametros_ar1 <- rbind(c(round(exp(ar1_trg$fit$par), 5)))
row.names(parametros_ar1) <- c("BSM_error")
colnames(parametros_ar1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1 <- rbind(2*(ar1_trg$fit$value) + 2*5)
colnames(AIC_ar1) <- "AIC"

BIC_ar1 <- 2*(ar1_trg$fit$value) + 2*5*log(ar1_trg$T)

# Matriz Hessiana

all(eigen(ar1_trg$fit$hessian, only.values = TRUE)$values > 0) # TRUE

# Diagnosticando os resíduos

lista_ar1 <- list(ar1_trg)
testes_ar1 <- sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1 <- t(testes_ar1)
row.names(testes_ar1) <- c("BSM_error")
colnames(testes_ar1) <- c("Shapiro", "Box", "H")

resultadostrg_ar1 <- cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
resultadostrg_ar1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_trg$ts.original, start = 2012, frequency = 4),
  ts(ar1_trg$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                             "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_trg$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_trg$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06 - Triângulo Mineiro (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)



#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbtrg[["mod_ma1"]][["theta1_ma1_dtrg"]]
grid_ma1 <- grid_error

# Rodando o modelo

source("data/funcoes/18_rodar_grid_ma1.R")
start_time <- Sys.time()
run_ma1trg <- rodar_grid_ma1(y, grid_ma1, f.estrutural_ma1)
end_time <- Sys.time()
end_time - start_time

mod_ma1trg_ini <- run_ma1trg$resultados

# Avaliação das iterações:
ini_ma1_trg <- cbind(
  round(exp(grid_ma1), 5),
  do.call(rbind, lapply(1:nrow(grid_ma1), function(i) {
    tryCatch({
      params <- round(exp(mod_ma1trg_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ma1trg_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ma1trg_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ma1_trg) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ma1_trg <- mod_ma1trg_ini[[which(
  ini_ma1_trg$log_like == min(ini_ma1_trg$log_like[ini_ma1_trg$convergence == 0], na.rm = TRUE) & 
    ini_ma1_trg$convergence == 0
)]]

# Verificando a convergência

conver_ma1 <- rbind(ma1_trg$fit$convergence)
colnames(conver_ma1) <- c("convergence")

# Parâmetros estimados:

parametros_ma1 <- rbind(c(round(exp(ma1_trg$fit$par), 5)))
row.names(parametros_ma1) <- c("BSM_error")
colnames(parametros_ma1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1 <- rbind(2*(ma1_trg$fit$value) + 2*5)
colnames(AIC_ma1) <- "AIC"

BIC_ma1 <- 2*(ma1_trg$fit$value) + 2*5*log(ma1_trg$T)

# Matriz Hessiana

all(eigen(ma1_trg$fit$hessian, only.values = TRUE)$values > 0) # FALSE

# Diagnosticando os resíduos

lista_ma1 <- list(ma1_trg)
testes_ma1 <- sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1 <- t(testes_ma1)
row.names(testes_ma1) <- c("BSM_error")
colnames(testes_ma1) <- c("Shapiro", "Box", "H")

resultadostrg_ma1 <- cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
resultadostrg_ma1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ma1 <- window(ts.union(
  ts(ma1_trg$ts.original, start = 2012, frequency = 4),
  ts(ma1_trg$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_trg$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_trg$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06 - Triângulo Mineiro (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# GRÁFICO UNIFICADO

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_trg <- window(ts.union(
  ts(ma1_trg$ts.original, start = 2012, frequency = 4),
  ts(ar1_trg$ts.signal, start = 2012, frequency = 4),
  ts(ma1_trg$ts.signal, start = 2012, frequency = 4) 
), start=c(2013,3))
plot(fig_trg, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                             "Sinal da desocupação AR(1): model-based",
                             "Sinal da Desocupação MA(1): model-based"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_trg.cv <- window(ts.union(
  ts((ma1_trg$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_trg$cv.signal, start = 2012, frequency = 4),
  ts(ma1_trg$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_trg.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados AR(1): model-based",
                             "Sinal CV desocupados MA(1): model-based"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06 - Triângulo Mineiro (AR(1) e MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

#save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/06_mod_trg.Rdata")

### ZONA DA MATA ###############################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid): # 16.0373 # 0.0004886218 # 82.69742
# Modelos para Mata: AR(1); MA(1), ARMA(1,1)

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
mat<-baseestr0424$`07-Mata de Minas Gerais`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtmat<-baseal0424$`07-Mata de Minas Gerais` ## Arquivo "cru", saída direta da rotina da base por rotação
dbmat<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/07_params_mat.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- mat$Total.de.desocupados/1000
se_db <- mat$sd_d/1000
cv_db <- se_db/y

# Parâmetros iniciais:
par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-seq(-3,6,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbmat[["mod_ar1"]][["phi1_ar1_dmat"]]
grid_ar1 <- grid_error[-c(97,213),]

# Rodando o modelo

source("data/funcoes/17_rodar_grid_ar1.R")
start_time <- Sys.time()
run_ar1mat <- rodar_grid_ar1(y, grid_ar1, f.estrutural_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1mat_ini <- run_ar1mat$resultados

# Avaliação das iterações:
ini_ar1_mat <- cbind(
  round(exp(grid_ar1), 5),
  do.call(rbind, lapply(1:nrow(grid_ar1), function(i) {
    tryCatch({
      params <- round(exp(mod_ar1mat_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ar1mat_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ar1mat_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ar1_mat) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ar1_mat <- mod_ar1mat_ini[[which(
  ini_ar1_mat$log_like == min(ini_ar1_mat$log_like[ini_ar1_mat$convergence == 0], na.rm = TRUE) & 
    ini_ar1_mat$convergence == 0
)]]

# Verificando a convergência

conver_ar1 <- rbind(ar1_mat$fit$convergence)
colnames(conver_ar1) <- c("convergence")

# Parâmetros estimados:

parametros_ar1 <- rbind(c(round(exp(ar1_mat$fit$par), 5)))
row.names(parametros_ar1) <- c("BSM_error")
colnames(parametros_ar1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1 <- rbind(2*(ar1_mat$fit$value) + 2*5)
colnames(AIC_ar1) <- "AIC"

BIC_ar1 <- 2*(ar1_mat$fit$value) + 2*5*log(ar1_mat$T)

# Matriz Hessiana

all(eigen(ar1_mat$fit$hessian, only.values = TRUE)$values > 0) # TRUE

# Diagnosticando os resíduos

lista_ar1 <- list(ar1_mat)
testes_ar1 <- sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1 <- t(testes_ar1)
row.names(testes_ar1) <- c("BSM_error")
colnames(testes_ar1) <- c("Shapiro", "Box", "H")

resultadosmat_ar1 <- cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
resultadosmat_ar1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_mat$ts.original, start = 2012, frequency = 4),
  ts(ar1_mat$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_mat$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_mat$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07 - Zona da Mata (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)



#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbmat[["mod_ma1"]][["theta1_ma1_dmat"]]
grid_ma1 <- grid_error[-c(97,213),]

# Rodando o modelo

source("data/funcoes/18_rodar_grid_ma1.R")
start_time <- Sys.time()
run_ma1mat <- rodar_grid_ma1(y, grid_ma1, f.estrutural_ma1)
end_time <- Sys.time()
end_time - start_time

mod_ma1mat_ini <- run_ma1mat$resultados

# Avaliação das iterações:
ini_ma1_mat <- cbind(
  round(exp(grid_ma1), 5),
  do.call(rbind, lapply(1:nrow(grid_ma1), function(i) {
    tryCatch({
      params <- round(exp(mod_ma1mat_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ma1mat_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ma1mat_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ma1_mat) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ma1_mat <- mod_ma1mat_ini[[which(
  ini_ma1_mat$log_like == min(ini_ma1_mat$log_like[ini_ma1_mat$convergence == 0], na.rm = TRUE) & 
    ini_ma1_mat$convergence == 0
)]]

# Verificando a convergência

conver_ma1 <- rbind(ma1_mat$fit$convergence)
colnames(conver_ma1) <- c("convergence")

# Parâmetros estimados:

parametros_ma1 <- rbind(c(round(exp(ma1_mat$fit$par), 5)))
row.names(parametros_ma1) <- c("BSM_error")
colnames(parametros_ma1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1 <- rbind(2*(ma1_mat$fit$value) + 2*5)
colnames(AIC_ma1) <- "AIC"

BIC_ma1 <- 2*(ma1_mat$fit$value) + 2*5*log(ma1_mat$T)

# Matriz Hessiana

all(eigen(ma1_mat$fit$hessian, only.values = TRUE)$values > 0) # FALSE

# Diagnosticando os resíduos

lista_ma1 <- list(ma1_mat)
testes_ma1 <- sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1 <- t(testes_ma1)
row.names(testes_ma1) <- c("BSM_error")
colnames(testes_ma1) <- c("Shapiro", "Box", "H")

resultadosmat_ma1 <- cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
resultadosmat_ma1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ma1 <- window(ts.union(
  ts(ma1_mat$ts.original, start = 2012, frequency = 4),
  ts(ma1_mat$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_mat$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_mat$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07 - Zona da Mata (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)



#### MODELO ARMA(1,1):

source("data/funcoes/15_estrutural_ARMA11.R")
phi1_arma11 <- dbmat[["mod_arma11"]][["phi1_arma11_dmat"]]
theta1_arma11 <- dbmat[["mod_arma11"]][["theta1_arma11_dmat"]]
grid_arma11<-grid_error[-c(97,213),]

# Rodando o modelo

source("data/funcoes/19_rodar_grid_arma11.R")
start_time <- Sys.time()
run_arma11mat <- rodar_grid_arma11(y, grid_arma11, f.estrutural_arma11)
end_time <- Sys.time()
end_time - start_time

mod_arma11mat_ini <- run_arma11mat$resultados

# Avaliação das iterações:
ini_arma11_mat <- cbind(
  round(exp(grid_arma11), 5),
  do.call(rbind, lapply(1:nrow(grid_arma11), function(i) {
    tryCatch({
      params <- round(exp(mod_arma11mat_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_arma11mat_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_arma11mat_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_arma11_mat) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                              "level","slope","seasonal","irregular", "sampl_error",
                              "convergence","log_like")

## Seleção do modelo:

arma11_mat <- mod_arma11mat_ini[[which(
  ini_arma11_mat$log_like == min(ini_arma11_mat$log_like[ini_arma11_mat$convergence == 0], na.rm = TRUE) & 
    ini_arma11_mat$convergence == 0
)]]

# Verificando a convergência

conver_arma11 <- rbind(arma11_mat$fit$convergence)
colnames(conver_arma11) <- c("convergence")

# Parâmetros estimados:

parametros_arma11 <- rbind(c(round(exp(arma11_mat$fit$par), 5)))
row.names(parametros_arma11) <- c("BSM_error")
colnames(parametros_arma11) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_arma11 <- rbind(2*(arma11_mat$fit$value) + 2*5)
colnames(AIC_arma11) <- "AIC"

BIC_arma11 <- 2*(arma11_mat$fit$value) + 2*5*log(arma11_mat$T)

# Matriz Hessiana

all(eigen(arma11_mat$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_arma11 <- list(arma11_mat)
testes_arma11 <- sapply(lista_arma11, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_arma11 <- t(testes_arma11)
row.names(testes_arma11) <- c("BSM_error")
colnames(testes_arma11) <- c("Shapiro", "Box", "H")  
resultadosmat_arma11 <- cbind(conver_arma11, parametros_arma11, testes_arma11, AIC_arma11, BIC_arma11)
resultadosmat_arma11

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_arma11 <- window(ts.union(
  ts(arma11_mat$ts.original, start = 2012, frequency = 4),
  ts(arma11_mat$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_arma11, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_arma11.cv <- window(ts.union(
  ts((arma11_mat$cv.original) * 100, start = 2012, frequency = 4),
  ts(arma11_mat$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_arma11.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07 - Zona da Mata (ARMA(1,1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


## GRÁFICO CONJUNTO

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_mat <- window(ts.union(
  ts(ma1_mat$ts.original, start = 2012, frequency = 4),
  ts(ar1_mat$ts.signal, start = 2012, frequency = 4),
  ts(ma1_mat$ts.signal, start = 2012, frequency = 4),
  ts(arma11_mat$ts.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_mat, plot.type = "single", col = c(1,4,2,3), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação AR(1): model-based",
                            "Sinal da Desocupação MA(1): model-based",
                            "Sinal da Desocupação ARMA(1,1): model-based"),
       lty = c(1,1,1,1), col = c(1,4,2,3), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_mat.cv <- window(ts.union(
  ts((ma1_mat$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_mat$cv.signal, start = 2012, frequency = 4),
  ts(ma1_mat$cv.signal, start = 2012, frequency = 4),
  ts(arma11_mat$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_mat.cv, plot.type = "single", col = c(1,4,2,3), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados AR(1): model-based",
                             "Sinal CV desocupados MA(1): model-based",
                             "Sinal CV desocupados ARMA(1,1): model-based"),
       lty = c(1,1,1,1), col = c(1,4,2,3), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07 - Zona da Mata (AR(1), MA(1) e ARMA(1,1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

#save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/07_mod_mat.Rdata")

### NORTE DE MINAS GERAIS ######################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid): # 43.22508 # 0.0009056865 # 142.2077
# Modelos para Norte: AR(1); ARMA(1,1)

## Funções e base de dados:

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
nrt<-baseestr0424$`08-Norte de Minas`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtnrt<-baseal0424$`08-Norte de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbnrt<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/08_params_nrt.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- nrt$Total.de.desocupados/1000
se_db <- nrt$sd_d/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-seq(-3,6,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)


#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbnrt[["mod_ar1"]][["phi1_ar1_dnrt"]]
grid_ar1 <- grid_error

# Rodando o modelo

source("data/funcoes/17_rodar_grid_ar1.R")
start_time <- Sys.time()
run_ar1nrt <- rodar_grid_ar1(y, grid_ar1, f.estrutural_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1nrt_ini <- run_ar1nrt$resultados

# Avaliação das iterações:
ini_ar1_nrt <- cbind(
  round(exp(grid_ar1), 5),
  do.call(rbind, lapply(1:nrow(grid_ar1), function(i) {
    tryCatch({
      params <- round(exp(mod_ar1nrt_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ar1nrt_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ar1nrt_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ar1_nrt) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ar1_nrt <- mod_ar1nrt_ini[[which(
  ini_ar1_nrt$log_like == min(ini_ar1_nrt$log_like[ini_ar1_nrt$convergence == 0], na.rm = TRUE) & 
    ini_ar1_nrt$convergence == 0
)]]

# Verificando a convergência

conver_ar1 <- rbind(ar1_nrt$fit$convergence)
colnames(conver_ar1) <- c("convergence")

# Parâmetros estimados:

parametros_ar1 <- rbind(c(round(exp(ar1_nrt$fit$par), 5)))
row.names(parametros_ar1) <- c("BSM_error")
colnames(parametros_ar1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1 <- rbind(2*(ar1_nrt$fit$value) + 2*5)
colnames(AIC_ar1) <- "AIC"

BIC_ar1 <- 2*(ar1_nrt$fit$value) + 2*5*log(ar1_nrt$T)

# Matriz Hessiana

all(eigen(ar1_nrt$fit$hessian, only.values = TRUE)$values > 0) # TRUE

# Diagnosticando os resíduos

lista_ar1 <- list(ar1_nrt)
testes_ar1 <- sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1 <- t(testes_ar1)
row.names(testes_ar1) <- c("BSM_error")
colnames(testes_ar1) <- c("Shapiro", "Box", "H")

resultadosnrt_ar1 <- cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
resultadosnrt_ar1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_nrt$ts.original, start = 2012, frequency = 4),
  ts(ar1_nrt$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_nrt$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_nrt$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Norte de Minas (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)



#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbnrt[["mod_ma1"]][["theta1_ma1_dnrt"]]
grid_ma1 <- grid_error

# Rodando o modelo

source("data/funcoes/18_rodar_grid_ma1.R")
start_time <- Sys.time()
run_ma1nrt <- rodar_grid_ma1(y, grid_ma1, f.estrutural_ma1)
end_time <- Sys.time()
end_time - start_time

mod_ma1nrt_ini <- run_ma1nrt$resultados

# Avaliação das iterações:
ini_ma1_nrt <- cbind(
  round(exp(grid_ma1), 5),
  do.call(rbind, lapply(1:nrow(grid_ma1), function(i) {
    tryCatch({
      params <- round(exp(mod_ma1nrt_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ma1nrt_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ma1nrt_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ma1_nrt) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ma1_nrt <- mod_ma1nrt_ini[[which(
  ini_ma1_nrt$log_like == min(ini_ma1_nrt$log_like[ini_ma1_nrt$convergence == 0], na.rm = TRUE) & 
    ini_ma1_nrt$convergence == 0
)]]

# Verificando a convergência

conver_ma1 <- rbind(ma1_nrt$fit$convergence)
colnames(conver_ma1) <- c("convergence")

# Parâmetros estimados:

parametros_ma1 <- rbind(c(round(exp(ma1_nrt$fit$par), 5)))
row.names(parametros_ma1) <- c("BSM_error")
colnames(parametros_ma1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1 <- rbind(2*(ma1_nrt$fit$value) + 2*5)
colnames(AIC_ma1) <- "AIC"

BIC_ma1 <- 2*(ma1_nrt$fit$value) + 2*5*log(ma1_nrt$T)

# Matriz Hessiana

all(eigen(ma1_nrt$fit$hessian, only.values = TRUE)$values > 0) # TRUE

# Diagnosticando os resíduos

lista_ma1 <- list(ma1_nrt)
testes_ma1 <- sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1 <- t(testes_ma1)
row.names(testes_ma1) <- c("BSM_error")
colnames(testes_ma1) <- c("Shapiro", "Box", "H")

resultadosnrt_ma1 <- cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
resultadosnrt_ma1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ma1 <- window(ts.union(
  ts(ma1_nrt$ts.original, start = 2012, frequency = 4),
  ts(ma1_nrt$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_nrt$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_nrt$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Norte de Minas (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)



#### MODELO ARMA(1,1):

source("data/funcoes/15_estrutural_ARMA11.R")
phi1_arma11 <- dbnrt[["mod_arma11"]][["phi1_arma11_dnrt"]]
theta1_arma11 <- dbnrt[["mod_arma11"]][["theta1_arma11_dnrt"]]
grid_arma11<-grid_error

# Rodando o modelo

source("data/funcoes/19_rodar_grid_arma11.R")
start_time <- Sys.time()
run_arma11nrt <- rodar_grid_arma11(y, grid_arma11, f.estrutural_arma11)
end_time <- Sys.time()
end_time - start_time

mod_arma11nrt_ini <- run_arma11nrt$resultados

# Avaliação das iterações:
ini_arma11_nrt <- cbind(
  round(exp(grid_arma11), 5),
  do.call(rbind, lapply(1:nrow(grid_arma11), function(i) {
    tryCatch({
      params <- round(exp(mod_arma11nrt_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_arma11nrt_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_arma11nrt_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_arma11_nrt) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                              "level","slope","seasonal","irregular", "sampl_error",
                              "convergence","log_like")

## Seleção do modelo:

arma11_nrt <- mod_arma11nrt_ini[[which(
  ini_arma11_nrt$log_like == min(ini_arma11_nrt$log_like[ini_arma11_nrt$convergence == 0], na.rm = TRUE) & 
    ini_arma11_nrt$convergence == 0
)]]

# Verificando a convergência

conver_arma11 <- rbind(arma11_nrt$fit$convergence)
colnames(conver_arma11) <- c("convergence")

# Parâmetros estimados:

parametros_arma11 <- rbind(c(round(exp(arma11_nrt$fit$par), 5)))
row.names(parametros_arma11) <- c("BSM_error")
colnames(parametros_arma11) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_arma11 <- rbind(2*(arma11_nrt$fit$value) + 2*5)
colnames(AIC_arma11) <- "AIC"

BIC_arma11 <- 2*(arma11_nrt$fit$value) + 2*5*log(arma11_nrt$T)

# Matriz Hessiana

all(eigen(arma11_nrt$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_arma11 <- list(arma11_nrt)
testes_arma11 <- sapply(lista_arma11, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_arma11 <- t(testes_arma11)
row.names(testes_arma11) <- c("BSM_error")
colnames(testes_arma11) <- c("Shapiro", "Box", "H")  
resultadosnrt_arma11 <- cbind(conver_arma11, parametros_arma11, testes_arma11, AIC_arma11, BIC_arma11)
resultadosnrt_arma11

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_arma11 <- window(ts.union(
  ts(arma11_nrt$ts.original, start = 2012, frequency = 4),
  ts(arma11_nrt$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_arma11, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_arma11.cv <- window(ts.union(
  ts((arma11_nrt$cv.original) * 100, start = 2012, frequency = 4),
  ts(arma11_nrt$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_arma11.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Norte de Minas (ARMA(1,1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# GRÁFICO UNIFICADO

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_nrt <- window(ts.union(
  ts(ma1_nrt$ts.original, start = 2012, frequency = 4),
  ts(ar1_nrt$ts.signal, start = 2012, frequency = 4),
  ts(ma1_nrt$ts.signal, start = 2012, frequency = 4),
  ts(arma11_nrt$ts.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_nrt, plot.type = "single", col = c(1,4,2,3), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação AR(1): model-based",
                            "Sinal da Desocupação MA(1): model-based",
                            "Sinal da Desocupação ARMA(1,1): model-based"),
       lty = c(1,1,1,1), col = c(1,4,2,3), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_nrt.cv <- window(ts.union(
  ts((ma1_nrt$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_nrt$cv.signal, start = 2012, frequency = 4),
  ts(ma1_nrt$cv.signal, start = 2012, frequency = 4),
  ts(arma11_nrt$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_nrt.cv, plot.type = "single", col = c(1,4,2,3), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados AR(1): model-based",
                             "Sinal CV desocupados MA(1): model-based",
                             "Sinal CV desocupados ARMA(1,1): model-based"),
       lty = c(1,1,1,1), col = c(1,4,2,3), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Norte de Minas (AR(1), MA(1) e ARMA(1,1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/08_mod_nrt.Rdata")

### VALE DO RIO DOCE ###########################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid): # 28.41725 # 1.194057 # 87.2518
# Modelos para Vale: AR(1);

## Funções e base de dados:

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
vl<-baseestr0424$`09-Vale do Rio Doce`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtvl<-baseal0424$`09-Vale do Rio Doce` ## Arquivo "cru", saída direta da rotina da base por rotação
dbvl<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/09_params_rio.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- vl$Total.de.desocupados/1000
se_db <- vl$sd_d/1000
cv_db <- se_db/y

# Parâmetros iniciais:
par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-seq(-3,6,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbvl[["mod_ar1"]][["phi1_ar1_drio"]]
grid_ar1 <- grid_error[-c(177),]

# Rodando o modelo

source("data/funcoes/17_rodar_grid_ar1.R")
start_time <- Sys.time()
run_ar1val <- rodar_grid_ar1(y, grid_ar1, f.estrutural_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1val_ini <- run_ar1val$resultados

# Avaliação das iterações:
ini_ar1_val <- cbind(
  round(exp(grid_ar1), 5),
  do.call(rbind, lapply(1:nrow(grid_ar1), function(i) {
    tryCatch({
      params <- round(exp(mod_ar1val_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ar1val_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ar1val_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ar1_val) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ar1_val <- mod_ar1val_ini[[which(
  ini_ar1_val$log_like == min(ini_ar1_val$log_like[ini_ar1_val$convergence == 0], na.rm = TRUE) & 
    ini_ar1_val$convergence == 0
)]]

# Verificando a convergência

conver_ar1 <- rbind(ar1_val$fit$convergence)
colnames(conver_ar1) <- c("convergence")

# Parâmetros estimados:

parametros_ar1 <- rbind(c(round(exp(ar1_val$fit$par), 5)))
row.names(parametros_ar1) <- c("BSM_error")
colnames(parametros_ar1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1 <- rbind(2*(ar1_val$fit$value) + 2*5)
colnames(AIC_ar1) <- "AIC"

BIC_ar1 <- 2*(ar1_val$fit$value) + 2*5*log(ar1_val$T)

# Matriz Hessiana

all(eigen(ar1_val$fit$hessian, only.values = TRUE)$values > 0) # TRUE

# Diagnosticando os resíduos

lista_ar1 <- list(ar1_val)
testes_ar1 <- sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1 <- t(testes_ar1)
row.names(testes_ar1) <- c("BSM_error")
colnames(testes_ar1) <- c("Shapiro", "Box", "H")

resultadosval_ar1 <- cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
resultadosval_ar1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_val$ts.original, start = 2012, frequency = 4),
  ts(ar1_val$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_val$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_val$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("09 - Vale do Rio Doce (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/09_mod_val.Rdata")

### CENTRAL ####################################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid): # 53.27499 # 0.001126252 # 83.0792
# Modelos para Central: AR(1); MA(1)

## Funções e base de dados:

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr0424 <- readRDS("D:/FJP2425/Programacao/data/baseestr0424.RDS")
cen<-baseestr0424$`10-Central`
baseal0424 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")
dtcen<-baseal0424$`10-Central` ## Arquivo "cru", saída direta da rotina da base por rotação
dbcen<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/10_params_cen.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- cen$Total.de.desocupados/1000
se_db <- cen$sd_d/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-seq(-3,6,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbcen[["mod_ar1"]][["phi1_ar1_dcen"]]
grid_ar1 <- grid_error[-c(241),]

# Rodando o modelo

source("data/funcoes/17_rodar_grid_ar1.R")
start_time <- Sys.time()
run_ar1cen <- rodar_grid_ar1(y, grid_ar1, f.estrutural_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1cen_ini <- run_ar1cen$resultados

# Avaliação das iterações:
ini_ar1_cen <- cbind(
  round(exp(grid_ar1), 5),
  do.call(rbind, lapply(1:nrow(grid_ar1), function(i) {
    tryCatch({
      params <- round(exp(mod_ar1cen_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ar1cen_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ar1cen_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ar1_cen) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ar1_cen <- mod_ar1cen_ini[[which(
  ini_ar1_cen$log_like == min(ini_ar1_cen$log_like[ini_ar1_cen$convergence == 0], na.rm = TRUE) & 
    ini_ar1_cen$convergence == 0
)]]

# Verificando a convergência

conver_ar1 <- rbind(ar1_cen$fit$convergence)
colnames(conver_ar1) <- c("convergence")

# Parâmetros estimados:

parametros_ar1 <- rbind(c(round(exp(ar1_cen$fit$par), 5)))
row.names(parametros_ar1) <- c("BSM_error")
colnames(parametros_ar1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1 <- rbind(2*(ar1_cen$fit$value) + 2*5)
colnames(AIC_ar1) <- "AIC"

BIC_ar1 <- 2*(ar1_cen$fit$value) + 2*5*log(ar1_cen$T)

# Matriz Hessiana

all(eigen(ar1_cen$fit$hessian, only.values = TRUE)$values > 0) # TRUE

# Diagnosticando os resíduos

lista_ar1 <- list(ar1_cen)
testes_ar1 <- sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1 <- t(testes_ar1)
row.names(testes_ar1) <- c("BSM_error")
colnames(testes_ar1) <- c("Shapiro", "Box", "H")

resultadoscen_ar1 <- cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
resultadoscen_ar1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_cen$ts.original, start = 2012, frequency = 4),
  ts(ar1_cen$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_cen$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_cen$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("10 - Central (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)



#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbcen[["mod_ma1"]][["theta1_ma1_dcen"]]
grid_ma1 <- grid_error[-c(241),]

# Rodando o modelo

source("data/funcoes/18_rodar_grid_ma1.R")
start_time <- Sys.time()
run_ma1cen <- rodar_grid_ma1(y, grid_ma1, f.estrutural_ma1)
end_time <- Sys.time()
end_time - start_time

mod_ma1cen_ini <- run_ma1cen$resultados

# Avaliação das iterações:
ini_ma1_cen <- cbind(
  round(exp(grid_ma1), 5),
  do.call(rbind, lapply(1:nrow(grid_ma1), function(i) {
    tryCatch({
      params <- round(exp(mod_ma1cen_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ma1cen_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ma1cen_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ma1_cen) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ma1_cen <- mod_ma1cen_ini[[which(
  ini_ma1_cen$log_like == min(ini_ma1_cen$log_like[ini_ma1_cen$convergence == 0], na.rm = TRUE) & 
    ini_ma1_cen$convergence == 0
)]]

# Verificando a convergência

conver_ma1 <- rbind(ma1_cen$fit$convergence)
colnames(conver_ma1) <- c("convergence")

# Parâmetros estimados:

parametros_ma1 <- rbind(c(round(exp(ma1_cen$fit$par), 5)))
row.names(parametros_ma1) <- c("BSM_error")
colnames(parametros_ma1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1 <- rbind(2*(ma1_cen$fit$value) + 2*5)
colnames(AIC_ma1) <- "AIC"

BIC_ma1 <- 2*(ma1_cen$fit$value) + 2*5*log(ma1_cen$T)

# Matriz Hessiana

all(eigen(ma1_cen$fit$hessian, only.values = TRUE)$values > 0) # FALSE

# Diagnosticando os resíduos

lista_ma1 <- list(ma1_cen)
testes_ma1 <- sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1 <- t(testes_ma1)
row.names(testes_ma1) <- c("BSM_error")
colnames(testes_ma1) <- c("Shapiro", "Box", "H")

resultadoscen_ma1 <- cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
resultadoscen_ma1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ma1 <- window(ts.union(
  ts(ma1_cen$ts.original, start = 2012, frequency = 4),
  ts(ma1_cen$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_cen$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_cen$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("10 - Central (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO UNIFICADO

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_cen <- window(ts.union(
  ts(ma1_cen$ts.original, start = 2012, frequency = 4),
  ts(ar1_cen$ts.signal, start = 2012, frequency = 4),
  ts(ma1_cen$ts.signal, start = 2012, frequency = 4) 
), start=c(2013,3))
plot(fig_cen, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação AR(1): model-based",
                            "Sinal da Desocupação MA(1): model-based"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_cen.cv <- window(ts.union(
  ts((ma1_cen$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_cen$cv.signal, start = 2012, frequency = 4),
  ts(ma1_cen$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_cen.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados AR(1): model-based",
                             "Sinal CV desocupados MA(1): model-based"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("10 - Central (AR(1) e MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/4_modestrutural_uni_desocup/10_mod_cen.Rdata")
