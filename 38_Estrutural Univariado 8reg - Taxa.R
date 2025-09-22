################################################################################
##              MODELO BSM UNIVARIADO - TAXA DESOCUPAÇÃO                      ##
################################################################################

library(dlm)
library(tidyverse)
library(parallel)
options(scipen=999)

# Neste script a grande diferença é deixar variar o nível da tendência na matriz W
# Um novo param deve ser incluído em cada grid

### MODELO BH ##################################################################
rm(list = ls())

# Modelos para BH: AR(1) e MA(1)

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
bh<-baseestr8reg$`01-Belo Horizonte`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/dadosalin_txdesoc_8reg.RDS")
dtbh<-baseal8reg$`01-Belo Horizonte`
dbbh<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/01_params_taxa_bh.RDS")

y <- (bh$Taxa.de.desocupação)*100
se_db<- (bh$sd_txd)*100
cv_db <- se_db/y

# Parâmetros iniciais:
par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-c(0)
par_5<-seq(-5,5,3)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

##### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")

phi1_ar1 <- dbbh[["taxamod_ar1"]][["phi1_ar1_txbh"]]

# Input dos parâmetros iniciais do modelo

grid_ar1<-grid_error[-c(113),]

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
legend("topleft", legend = c("Taxa da desocupação: design-based",
                             "Sinal da taxa da desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_bh$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_bh$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_ar1.cv, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("CV taxa: design-based",
                             "Sinal CV taxa: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_bh$ts.original, start = 2012, frequency = 4),ts(ar1_bh$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_bh$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_bh$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_bh$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de Desocupação: design-based",
                            "Tendência da Taxa de Desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de Desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de Desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de Desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de Desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

##### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")

theta1_ma1 <- dbbh[["taxamod_ma1"]][["theta1_ma1_txbh"]]

grid_ma1<-grid_error[-c(113),]

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

ma1_bh <- mod_ma1bh_ini[[which(
  ini_ma1_bh$log_like == min(ini_ma1_bh$log_like[ini_ma1_bh$convergence == 0], na.rm = TRUE) & 
    ini_ma1_bh$convergence == 0
)]]

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
legend("topleft", legend = c("Taxa da Desocupação: design-based",
                             "Sinal da Taxa da Desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("Taxa de Desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_bh$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_bh$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_ma1.cv, plot.type = "single", col = c(1,4), ylab="", xlab="", lty = c(1,1), lwd=c(2))
legend("topleft", legend = c("CV Taxa de Desocupação: design-based",
                             "Sinal CV Taxa de Desocupação: model-based"),
       lty = c(1,1), col = c(1,4), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte (MA1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_bh$ts.original, start = 2012, frequency = 4),ts(ma1_bh$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_bh$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_bh$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_bh$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de Desocupação: design-based",
                            "Tendência da Taxa de Desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de Desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de Desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de Desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("Taxa de Desocupação (%)", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de Desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

# Gráfico conjunto:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_bh <- window(ts.union(
  ts(ma1_bh$ts.original, start = 2012, frequency = 4),
  ts(ar1_bh$ts.signal, start = 2012, frequency = 4),
  ts(ma1_bh$ts.signal, start = 2012, frequency = 4) 
), start=c(2013,3))
plot(fig_bh, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("Taxa de desocupação: design-based",
                             "Sinal da Taxa de desocupação AR(1): model-based",
                             "Sinal da Taxa de desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_bh.cv <- window(ts.union(
  ts((ma1_bh$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_bh$cv.signal, start = 2012, frequency = 4),
  ts(ma1_bh$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_bh.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação AR(1): model-based",
                             "Sinal CV Taxa de desocupação MA(1): model-based"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte (todos os modelos)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/01_mod_txbh.Rdata")


### COLAR E ENTORNO METROPOLITANO ###############################################
rm(list = ls())

# Modelos para Entorno: AR(1); MA(1)

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
ent<-baseestr8reg$`02-Colar e Entorno metropolitano de BH`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/dadosalin_txdesoc_8reg.RDS")
dtent<-baseal8reg$`02-Colar e Entorno Metropolitano de BH`
dbent<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/02_params_taxa_ent.RDS")

## Definindo variáveis e inputs:

y <- (ent$Taxa.de.desocupação)*100
se_db <- (ent$sd_txd)*100
cv_db <- se_db/y

# Parâmetros iniciais:
# Realizei várias alterações nos iniciais, visto que alguns estavam quebrando o cálculo

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-c(0)
par_5<-seq(-3,6,3)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbent[["taxamod_ar1"]][["phi1_ar1_txent"]]
grid_ar1 <- grid_error[-c(145),]

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
legend("topleft", legend = c("Taxa de desocupação: design-based",
                             "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_ent$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_ent$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_ent$ts.original, start = 2012, frequency = 4),ts(ar1_ent$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_ent$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_ent$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_ent$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbent[["taxamod_ma1"]][["theta1_ma1_txent"]]
grid_ma1 <- grid_error[-c(145),]

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

ma1_ent <- mod_ma1ent_ini[[which(
  ini_ma1_ent$log_like == min(ini_ma1_ent$log_like[ini_ma1_ent$convergence == 0], na.rm = TRUE) & 
    ini_ma1_ent$convergence == 0
)]]

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
legend("topleft", legend = c("Taxa de desocupação: design-based",
                             "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_ent$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_ent$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte (MA1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_ent$ts.original, start = 2012, frequency = 4),ts(ma1_ent$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_ent$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_ent$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_ent$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO UNIFICADO:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_ent <- window(ts.union(
  ts(ma1_ent$ts.original, start = 2012, frequency = 4),
  ts(ar1_ent$ts.signal, start = 2012, frequency = 4),
  ts(ma1_ent$ts.signal, start = 2012, frequency = 4) 
), start=c(2013,3))
plot(fig_ent, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação AR(1)",
                            "Sinal da Taxa de desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ent.cv <- window(ts.union(
  ts((ma1_ent$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_ent$cv.signal, start = 2012, frequency = 4),
  ts(ma1_ent$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_ent.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação AR(1)",
                             "Sinal CV Taxa de desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte (AR e MA)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/02_mod_txent.Rdata")

### SUL DE MINAS ###############################################################
rm(list = ls())

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
sul<-baseestr8reg$`03-Sul de Minas`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/dadosalin_txdesoc_8reg.RDS")
dtsul<-baseal8reg$`03-Sul de Minas`
dbsul<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/03_params_taxa_sul.RDS")

y <- (sul$Taxa.de.desocupação)*100
se_db <- (sul$sd_txd)*100
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-c(0)
par_5<-seq(-3,6,3)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbsul[["taxamod_ar1"]][["phi1_ar1_txsul"]]
grid_ar1<-grid_error

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
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_sul$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_sul$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Estrututral Sul de Minas (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_sul$ts.original, start = 2012, frequency = 4),ts(ar1_sul$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_sul$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_sul$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_sul$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Estrutural Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbsul[["taxamod_ma1"]][["theta1_ma1_txsul"]]
grid_ma1<-grid_error

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
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_sul$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_sul$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Estrututral Sul de Minas (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_sul$ts.original, start = 2012, frequency = 4),ts(ma1_sul$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_sul$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_sul$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_sul$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Estrutural Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


## Gráfico conjunto:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_sul <- window(ts.union(
  ts(ma1_sul$ts.original, start = 2012, frequency = 4),
  ts(ar1_sul$ts.signal, start = 2012, frequency = 4),
  ts(ma1_sul$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_sul, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("Taxa de desocupação: design-based",
                             "Sinal da Taxa de desocupação AR(1)",
                             "Sinal da Taxa de desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_sul.cv <- window(ts.union(
  ts((ma1_sul$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_sul$cv.signal, start = 2012, frequency = 4),
  ts(ma1_sul$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_sul.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação AR(1)",
                             "Sinal CV Taxa de desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Estrutural Sul de Minas (AR(1) e MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/03_mod_txsul.Rdata")

### TRIÂNGULO MINEIRO ##########################################################
rm(list = ls())

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
trg<-baseestr8reg$`04-Triângulo Mineiro`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/dadosalin_txdesoc_8reg.RDS")
dttrg<-baseal8reg$`04-Triângulo Mineiro`
dbtrg<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/04_params_taxa_trg.RDS")

y <- (trg$Taxa.de.desocupação)*100
se_db <- (trg$sd_txd)*100
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-c(0)
par_5<-seq(-3,6,3)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbtrg[["taxamod_ar1"]][["phi1_ar1_txtrg"]]
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
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_trg$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_trg$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("04 - Estrutural Triângulo Mineiro (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_trg$ts.original, start = 2012, frequency = 4),ts(ar1_trg$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_trg$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_trg$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_trg$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("04 - Estrutural Triângulo Mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbtrg[["taxamod_ma1"]][["theta1_ma1_txtrg"]]
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
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_trg$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_trg$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("04 - Estrutural Triângulo Mineiro (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_trg$ts.original, start = 2012, frequency = 4),ts(ma1_trg$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_trg$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_trg$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_trg$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("04 - Estrutural Triângulo Mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# GRÁFICO UNIFICADO

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_trg <- window(ts.union(
  ts(ma1_trg$ts.original, start = 2012, frequency = 4),
  ts(ar1_trg$ts.signal, start = 2012, frequency = 4),
  ts(ma1_trg$ts.signal, start = 2012, frequency = 4) 
), start=c(2013,3))
plot(fig_trg, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação AR(1)",
                            "Sinal da Taxa de desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_trg.cv <- window(ts.union(
  ts((ma1_trg$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_trg$cv.signal, start = 2012, frequency = 4),
  ts(ma1_trg$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_trg.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação AR(1)",
                             "Sinal CV Taxa de desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("04 - Estrutural Triângulo Mineiro (AR(1) e MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/04_mod_txtrg.Rdata")

### ZONA DA MATA ###############################################################
rm(list = ls())

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
mat<-baseestr8reg$`05-Mata de Minas Gerais`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/dadosalin_txdesoc_8reg.RDS")
dtmat<-baseal8reg$`05-Mata de Minas Gerais`
dbmat<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/05_params_taxa_mat.RDS")

y <- (mat$Taxa.de.desocupação)*100
se_db <- (mat$sd_txd)*100
cv_db <- se_db/y

# Parâmetros iniciais:
par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-c(0)
par_5<-seq(-3,6,3)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbmat[["taxamod_ar1"]][["phi1_ar1_txmat"]]
grid_ar1 <- grid_error   #[-c(45,97),]

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
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_mat$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_mat$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05 - Estrutural Zona da Mata (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_mat$ts.original, start = 2012, frequency = 4),ts(ar1_mat$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_mat$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_mat$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_mat$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05 - Estrutural Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbmat[["taxamod_ma1"]][["theta1_ma1_txmat"]]
grid_ma1 <- grid_error

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
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_mat$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_mat$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05 - Zona da Mata Estrutural (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_mat$ts.original, start = 2012, frequency = 4),ts(ma1_mat$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_mat$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_mat$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_mat$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05 - Estrutural Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO CONJUNTO

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_mat <- window(ts.union(
  ts(ma1_mat$ts.original, start = 2012, frequency = 4),
  ts(ar1_mat$ts.signal, start = 2012, frequency = 4),
  ts(ma1_mat$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_mat, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação AR(1)",
                            "Sinal da Taxa de desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_mat.cv <- window(ts.union(
  ts((ma1_mat$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_mat$cv.signal, start = 2012, frequency = 4),
  ts(ma1_mat$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_mat.cv, plot.type = "single", col = c(1,4,2,3), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação AR(1)",
                             "Sinal CV Taxa de desocupação MA(1)"),
       lty = c(1,1,1,1), col = c(1,4,2,3), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("Comparativo entre modelos - Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/05_mod_txmat.Rdata")

### NORTE DE MINAS GERAIS ######################################################
rm(list = ls())

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
nrt<-baseestr8reg$`06-Norte de Minas`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/dadosalin_txdesoc_8reg.RDS")
dtnrt<-baseal8reg$`06-Norte de Minas`
dbnrt<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/06_params_taxa_nrt.RDS")

y <- (nrt$Taxa.de.desocupação)*100
se_db <- (nrt$sd_txd)*100
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-c(0)
par_5<-seq(-3,6,3)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbnrt[["taxamod_ar1"]][["phi1_ar1_txnrt"]]
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
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_nrt$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_nrt$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06 - Estrutural Norte de Minas (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_nrt$ts.original, start = 2012, frequency = 4),ts(ar1_nrt$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_nrt$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_nrt$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_nrt$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06- Estrutural Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbnrt[["taxamod_ma1"]][["theta1_ma1_txnrt"]]
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
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_nrt$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_nrt$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06 - Estrutural Norte de Minas (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_nrt$ts.original, start = 2012, frequency = 4),ts(ma1_nrt$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_nrt$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_nrt$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_nrt$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06- Estrutural Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# GRÁFICO UNIFICADO

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_nrt <- window(ts.union(
  ts(ma1_nrt$ts.original, start = 2012, frequency = 4),
  ts(ar1_nrt$ts.signal, start = 2012, frequency = 4),
  ts(ma1_nrt$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_nrt, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação AR(1)",
                            "Sinal da Desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_nrt.cv <- window(ts.union(
  ts((ma1_nrt$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_nrt$cv.signal, start = 2012, frequency = 4),
  ts(ma1_nrt$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_nrt.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação AR(1)",
                             "Sinal CV Taxa de desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06 - Estrutural Norte de Minas (AR(1) e MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/06_mod_txnrt.Rdata")

### VALE DO RIO DOCE ###########################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid): # 28.41725 # 1.194057 # 87.2518
# Modelos para Vale: AR(1);

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
vl<-baseestr8reg$`07-Vale do Rio Doce`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/dadosalin_txdesoc_8reg.RDS")
dtvl<-baseal8reg$`07-Vale do Rio Doce`
dbvl<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/07_params_taxa_val.RDS")

y <- (vl$Taxa.de.desocupação)*100
se_db <- (vl$sd_txd)*100
cv_db <- se_db/y

# Parâmetros iniciais:
par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-c(0)
par_5<-seq(-3,6,3)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbvl[["taxamod_ar1"]][["phi1_ar1_txval"]]
grid_ar1 <- grid_error[-c(172),]

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

all(eigen(ar1_val$fit$hessian, only.values = TRUE)$values > 0) # FALSE

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
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_val$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_val$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07 - Estrutural Vale do Rio Doce (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_val$ts.original, start = 2012, frequency = 4),ts(ar1_val$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_val$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_val$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_val$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07- Estrutural Vale do Rio Doce (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbvl[["taxamod_ma1"]][["theta1_ma1_txval"]]
grid_ma1 <- grid_error[-c(172),]

# Rodando o modelo

source("data/funcoes/18_rodar_grid_ma1.R")
start_time <- Sys.time()
run_ma1val <- rodar_grid_ma1(y, grid_ma1, f.estrutural_ma1)
end_time <- Sys.time()
end_time - start_time

mod_ma1val_ini <- run_ma1val$resultados

# Avaliação das iterações:
ini_ma1_val <- cbind(
  round(exp(grid_ma1), 5),
  do.call(rbind, lapply(1:nrow(grid_ma1), function(i) {
    tryCatch({
      params <- round(exp(mod_ma1val_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ma1val_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ma1val_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ma1_val) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ma1_val <- mod_ma1val_ini[[which(
  ini_ma1_val$log_like == min(ini_ma1_val$log_like[ini_ma1_val$convergence == 0], na.rm = TRUE) & 
    ini_ma1_val$convergence == 0
)]]

# Verificando a convergência

conver_ma1 <- rbind(ma1_val$fit$convergence)
colnames(conver_ma1) <- c("convergence")

# Parâmetros estimados:

parametros_ma1 <- rbind(c(round(exp(ma1_val$fit$par), 5)))
row.names(parametros_ma1) <- c("BSM_error")
colnames(parametros_ma1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1 <- rbind(2*(ma1_val$fit$value) + 2*5)
colnames(AIC_ma1) <- "AIC"

BIC_ma1 <- 2*(ma1_val$fit$value) + 2*5*log(ma1_val$T)

# Matriz Hessiana

all(eigen(ma1_val$fit$hessian, only.values = TRUE)$values > 0) # FALSE

# Diagnosticando os resíduos

lista_ma1 <- list(ma1_val)
testes_ma1 <- sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1 <- t(testes_ma1)
row.names(testes_ma1) <- c("BSM_error")
colnames(testes_ma1) <- c("Shapiro", "Box", "H")

resultadosval_ma1 <- cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
resultadosval_ma1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ma1 <- window(ts.union(
  ts(ma1_val$ts.original, start = 2012, frequency = 4),
  ts(ma1_val$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_val$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_val$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07 - Estrutural Vale do Rio Doce (MA1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ma1<-window(ts.union(ts(ma1_val$ts.original, start = 2012, frequency = 4),ts(ma1_val$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_val$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_val$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_val$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07- Estrutural Vale do Rio Doce (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_val <- window(ts.union(
  ts(ma1_val$ts.original, start = 2012, frequency = 4),
  ts(ar1_val$ts.signal, start = 2012, frequency = 4),
  ts(ma1_val$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_val, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação AR(1)",
                            "Sinal da Desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_val.cv <- window(ts.union(
  ts((ma1_val$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_val$cv.signal, start = 2012, frequency = 4),
  ts(ma1_val$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_val.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação AR(1)",
                             "Sinal CV Taxa de desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07 - Estrutural Vale do Rio Doce (AR(1) e MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/07_mod_txval.Rdata")

### CENTRAL ####################################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid): # 53.27499 # 0.001126252 # 83.0792
# Modelos para Central: AR(1); MA(1)

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
cen<-baseestr8reg$`08-Central`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/dadosalin_txdesoc_8reg.RDS")
dtcen<-baseal8reg$`08-Central`
dbcen<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/08_params_taxa_cen.RDS") 

y <- (cen$Taxa.de.desocupação)*100
se_db <- (cen$sd_txd)*100
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-c(0)
par_5<-seq(-3,6,3)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbcen[["taxamod_ar1"]][["phi1_ar1_txcen"]]
grid_ar1 <- grid_error[-c(72,132,136,137,160,205),]

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
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_cen$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_cen$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Estrutural Central (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_cen$ts.original, start = 2012, frequency = 4),ts(ar1_cen$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_cen$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_cen$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_cen$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Estrutural Central", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbcen[["taxamod_ma1"]][["theta1_ma1_txcen"]]
grid_ma1 <- grid_error[-c(72,132,136,137,160,205),]

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
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_cen$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_cen$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("10 - Estrutural Central (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_cen$ts.original, start = 2012, frequency = 4),ts(ma1_cen$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_cen$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_cen$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_cen$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Estrutural Central", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### GRÁFICO UNIFICADO

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_cen <- window(ts.union(
  ts(ma1_cen$ts.original, start = 2012, frequency = 4),
  ts(ar1_cen$ts.signal, start = 2012, frequency = 4),
  ts(ma1_cen$ts.signal, start = 2012, frequency = 4) 
), start=c(2013,3))
plot(fig_cen, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação AR(1)",
                            "Sinal da Taxa de desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_cen.cv <- window(ts.union(
  ts((ma1_cen$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_cen$cv.signal, start = 2012, frequency = 4),
  ts(ma1_cen$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_cen.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação AR(1)",
                             "Sinal CV Taxa de desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Estrutural Central (AR(1) e MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata
save.image(file = "C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/08_mod_txcen.Rdata")

### MINAS GERAIS ###############################################################

rm(list = ls())

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
mg<-baseestr8reg$`09 - Minas Gerais`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/dadosalin_txdesoc_8reg.RDS")
dtmg<-baseal8reg$`09 - Minas Gerais`
dbmg<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/09_params_taxa_mg.RDS")

y <- (mg$Taxa.de.desocupação)*100
se_db <- (mg$sd_txd)*100
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-2,7,3)
par_2<-seq(-2,7,3)
par_3<-seq(-2,7,3)
par_4<-c(0)
par_5<-seq(-2,7,3)

grid_error <- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbmg[["taxamod_ar1"]][["phi1_ar1_txmg"]]
grid_ar1 <- grid_error

# Rodando o modelo

source("data/funcoes/17_rodar_grid_ar1.R")
start_time <- Sys.time()
run_ar1mg <- rodar_grid_ar1(y, grid_ar1, f.estrutural_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1mg_ini <- run_ar1mg$resultados

# Avaliação das iterações:
ini_ar1_mg <- cbind(
  round(exp(grid_ar1), 5),
  do.call(rbind, lapply(1:nrow(grid_ar1), function(i) {
    tryCatch({
      params <- round(exp(mod_ar1mg_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ar1mg_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ar1mg_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ar1_mg) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                          "level","slope","seasonal","irregular", "sampl_error",
                          "convergence","log_like")

## Seleção do modelo:

ar1_mg <- mod_ar1mg_ini[[which(
  ini_ar1_mg$log_like == min(ini_ar1_mg$log_like[ini_ar1_mg$convergence == 0], na.rm = TRUE) &
    ini_ar1_mg$convergence == 0
)]]

# Verificando a convergência

conver_ar1 <- rbind(ar1_mg$fit$convergence)
colnames(conver_ar1) <- c("convergence")

# Parâmetros estimados:

parametros_ar1 <- rbind(c(round(exp(ar1_mg$fit$par), 5)))
row.names(parametros_ar1) <- c("BSM_error")
colnames(parametros_ar1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1 <- rbind(2*(ar1_mg$fit$value) + 2*5)
colnames(AIC_ar1) <- "AIC"

BIC_ar1 <- 2*(ar1_mg$fit$value) + 2*5*log(ar1_mg$T)

# Matriz Hessiana

all(eigen(ar1_mg$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ar1 <- list(ar1_mg)
testes_ar1 <- sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1 <- t(testes_ar1)
row.names(testes_ar1) <- c("BSM_error")
colnames(testes_ar1) <- c("Shapiro", "Box", "H")

resultadosmg_ar1 <- cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
resultadosmg_ar1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_mg$ts.original, start = 2012, frequency = 4),
  ts(ar1_mg$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_mg$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_mg$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("09 - Estrutural Minas Gerais (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_mg$ts.original, start = 2012, frequency = 4),ts(ar1_mg$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_mg$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_mg$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_mg$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("09 - Estrutural Minas Gerais", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


#### MODELO MA(1)

source("data/funcoes/14_estrutural_MA1.R")
theta1_ma1 <- dbmg[["taxamod_ma1"]][["theta1_ma1_txmg"]]
grid_ma1 <- grid_error

# Rodando o modelo

source("data/funcoes/18_rodar_grid_ma1.R")
start_time <- Sys.time()
run_ma1mg <- rodar_grid_ma1(y, grid_ma1, f.estrutural_ma1)
end_time <- Sys.time()
end_time - start_time

mod_ma1mg_ini <- run_ma1mg$resultados

# Avaliação das iterações:
ini_ma1_mg <- cbind(
  round(exp(grid_ma1), 5),
  do.call(rbind, lapply(1:nrow(grid_ma1), function(i) {
    tryCatch({
      params <- round(exp(mod_ma1mg_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ma1mg_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ma1mg_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ma1_mg) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                          "level","slope","seasonal","irregular", "sampl_error",
                          "convergence","log_like")

## Seleção do modelo:

ma1_mg <- mod_ma1mg_ini[[which(
  ini_ma1_mg$log_like == min(ini_ma1_mg$log_like[ini_ma1_mg$convergence == 0], na.rm = TRUE) &
    ini_ma1_mg$convergence == 0
)]]

# Verificando a convergência

conver_ma1 <- rbind(ma1_mg$fit$convergence)
colnames(conver_ma1) <- c("convergence")

# Parâmetros estimados:

parametros_ma1 <- rbind(c(round(exp(ma1_mg$fit$par), 5)))
row.names(parametros_ma1) <- c("BSM_error")
colnames(parametros_ma1) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1 <- rbind(2*(ma1_mg$fit$value) + 2*5)
colnames(AIC_ma1) <- "AIC"

BIC_ma1 <- 2*(ma1_mg$fit$value) + 2*5*log(ma1_mg$T)

# Matriz Hessiana

all(eigen(ma1_mg$fit$hessian, only.values = TRUE)$values > 0) # FALSE

# Diagnosticando os resíduos

lista_ma1 <- list(ma1_mg)
testes_ma1 <- sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1 <- t(testes_ma1)
row.names(testes_ma1) <- c("BSM_error")
colnames(testes_ma1) <- c("Shapiro", "Box", "H")

resultadosmg_ma1 <- cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
resultadosmg_ma1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ma1 <- window(ts.union(
  ts(ma1_mg$ts.original, start = 2012, frequency = 4),
  ts(ma1_mg$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Sinal da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_mg$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_mg$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV Taxa de desocupação: design-based",
                             "Sinal CV Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("09 - Estrutural Minas Gerais (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_mg$ts.original, start = 2012, frequency = 4),ts(ma1_mg$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_mg$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_mg$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_mg$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Taxa de desocupação: design-based",
                            "Tendência da Taxa de desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Taxa de desocupação", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("09 - Estrutural Minas Gerais", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO CONJUNTO

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_mg <- window(ts.union(
  ts(ma1_mg$ts.original, start = 2012, frequency = 4),
  ts(ar1_mg$ts.signal, start = 2012, frequency = 4),
  ts(ma1_mg$ts.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_mg, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação AR(1)",
                            "Sinal da Desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Taxa de desocupação (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_mg.cv <- window(ts.union(
  ts((ma1_mg$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_mg$cv.signal, start = 2012, frequency = 4),
  ts(ma1_mg$cv.signal, start = 2012, frequency = 4)), start=c(2013,3))
plot(fig_mg.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados AR(1)",
                             "Sinal CV desocupados MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("09 - Estrutural Minas Gerais (AR(1) e MA(1))", side = 3, outer = TRUE, line = 0.5)

# Salvando o .Rdata

save.image(file = "C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/09_mod_txmg.Rdata")
