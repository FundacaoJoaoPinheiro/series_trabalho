################################################################################
##                MODELO ESTRUTURAL COM SAZONALIDADE DUMMY                    ##
################################################################################

# Teste dos modelos alterando a sazonalidade para dummy
  # O comportamento do erro amostral para cada região já foi escolhido a partir do resultado dos
  #  scripts que realizam a estimação dos modelos suavizado e estrutural

library(dlm)
library(tidyverse)
library(beepr)
library(parallel)

options(scipen=999)

### MODELO BH ##################################################################
rm(list = ls())

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
bh<-baseestr8reg$`01-Belo Horizonte`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtbh<-baseal8reg$`01-Belo Horizonte` ## Arquivo "cru", saída direta da rotina da base por rotação
dbbh<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/01_params_bh.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

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


##### MODELO AR(1) - (TESTE PARA POSSÍVEIS TRAVAMENTOS)

source("data/funcoes/24_estruturaldummy_AR1.R")

phi1_ar1 <- dbbh[["mod_ar1"]][["phi1_ar1_dbh"]]

# Input dos parâmetros iniciais do modelo

grid_ar1<-grid_error

# Antes de rodar o modelo com processamento paralelo, recomenda-se testar os parâmetros

source("data/funcoes/25_grid_dummy_ar1.R")
start_time <- Sys.time()
run_ar1bh <- grid_dummy_ar1(y, grid_ar1, f.estruturaldummy_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1bh_ini<-run_ar1bh$resultados

rm(list = c("run_ar1bh","mod_ar1bh_ini","grid_ar1"))



##### MODELO MA(1)

source("data/funcoes/26_estruturaldummy_MA1.R")

theta1_ma1 <- dbbh[["mod_ma1"]][["theta1_ma1_dbh"]]

# Input dos parâmetros iniciais do modelo

grid_ma1<-grid_error

# Rodando os parâmetros iniciais do modelo:

source("data/funcoes/27_grid_dummy_ma1.R")

start_time <- Sys.time()
run_ma1bh <- grid_dummy_ma1(y, grid_ma1, f.estruturaldummy_ma1)
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

## Seleção do modelo:

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

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_bh$ts.original, start = 2012, frequency = 4),ts(ma1_bh$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_bh$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_bh$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_bh$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["01-Belo Horizonte"]][["sinal_estrutural_ma1bh"]]<-ma1_bh$ts.signal
result_mods_deso[["01-Belo Horizonte"]][["cv_sinal_estrutural_ma1bh"]]<-ma1_bh$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/01_mod_bh.Rdata")

### COLAR E ENTORNO METROPOLITANO ###############################################
rm(list = ls())

# Modelo escolhido: MA(1)

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
ent<-baseestr8reg$`02-Colar e Entorno metropolitano de BH`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtent<-baseal8reg$`02-Colar e Entorno Metropolitano de BH` ## Arquivo "cru", saída direta da rotina da base por rotação
dbent<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/02_params_ent.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- (ent$Total.de.desocupados)/1000
se_db <- (ent$sd_d)/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-seq(-3,6,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1) - TESTE

source("data/funcoes/24_estruturaldummy_AR1.R")

phi1_ar1 <- dbent[["mod_ar1"]][["phi1_ar1_dent"]]

# Input dos parâmetros iniciais do modelo

grid_ar1<-grid_error

# Antes de rodar o modelo com processamento paralelo, recomenda-se testar os parâmetros

source("data/funcoes/25_grid_dummy_ar1.R")
start_time <- Sys.time()
run_ar1ent <- grid_dummy_ar1(y, grid_ar1, f.estruturaldummy_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1ent_ini<-run_ar1ent$resultados

rm(list = c("run_ar1ent","mod_ar1ent_ini","grid_ar1"))


#### MODELO MA(1)

source("data/funcoes/26_estruturaldummy_MA1.R")
theta1_ma1 <- dbent[["mod_ma1"]][["theta1_ma1_dent"]]
grid_ma1 <- grid_error

# Rodando o modelo

source("data/funcoes/27_grid_dummy_ma1.R")
start_time <- Sys.time()
run_ma1ent <- grid_dummy_ma1(y, grid_ma1, f.estruturaldummy_ma1)
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

## Seleção do modelo:

ma1_ent <- mod_ma1ent_ini[[110]] ## Tive que selecionar a linha específica. Menor log-like com valores semelhantes

#ma1_ent <- mod_ma1ent_ini[[which(
#  ini_ma1_ent$log_like == min(ini_ma1_ent$log_like[ini_ma1_ent$convergence == 0], na.rm = TRUE) & 
#    ini_ma1_ent$convergence == 0
#)]]

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
legend("bottom", legend = c("Desocupação: design-based",
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
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte (MA1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_ent$ts.original, start = 2012, frequency = 4),ts(ma1_ent$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_ent$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_ent$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_ent$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/02_mod_ent.Rdata")

### SUL DE MINAS ###############################################################
rm(list = ls())

# Modelos de escolha para o Sul: ARMA(1,1)

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
sul<-baseestr8reg$`03-Sul de Minas`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtsul<-baseal8reg$`03-Sul de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbsul<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/03_params_sul.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

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

#### MODELO AR(1) - TESTE DE FUNCAO

source("data/funcoes/24_estruturaldummy_AR1.R")
phi1_ar1 <- dbsul[["mod_ar1"]][["phi1_ar1_dsul"]]
grid_ar1<-grid_error[-c(113,153),]

# Rodando o modelo

source("data/funcoes/25_grid_dummy_ar1.R")
start_time <- Sys.time()
run_ar1sul <- grid_dummy_ar1(y, grid_ar1, f.estruturaldummy_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1sul_ini <- run_ar1sul$resultados

rm(list = c("run_ar1sul","mod_ar1sul_ini","grid_ar1"))


#### MODELO ARMA(1,1):

source("data/funcoes/28_estruturaldummy_ARMA11.R")
phi1_arma11 <- dbsul[["mod_arma11"]][["phi1_arma11_dsul"]]
theta1_arma11 <- dbsul[["mod_arma11"]][["theta1_arma11_dsul"]]
grid_arma11<-grid_error[-c(113,153),]

# Rodando o modelo

source("data/funcoes/29_grid_dummy_arma11.R")
start_time <- Sys.time()
run_arma11sul <- grid_dummy_arma11(y, grid_arma11, f.estruturaldummy_arma11)
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
mtext("03 - Estrututral Sul de Minas (ARMA(1,1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE ARMA(1,1)

figtend_arma11<-window(ts.union(ts(arma11_sul$ts.original, start = 2012, frequency = 4),ts(arma11_sul$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_arma11<-window(ts.union(ts(arma11_sul$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_arma11<-window(ts.union(ts(arma11_sul$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_arma11<-window(ts.union(ts(arma11_sul$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_arma11, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_arma11, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_arma11, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_arma11, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Estrutural Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/03_mod_sul.Rdata")

### TRIÂNGULO MINEIRO ##########################################################
rm(list = ls())

# Modelo escolhido para o Triângulo: MA(1)

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
trg<-baseestr8reg$`04-Triângulo Mineiro`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dttrg<-baseal8reg$`04-Triângulo Mineiro` ## Arquivo "cru", saída direta da rotina da base por rotação
dbtrg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/04_params_trg.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

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

#### MODELO AR(1) - TESTE DA FUNCAO

source("data/funcoes/24_estruturaldummy_AR1.R")
phi1_ar1 <- dbtrg[["mod_ar1"]][["phi1_ar1_dtrg"]]
grid_ar1 <- grid_error

# Rodando o modelo

source("data/funcoes/25_grid_dummy_ar1.R")
start_time <- Sys.time()
run_ar1trg <- grid_dummy_ar1(y, grid_ar1, f.estruturaldummy_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1trg_ini <- run_ar1trg$resultados

rm(list = c("run_ar1trg","mod_ar1trg_ini","grid_ar1"))


#### MODELO MA(1)

source("data/funcoes/26_estruturaldummy_MA1.R")
theta1_ma1 <- dbtrg[["mod_ma1"]][["theta1_ma1_dtrg"]]
grid_ma1 <- grid_error

# Rodando o modelo

source("data/funcoes/27_grid_dummy_ma1.R")
start_time <- Sys.time()
run_ma1trg <- grid_dummy_ma1(y, grid_ma1, f.estruturaldummy_ma1)
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
mtext("04 - Estrutural Triângulo Mineiro (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_trg$ts.original, start = 2012, frequency = 4),ts(ma1_trg$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_trg$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_trg$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_trg$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("04 - Estrutural Triângulo Mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/04_mod_trg.Rdata")

### ZONA DA MATA ###############################################################
rm(list = ls())

# Modelo escolhido para Mata: MA(1)

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
mat<-baseestr8reg$`05-Mata de Minas Gerais`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtmat<-baseal8reg$`05-Mata de Minas Gerais` ## Arquivo "cru", saída direta da rotina da base por rotação
dbmat<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/05_params_mat.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

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

source("data/funcoes/24_estruturaldummy_AR1.R")
phi1_ar1 <- dbmat[["mod_ar1"]][["phi1_ar1_dmat"]]
grid_ar1 <- grid_error[-c(246),]

# Rodando o modelo

source("data/funcoes/25_grid_dummy_ar1.R")
start_time <- Sys.time()
run_ar1mat <- grid_dummy_ar1(y, grid_ar1, f.estruturaldummy_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1mat_ini <- run_ar1mat$resultados

rm(list = c("run_ar1mat","mod_ar1mat_ini","grid_ar1"))



#### MODELO MA(1)

source("data/funcoes/26_estruturaldummy_MA1.R")
theta1_ma1 <- dbmat[["mod_ma1"]][["theta1_ma1_dmat"]]
grid_ma1 <- grid_error[-c(246),]

# Rodando o modelo

source("data/funcoes/27_grid_dummy_ma1.R")
start_time <- Sys.time()
run_ma1mat <- grid_dummy_ma1(y, grid_ma1, f.estruturaldummy_ma1)
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
mtext("05 - Estrutural  (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_mat$ts.original, start = 2012, frequency = 4),ts(ma1_mat$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_mat$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_mat$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_mat$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05 - Estrutural Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/05_mod_mat.Rdata")

### NORTE DE MINAS GERAIS ######################################################
rm(list = ls())

# Modelos para escolhido Norte: MA1

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
nrt<-baseestr8reg$`06-Norte de Minas`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtnrt<-baseal8reg$`06-Norte de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbnrt<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/06_params_nrt.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

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


#### MODELO AR(1) - TESTE PARA TRAVAMENTO

source("data/funcoes/24_estruturaldummy_AR1.R")
phi1_ar1 <- dbnrt[["mod_ar1"]][["phi1_ar1_dnrt"]]
grid_ar1 <- grid_error[-c(242),]

# Rodando o modelo

source("data/funcoes/25_grid_dummy_ar1.R")
start_time <- Sys.time()
run_ar1nrt <- grid_dummy_ar1(y, grid_ar1, f.estruturaldummy_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1nrt_ini <- run_ar1nrt$resultados

rm(list = c("run_ar1nrt","mod_ar1nrt_ini","grid_ar1"))



#### MODELO MA(1)

source("data/funcoes/26_estruturaldummy_MA1.R")
theta1_ma1 <- dbnrt[["mod_ma1"]][["theta1_ma1_dnrt"]]
grid_ma1 <- grid_error[-c(242),]

# Rodando o modelo

source("data/funcoes/27_grid_dummy_ma1.R")
start_time <- Sys.time()
run_ma1nrt <- grid_dummy_ma1(y, grid_ma1, f.estruturaldummy_ma1)
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
mtext("06 - Estrutural (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_nrt$ts.original, start = 2012, frequency = 4),ts(ma1_nrt$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_nrt$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_nrt$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_nrt$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06- Estrutural Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/06_mod_nrt.Rdata")

### VALE DO RIO DOCE ###########################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid): # 28.41725 # 1.194057 # 87.2518
# Modelos para Vale: AR(1);

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
vl<-baseestr8reg$`07-Vale do Rio Doce`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtvl<-baseal8reg$`07-Vale do Rio Doce` ## Arquivo "cru", saída direta da rotina da base por rotação
dbvl<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/07_params_rio.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

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

source("data/funcoes/24_estruturaldummy_AR1.R")
phi1_ar1 <- dbvl[["mod_ar1"]][["phi1_ar1_drio"]]
grid_ar1 <- grid_error[-c(69),]

# Rodando o modelo

source("data/funcoes/25_grid_dummy_ar1.R")
start_time <- Sys.time()
run_ar1val <- grid_dummy_ar1(y, grid_ar1, f.estruturaldummy_ar1)
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
mtext("07 - Estrutural Vale do Rio Doce (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_val$ts.original, start = 2012, frequency = 4),ts(ar1_val$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_val$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_val$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_val$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07- Estrutural Vale do Rio Doce (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/07_mod_val.Rdata")

### CENTRAL ####################################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid): # 53.27499 # 0.001126252 # 83.0792
# Modelos para Central: AR(1); MA(1)

## Funções e base de dados

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
cen<-baseestr8reg$`08-Central`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtcen<-baseal8reg$`08-Central` ## Arquivo "cru", saída direta da rotina da base por rotação
dbcen<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/08_params_cen.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

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

source("data/funcoes/24_estruturaldummy_AR1.R")
phi1_ar1 <- dbcen[["mod_ar1"]][["phi1_ar1_dcen"]]
grid_ar1 <- grid_error

# Rodando o modelo

source("data/funcoes/25_grid_dummy_ar1.R")
start_time <- Sys.time()
run_ar1cen <- grid_dummy_ar1(y, grid_ar1, f.estruturaldummy_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1cen_ini <- run_ar1cen$resultados

rm(list = c("run_ar1cen","mod_ar1cen_ini","grid_ar1"))



#### MODELO MA(1)

source("data/funcoes/26_estruturaldummy_MA1.R")
theta1_ma1 <- dbcen[["mod_ma1"]][["theta1_ma1_dcen"]]
grid_ma1 <- grid_error

# Rodando o modelo

source("data/funcoes/27_grid_dummy_ma1.R")
start_time <- Sys.time()
run_ma1cen <- grid_dummy_ma1(y, grid_ma1, f.estruturaldummy_ma1)
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
mtext("10 - Estrutural Central (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_cen$ts.original, start = 2012, frequency = 4),ts(ma1_cen$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_cen$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_cen$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_cen$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Estrutural Central", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/08_mod_cen.Rdata")


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

y <- mg$Total.de.desocupados/1000
se_db <- mg$sd_d/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-2,7,3)
par_2<-seq(-2,7,3)
par_3<-seq(-2,7,3)
par_4<-seq(-2,7,3)
par_5<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/24_estruturaldummy_AR1.R")
phi1_ar1 <- dbmg[["mod_ar1"]][["phi1_ar1_dmg"]]
grid_ar1 <- grid_error[-c(131),]

# Rodando o modelo

source("data/funcoes/25_grid_dummy_ar1.R")
start_time <- Sys.time()
run_ar1mg <- grid_dummy_ar1(y, grid_ar1, f.estruturaldummy_ar1)
end_time <- Sys.time()
end_time - start_time

mod_ar1mg_ini <- run_ar1mg$resultados

rm(list = c("run_ar1mg","mod_ar1mg_ini","grid_ar1"))


#### MODELO MA(1)

source("data/funcoes/26_estruturaldummy_MA1.R")
theta1_ma1 <- dbmg[["mod_ma1"]][["theta1_ma1_dmg"]]
grid_ma1 <- grid_error[-c(131),]

# Rodando o modelo

source("data/funcoes/27_grid_dummy_ma1.R")
start_time <- Sys.time()
run_ma1mg <- grid_dummy_ma1(y, grid_ma1, f.estruturaldummy_ma1)
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
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_mg$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_mg$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
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
legend("bottom", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ma1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("09 - Estrutural Minas Gerais", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/09_mod_mg.Rdata")
