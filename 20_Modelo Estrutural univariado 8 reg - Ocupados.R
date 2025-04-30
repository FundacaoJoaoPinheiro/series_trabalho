################################################################################
##                    MODELO ESTRUTURAL UNIVARIADO 8 ESTRATOS                 ##
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

# Modelos para BH: AR(1)

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
par_1<-seq(-5,5,3)
par_2<-seq(-5,5,3)
par_3<-seq(-5,5,3)
par_4<-seq(-5,5,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

##### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")

phi1_ar1 <- dbbh[["mod_ar1"]][["phi1_ar1_obh"]]

# Input dos parâmetros iniciais do modelo

grid_ar1<-grid_error

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

all(eigen(ar1_bh$fit$hessian, only.values = TRUE)$values > 0) # TRUE

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
mtext("01 - Estrutural Belo Horizonte (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_bh$ts.original, start = 2012, frequency = 4),ts(ar1_bh$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_bh$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_bh$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_bh$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("Desocupação: design-based",
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
mtext("01 - Belo Horizonte (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_ocup<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
result_mods_ocup[["01-Belo Horizonte"]][["sinal_estrutural_ar1bh"]]<-ar1_bh$ts.signal
result_mods_ocup[["01-Belo Horizonte"]][["cv_sinal_estrutural_ar1bh"]]<-ar1_bh$cv.signal
saveRDS(result_mods_ocup, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
rm(result_mods_ocup)


# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/01_mod_bh.Rdata")

### COLAR E ENTORNO METROPOLITANO ###############################################
rm(list = ls())

## Parâmetros do modelo UCM (referência para o grid): 106.7323; 0.00348154; 106.8907

# Modelos para Entorno: AR(1); MA(1), ARMA(1,1)

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
  # Realizei várias alterações nos iniciais, visto que alguns estavam quebrando o cálculo

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-seq(-3,6,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbent[["mod_ar1"]][["phi1_ar1_oent"]]
grid_ar1 <- grid_error

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
mtext("02 - Estrutural Colar e Entorno Metropolitano de Belo Horizonte (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_ent$ts.original, start = 2012, frequency = 4),ts(ar1_ent$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_ent$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_ent$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_ent$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("Desocupação: design-based",
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
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_ocup<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
result_mods_ocup[["02-Colar e Entorno Metropolitano de BH"]][["sinal_estrutural_ar1ent"]]<-ar1_ent$ts.signal
result_mods_ocup[["02-Colar e Entorno Metropolitano de BH"]][["cv_sinal_estrutural_ar1ent"]]<-ar1_ent$cv.signal
saveRDS(result_mods_ocup, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
rm(result_mods_ocup)


# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/02_mod_ent.Rdata")

### SUL DE MINAS ###############################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid): 56.85534; 0.0008044187; 90.04413
# Modelos para Sul: AR(1); MA(1), ARMA(1,1)

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

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-seq(-3,6,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbsul[["mod_ar1"]][["phi1_ar1_osul"]]
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
legend("topleft", legend = c("Desocupação: design-based",
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
mtext("03 - Estrututral Sul de Minas (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_sul$ts.original, start = 2012, frequency = 4),ts(ar1_sul$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_sul$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_sul$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_sul$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottom", legend = c("Sazonalidade"),
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
mtext("03 - Estrutural Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_ocup<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
result_mods_ocup[["03-Sul de Minas"]][["sinal_estrutural_ar1sul"]]<-ar1_sul$ts.signal
result_mods_ocup[["03-Sul de Minas"]][["cv_sinal_estrutural_ar1sul"]]<-ar1_sul$cv.signal
saveRDS(result_mods_ocup, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
rm(result_mods_ocup)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/03_mod_sul.Rdata")

### TRIÂNGULO MINEIRO ##########################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid):  # 11.70026 # 0.00006308897 # 84.38331
# Modelos para Triângulo: AR(1); MA(1)

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

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-seq(-3,6,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbtrg[["mod_ar1"]][["phi1_ar1_otrg"]]
grid_ar1 <- grid_error[-c(14,154),]

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

all(eigen(ar1_trg$fit$hessian, only.values = TRUE)$values > 0) # FALSE

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
legend("topright", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
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
legend("topleft", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottom", legend = c("Sazonalidade"),
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
mtext("04 - Estrutural Triângulo Mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_ocup<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
result_mods_ocup[["04-Triângulo Mineiro"]][["sinal_estrutural_ar1trg"]]<-ar1_trg$ts.signal
result_mods_ocup[["04-Triângulo Mineiro"]][["cv_sinal_estrutural_ar1trg"]]<-ar1_trg$cv.signal
saveRDS(result_mods_ocup, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
rm(result_mods_ocup)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/04_mod_trg.Rdata")

### ZONA DA MATA ###############################################################
rm(list = ls())

# Parâmetros do modelo UCM (referência para o grid): # 16.0373 # 0.0004886218 # 82.69742
# Modelos para Mata: AR(1); MA(1), ARMA(1,1)

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
par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-seq(-3,6,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbmat[["mod_ar1"]][["phi1_ar1_omat"]]
grid_ar1 <- grid_error[-c(162),]

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

all(eigen(ar1_mat$fit$hessian, only.values = TRUE)$values > 0) # FALSE

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
legend("bottomleft", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_mat$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_mat$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topright", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
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
mtext("05 - Estrutural Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_ocup<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
result_mods_ocup[["05-Zona da Mata"]][["sinal_estrutural_ar1mat"]]<-ar1_mat$ts.signal
result_mods_ocup[["05-Zona da Mata"]][["cv_sinal_estrutural_ar1mat"]]<-ar1_mat$cv.signal
saveRDS(result_mods_ocup, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
rm(result_mods_ocup)


# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/05_mod_mat.Rdata")

### NORTE DE MINAS GERAIS ######################################################
rm(list = ls())

# Modelos para Norte: AR(1); AR(5)

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

par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-seq(-3,6,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)


#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbnrt[["mod_ar1"]][["phi1_ar1_onrt"]]
grid_ar1 <- grid_error[-c(170),]

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

all(eigen(ar1_nrt$fit$hessian, only.values = TRUE)$values > 0) # FALSE

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
mtext("06 - Estrutural Norte de Minas (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_nrt$ts.original, start = 2012, frequency = 4),ts(ar1_nrt$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_nrt$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_nrt$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_nrt$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottom", legend = c("Sazonalidade"),
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
mtext("06- Estrutural Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_ocup<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
result_mods_ocup[["06-Norte de Minas"]][["sinal_estrutural_ar1nrt"]]<-ar1_nrt$ts.signal
result_mods_ocup[["06-Norte de Minas"]][["cv_sinal_estrutural_ar1nrt"]]<-ar1_nrt$cv.signal
saveRDS(result_mods_ocup, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
rm(result_mods_ocup)



#### MODELO AR(5)

source("data/funcoes/21_estrutural_AR5.R")
phi1_ar5 <- dbnrt[["mod_ar5"]][["phi_o"]][1]
phi2_ar5 <- dbnrt[["mod_ar5"]][["phi_o"]][2]
phi3_ar5 <- dbnrt[["mod_ar5"]][["phi_o"]][3]
phi4_ar5 <- dbnrt[["mod_ar5"]][["phi_o"]][4]
phi5_ar5 <- dbnrt[["mod_ar5"]][["phi_o"]][5]
grid_ar5 <- grid_error[-c(170),]

# Rodando o modelo

source("data/funcoes/22_rodar_grid_ar5.R")
start_time <- Sys.time()
run_ar5nrt <- rodar_grid_ar5(y, grid_ar5, f.estrutural_ar5)
end_time <- Sys.time()
end_time - start_time

mod_ar5nrt_ini <- run_ar5nrt$resultados

# Avaliação das iterações:
ini_ar5_nrt <- cbind(
  round(exp(grid_ar5), 5),
  do.call(rbind, lapply(1:nrow(grid_ar5), function(i) {
    tryCatch({
      params <- round(exp(mod_ar5nrt_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ar5nrt_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ar5nrt_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ar5_nrt) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ar5_nrt <- mod_ar5nrt_ini[[which(
  ini_ar5_nrt$log_like == min(ini_ar5_nrt$log_like[ini_ar5_nrt$convergence == 0], na.rm = TRUE) &
    ini_ar5_nrt$convergence == 0
)]]

# Verificando a convergência

conver_ar5 <- rbind(ar5_nrt$fit$convergence)
colnames(conver_ar5) <- c("convergence")

# Parâmetros estimados:

parametros_ar5 <- rbind(c(round(exp(ar5_nrt$fit$par), 5)))
row.names(parametros_ar5) <- c("BSM_error")
colnames(parametros_ar5) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar5 <- rbind(2*(ar5_nrt$fit$value) + 2*5)
colnames(AIC_ar5) <- "AIC"

BIC_ar5 <- 2*(ar5_nrt$fit$value) + 2*5*log(ar5_nrt$T)

# Matriz Hessiana

all(eigen(ar5_nrt$fit$hessian, only.values = TRUE)$values > 0) # FALSE

# Diagnosticando os resíduos

lista_ar5 <- list(ar5_nrt)
testes_ar5 <- sapply(lista_ar5, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar5 <- t(testes_ar5)
row.names(testes_ar5) <- c("BSM_error")
colnames(testes_ar5) <- c("Shapiro", "Box", "H")

resultadosnrt_ar5 <- cbind(conver_ar5, parametros_ar5, testes_ar5, AIC_ar5, BIC_ar5)
resultadosnrt_ar5

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar5 <- window(ts.union(
  ts(ar5_nrt$ts.original, start = 2012, frequency = 4),
  ts(ar5_nrt$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar5, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar5.cv <- window(ts.union(
  ts((ar5_nrt$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar5_nrt$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar5.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06 - Estrutural Norte de Minas (AR5)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(5)

figtend_ar5<-window(ts.union(ts(ar5_nrt$ts.original, start = 2012, frequency = 4),ts(ar5_nrt$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar5<-window(ts.union(ts(ar5_nrt$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar5<-window(ts.union(ts(ar5_nrt$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar5<-window(ts.union(ts(ar5_nrt$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar5, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar5, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("topright", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ar5, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("topright", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar5, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("topright", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06- Estrutural Norte de Minas AR(5)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_ocup<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
result_mods_ocup[["06-Norte de Minas"]][["sinal_estrutural_ar5nrt"]]<-ar5_nrt$ts.signal
result_mods_ocup[["06-Norte de Minas"]][["cv_sinal_estrutural_ar5nrt"]]<-ar5_nrt$cv.signal
saveRDS(result_mods_ocup, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
rm(result_mods_ocup)


# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/06_mod_nrt.Rdata")

### VALE DO RIO DOCE ###########################################################
rm(list = ls())

# Modelos para Vale: AR(1); AR(5)

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
par_1<-seq(-3,6,3)
par_2<-seq(-3,6,3)
par_3<-seq(-3,6,3)
par_4<-seq(-3,6,3)
par_5<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbvl[["mod_ar1"]][["phi1_ar1_orio"]]
grid_ar1 <- grid_error

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
legend("topleft", legend = c("Desocupação: design-based",
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
legend("topright", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07- Estrutural Vale do Rio Doce (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_ocup<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
result_mods_ocup[["07-Vale do Rio Doce"]][["sinal_estrutural_ar1val"]]<-ar1_val$ts.signal
result_mods_ocup[["07-Vale do Rio Doce"]][["cv_sinal_estrutural_ar1val"]]<-ar1_val$cv.signal
saveRDS(result_mods_ocup, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
rm(result_mods_ocup)



#### MODELO AR(5)

source("data/funcoes/21_estrutural_AR5.R")
phi1_ar5 <- dbvl[["mod_ar5"]][["phi_o"]][1]
phi2_ar5 <- dbvl[["mod_ar5"]][["phi_o"]][2]
phi3_ar5 <- dbvl[["mod_ar5"]][["phi_o"]][3]
phi4_ar5 <- dbvl[["mod_ar5"]][["phi_o"]][4]
phi5_ar5 <- dbvl[["mod_ar5"]][["phi_o"]][5]
grid_ar5 <- grid_error

# Rodando o modelo

source("data/funcoes/22_rodar_grid_ar5.R")
start_time <- Sys.time()
run_ar5val <- rodar_grid_ar5(y, grid_ar5, f.estrutural_ar5)
end_time <- Sys.time()
end_time - start_time

mod_ar5val_ini <- run_ar5val$resultados

# Avaliação das iterações:
ini_ar5_val <- cbind(
  round(exp(grid_ar5), 5),
  do.call(rbind, lapply(1:nrow(grid_ar5), function(i) {
    tryCatch({
      params <- round(exp(mod_ar5val_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ar5val_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ar5val_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ar5_val) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "level","slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Seleção do modelo:

ar5_val <- mod_ar5val_ini[[which(
  ini_ar5_val$log_like == min(ini_ar5_val$log_like[ini_ar5_val$convergence == 0], na.rm = TRUE) &
    ini_ar5_val$convergence == 0
)]]

# Verificando a convergência

conver_ar5 <- rbind(ar5_val$fit$convergence)
colnames(conver_ar5) <- c("convergence")

# Parâmetros estimados:

parametros_ar5 <- rbind(c(round(exp(ar5_val$fit$par), 5)))
row.names(parametros_ar5) <- c("BSM_error")
colnames(parametros_ar5) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar5 <- rbind(2*(ar5_val$fit$value) + 2*5)
colnames(AIC_ar5) <- "AIC"

BIC_ar5 <- 2*(ar5_val$fit$value) + 2*5*log(ar5_val$T)

# Matriz Hessiana

all(eigen(ar5_val$fit$hessian, only.values = TRUE)$values > 0) # FALSE

# Diagnosticando os resíduos

lista_ar5 <- list(ar5_val)
testes_ar5 <- sapply(lista_ar5, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar5 <- t(testes_ar5)
row.names(testes_ar5) <- c("BSM_error")
colnames(testes_ar5) <- c("Shapiro", "Box", "H")

resultadosval_ar5 <- cbind(conver_ar5, parametros_ar5, testes_ar5, AIC_ar5, BIC_ar5)
resultadosval_ar5

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar5 <- window(ts.union(
  ts(ar5_val$ts.original, start = 2012, frequency = 4),
  ts(ar5_val$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar5, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar5.cv <- window(ts.union(
  ts((ar5_val$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar5_val$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar5.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07-Vale do Rio Doce (AR5)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(5)

figtend_ar5<-window(ts.union(ts(ar5_val$ts.original, start = 2012, frequency = 4),ts(ar5_val$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar5<-window(ts.union(ts(ar5_val$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar5<-window(ts.union(ts(ar5_val$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar5<-window(ts.union(ts(ar5_val$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar5, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar5, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottom", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ar5, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("topright", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar5, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("topright", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07-Estrututral Vale do Rio Doce AR(5)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_ocup<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
result_mods_ocup[["07-Vale do Rio Doce"]][["sinal_estrutural_ar5val"]]<-ar5_val$ts.signal
result_mods_ocup[["07-Vale do Rio Doce"]][["cv_sinal_estrutural_ar5val"]]<-ar5_val$cv.signal
saveRDS(result_mods_ocup, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
rm(result_mods_ocup)


# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/07_mod_val.Rdata")

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

y <- cen$Total.de.ocupados/1000
se_db <- cen$sd_o/1000
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
phi1_ar1 <- dbcen[["mod_ar1"]][["phi1_ar1_ocen"]]
grid_ar1 <- grid_error[-c(65,79,157,189),]

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
legend("topleft", legend = c("Desocupação: design-based",
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
mtext("08 - Estrutural Central (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_cen$ts.original, start = 2012, frequency = 4),ts(ar1_cen$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_cen$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_cen$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_cen$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottom", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("topright", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("topright", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Estrutural Central AR(1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_ocup<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
result_mods_ocup[["08-Central"]][["sinal_estrutural_ar1cen"]]<-ar1_cen$ts.signal
result_mods_ocup[["08-Central"]][["cv_sinal_estrutural_ar1cen"]]<-ar1_cen$cv.signal
saveRDS(result_mods_ocup, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
rm(result_mods_ocup)


# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/08_mod_cen.Rdata")


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

par_1<-seq(-2,7,3)
par_2<-seq(-2,7,3)
par_3<-seq(-2,7,3)
par_4<-seq(-2,7,3)
par_5<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4,par_5)

#### MODELO AR(1)

source("data/funcoes/12_estrutural_AR1.R")
phi1_ar1 <- dbmg[["mod_ar1"]][["phi1_ar1_omg"]]
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
legend("topleft", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_mg$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_mg$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
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
legend("topleft", legend = c("Desocupação: design-based",
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
legend("topright", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("topright", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("09 - Estrutural Minas Gerais AR(1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_ocup<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
result_mods_ocup[["09 - Minas Gerais"]][["sinal_estrutural_ar1mg"]]<-ar1_mg$ts.signal
result_mods_ocup[["09 - Minas Gerais"]][["cv_sinal_estrutural_ar1mg"]]<-ar1_mg$cv.signal
saveRDS(result_mods_ocup, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
rm(result_mods_ocup)



#### MODELO AR(5)

source("data/funcoes/21_estrutural_AR5.R")
phi1_ar5 <- dbmg[["mod_ar5"]][["phi_o"]][1]
phi2_ar5 <- dbmg[["mod_ar5"]][["phi_o"]][2]
phi3_ar5 <- dbmg[["mod_ar5"]][["phi_o"]][3]
phi4_ar5 <- dbmg[["mod_ar5"]][["phi_o"]][4]
phi5_ar5 <- dbmg[["mod_ar5"]][["phi_o"]][5]
grid_ar5 <- grid_error

# Rodando o modelo

source("data/funcoes/22_rodar_grid_ar5.R")
start_time <- Sys.time()
run_ar5mg <- rodar_grid_ar5(y, grid_ar5, f.estrutural_ar5)
end_time <- Sys.time()
end_time - start_time

mod_ar5mg_ini <- run_ar5mg$resultados

# Avaliação das iterações:
ini_ar5_mg <- cbind(
  round(exp(grid_ar5), 5),
  do.call(rbind, lapply(1:nrow(grid_ar5), function(i) {
    tryCatch({
      params <- round(exp(mod_ar5mg_ini[[i]][["fit"]][["par"]]), 5)
      convergence <- mod_ar5mg_ini[[i]][["fit"]][["convergence"]]
      log_like <- mod_ar5mg_ini[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 7))
  }))
)

colnames(ini_ar5_mg) <- c("level_ini","slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                          "level","slope","seasonal","irregular", "sampl_error",
                          "convergence","log_like")

## Seleção do modelo:

ar5_mg <- mod_ar5mg_ini[[which(
  ini_ar5_mg$log_like == min(ini_ar5_mg$log_like[ini_ar5_mg$convergence == 0], na.rm = TRUE) &
    ini_ar5_mg$convergence == 0
)]]

# Verificando a convergência

conver_ar5 <- rbind(ar5_mg$fit$convergence)
colnames(conver_ar5) <- c("convergence")

# Parâmetros estimados:

parametros_ar5 <- rbind(c(round(exp(ar5_mg$fit$par), 5)))
row.names(parametros_ar5) <- c("BSM_error")
colnames(parametros_ar5) <- c("Level","Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar5 <- rbind(2*(ar5_mg$fit$value) + 2*5)
colnames(AIC_ar5) <- "AIC"

BIC_ar5 <- 2*(ar5_mg$fit$value) + 2*5*log(ar5_mg$T)

# Matriz Hessiana

all(eigen(ar5_mg$fit$hessian, only.values = TRUE)$values > 0) # FALSE

# Diagnosticando os resíduos

lista_ar5 <- list(ar5_mg)
testes_ar5 <- sapply(lista_ar5, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]], 5),
                                                   round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]], 5),
                                                   teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar5 <- t(testes_ar5)
row.names(testes_ar5) <- c("BSM_error")
colnames(testes_ar5) <- c("Shapiro", "Box", "H")

resultadosmg_ar5 <- cbind(conver_ar5, parametros_ar5, testes_ar5, AIC_ar5, BIC_ar5)
resultadosmg_ar5

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar5 <- window(ts.union(
  ts(ar5_mg$ts.original, start = 2012, frequency = 4),
  ts(ar5_mg$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar5, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar5.cv <- window(ts.union(
  ts((ar5_mg$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar5_mg$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar5.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topright", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("09 - Estrutural Minas Gerais (AR5)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(5)

figtend_ar5<-window(ts.union(ts(ar5_mg$ts.original, start = 2012, frequency = 4),ts(ar5_mg$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar5<-window(ts.union(ts(ar5_mg$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar5<-window(ts.union(ts(ar5_mg$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar5<-window(ts.union(ts(ar5_mg$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figtend_ar5, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_ar5, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottom", legend = c("Sazonalidade"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_ar5, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_ar5, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("09 - Minas Gerais AR(5)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_ocup<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
result_mods_ocup[["09 - Minas Gerais"]][["sinal_estrutural_ar5mg"]]<-ar5_mg$ts.signal
result_mods_ocup[["09 - Minas Gerais"]][["cv_sinal_estrutural_ar5mg"]]<-ar5_mg$cv.signal
saveRDS(result_mods_ocup, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
rm(result_mods_ocup)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/09_mod_mg.Rdata")
