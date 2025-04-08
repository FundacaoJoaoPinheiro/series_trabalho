################################################################################
##                          MODELO BSM UNIVARIADO                             ##
################################################################################

library(dlm)
library(tidyverse)
library(beepr)
library(parallel)
library(rucm)
options(scipen=999)

## Anotações:
  # Os processos adotados para o sample erro foram escolhidos conforme a FAC e a FACP dos pseudo erros
    # Logo, não há mais uma padronização para todos os estratos

## Estratos:
  # BH: MA(1) ENT: MA(1)* SUL:MA(1)* TRG: MA(1) MAT: ARMA(1,1) 
  # NRT: ARMA(1,1) VAL: AR(1) CEN: MA(1) MG: MA(1)

## Os resultados mudam apenas para as regiões 02, 06 e 09 (novos resultados para MG)

## No seguinte arquivo estarão alguns resultados para os modelos smooth e estrutural:
# result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/result_mods_deso.rds")

### BELO HORIZONTE #############################################################
rm(list = ls())

## UCM: 26.15096; 0.0001269569; 75.98691
## Para BH: AR(1) e MA(1)

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

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error<- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO AR(1)

source("data/funcoes/07_smooth_AR1.R")
phi1_ar1 <- dbbh[["mod_ar1"]][["phi1_ar1_dbh"]]

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
run_ar1bh<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.smooth_ar1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ar1_bh <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ar1bh[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ar1bh[[i]][["fit"]][["convergence"]]
      log_like <- run_ar1bh[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)


colnames(ini_ar1_bh) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                          "slope","seasonal","irregular", "sampl_error",
                          "convergence","log_like")

## Após iniciais, seleção do modelo:

ar1_bh <- run_ar1bh[[which(
  ini_ar1_bh$log_like == min(ini_ar1_bh$log_like[ini_ar1_bh$convergence == 0], na.rm = TRUE) & 
    ini_ar1_bh$convergence == 0
)]]

# Verificando a convergência

conver_ar1<-rbind(ar1_bh$fit$convergence)
colnames(conver_ar1)<-c("convergence") 

# Parâmetros estimados:

parametros_ar1<-rbind(c(round(exp(ar1_bh$fit$par),4)))
row.names(parametros_ar1)<-c("BSM_error")
colnames(parametros_ar1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1<-rbind(2*(ar1_bh$fit$value)+2*5)
colnames(AIC_ar1)<-"AIC"

BIC_ar1<-2*(ar1_bh$fit$value)+2*5*log(ar1_bh$T)

# Matriz Hessiana

all(eigen(ar1_bh$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ar1<-list(ar1_bh)
testes_ar1<-sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1<-t(testes_ar1)
row.names(testes_ar1)<-c("BSM_error")
colnames(testes_ar1)<-c("Shapiro","Box","H")
resultadosbh_ar1<-cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
resultadosbh_ar1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_bh$ts.original, start = 2012, frequency = 4),
  ts(ar1_bh$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_bh$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_bh$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Smooth Belo Horizonte (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_bh$ts.original, start = 2012, frequency = 4),ts(ar1_bh$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_bh$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_bh$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_bh$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

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

#result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
#result_mods_deso[["01-Belo Horizonte"]][["sinal_smooth_ar1bh"]]<-ar1_bh$ts.signal
#result_mods_deso[["01-Belo Horizonte"]][["cv_sinal_smooth_ar1bh"]]<-ar1_bh$cv.signal
#saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")

#### MODELO MA(1)

source("data/funcoes/08_smooth_MA1.R")
theta1_ma1 <- dbbh[["mod_ma1"]][["theta1_ma1_dbh"]]

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

# Estimação do Modelo

start_time <- Sys.time()
run_ma1bh<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.smooth_ma1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ma1_bh <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ma1bh[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ma1bh[[i]][["fit"]][["convergence"]]
      log_like <- run_ma1bh[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ma1_bh) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                          "slope","seasonal","irregular", "sampl_error",
                          "convergence","log_like")

## Após iniciais, seleção do modelo:

ma1_bh <- run_ma1bh[[which(
  ini_ma1_bh$log_like == min(ini_ma1_bh$log_like[ini_ma1_bh$convergence == 0], na.rm = TRUE) & 
    ini_ma1_bh$convergence == 0
)]]

# Verificando a convergência

conver_ma1<-rbind(ma1_bh$fit$convergence)
colnames(conver_ma1)<-c("convergence") 

# Parâmetros estimados:

parametros_ma1<-rbind(c(round(exp(ma1_bh$fit$par),4)))
row.names(parametros_ma1)<-c("BSM_error")
colnames(parametros_ma1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1<-rbind(2*(ma1_bh$fit$value)+2*5)
colnames(AIC_ma1)<-"AIC"

BIC_ma1<-2*(ma1_bh$fit$value)+2*5*log(ma1_bh$T)

# Matriz Hessiana

all(eigen(ma1_bh$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ma1<-list(ma1_bh)
testes_ma1<-sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1<-t(testes_ma1)
row.names(testes_ma1)<-c("BSM_error")
colnames(testes_ma1)<-c("Shapiro","Box","H")
resultadosbh_ma1<-cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
resultadosbh_ma1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ma1 <- window(ts.union(
  ts(ma1_bh$ts.original, start = 2012, frequency = 4),
  ts(ma1_bh$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ma1.cv <- window(ts.union(
  ts((ma1_bh$cv.original) * 100, start = 2012, frequency = 4),
  ts(ma1_bh$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ma1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Smooth Belo Horizonte (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

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

#result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
#result_mods_deso[["01-Belo Horizonte"]][["sinal_smooth_ma1bh"]]<-ma1_bh$ts.signal
#result_mods_deso[["01-Belo Horizonte"]][["cv_sinal_smooth_ma1bh"]]<-ma1_bh$cv.signal
#saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")

# Salvando o .Rdata

#save.image(file = "D:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/01_mod_bh.Rdata")

### COLAR e ENTORNO METROPOLITANO ###############################################

# Modelos para Entorno: AR(1); MA(1), ARMA(1,1)

rm(list = ls())

## UCM: 106.7323; 0.00348154; 106.8907

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

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO AR(1)

source("data/funcoes/07_smooth_AR1.R")
phi1_ar1 <- dbent[["mod_ar1"]][["phi1_ar1_dent"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ar1ent<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.smooth_ar1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ar1_ent <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ar1ent[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ar1ent[[i]][["fit"]][["convergence"]]
      log_like <- run_ar1ent[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ar1_ent) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Após iniciais, seleção do modelo:

ar1_ent <- run_ar1ent[[which(
  ini_ar1_ent$log_like == min(ini_ar1_ent$log_like[ini_ar1_ent$convergence == 0], na.rm = TRUE) & 
    ini_ar1_ent$convergence == 0
)]]

# Verificando a convergência

conver_ar1<-rbind(ar1_ent$fit$convergence)
colnames(conver_ar1)<-c("convergence")

# Parâmetros estimados:

parametros_ar1<-rbind(c(round(exp(ar1_ent$fit$par),4)))
row.names(parametros_ar1)<-c("BSM_error")
colnames(parametros_ar1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1<-rbind(2*(ar1_ent$fit$value)+2*5)
colnames(AIC_ar1)<-"AIC"

BIC_ar1<-2*(ar1_ent$fit$value)+2*5*log(ar1_ent$T)

# Matriz Hessiana

all(eigen(ar1_ent$fit$hessian, only.values = TRUE)$values > 0) # true

# Diagnosticando os resíduos

lista_ar1<-list(ar1_ent)
testes_ar1<-sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1<-t(testes_ar1)
row.names(testes_ar1)<-c("BSM_error")
colnames(testes_ar1)<-c("Shapiro","Box","H")
resultadosent_ar1<-cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
resultadosent_ar1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_ent$ts.original, start = 2012, frequency = 4),
  ts(ar1_ent$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
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
mtext("02 - Smooth Colar e Entorno Metropolitano de BH (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_ent$ts.original, start = 2012, frequency = 4),ts(ar1_ent$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_ent$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_ent$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_ent$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

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

#result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
#result_mods_deso[["02-Colar e Entorno Metropolitano de BH"]][["sinal_smooth_ar1ent"]]<-ar1_ent$ts.signal
#result_mods_deso[["02-Colar e Entorno Metropolitano de BH"]][["cv_sinal_smooth_ar1ent"]]<-ar1_ent$cv.signal
#saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")



#### MODELO MA(1)

source("data/funcoes/08_smooth_MA1.R")
theta1_ma1 <- dbent[["mod_ma1"]][["theta1_ma1_dent"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ma1ent<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_ma1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ma1_ent <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ma1ent[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ma1ent[[i]][["fit"]][["convergence"]]
      log_like <- run_ma1ent[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ma1_ent) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Após iniciais, seleção do modelo:

ma1_ent <- run_ma1ent[[which(
  ini_ma1_ent$log_like == min(ini_ma1_ent$log_like[ini_ma1_ent$convergence == 0], na.rm = TRUE) &
    ini_ma1_ent$convergence == 0
)]]

# Verificando a convergência

conver_ma1<-rbind(ma1_ent$fit$convergence)
colnames(conver_ma1)<-c("convergence")

# Parâmetros estimados:

parametros_ma1<-rbind(c(round(exp(ma1_ent$fit$par),4)))
row.names(parametros_ma1)<-c("BSM_error")
colnames(parametros_ma1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1<-rbind(2*(ma1_ent$fit$value)+2*5)
colnames(AIC_ma1)<-"AIC"

BIC_ma1<-2*(ma1_ent$fit$value)+2*5*log(ma1_ent$T)

# Matriz Hessiana

all(eigen(ma1_ent$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ma1<-list(ma1_ent)
testes_ma1<-sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1<-t(testes_ma1)
row.names(testes_ma1)<-c("BSM_error")
colnames(testes_ma1)<-c("Shapiro","Box","H")
resultadosent_ma1<-cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
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
mtext("02 - Smooth Colar e Entorno Metropolitano de BH (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

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

#result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
#result_mods_deso[["02-Colar e Entorno Metropolitano de BH"]][["sinal_smooth_ma1ent"]]<-ma1_ent$ts.signal
#result_mods_deso[["02-Colar e Entorno Metropolitano de BH"]][["cv_sinal_smooth_ma1ent"]]<-ma1_ent$cv.signal
#saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")



#### MODELO ARMA(1,1)

source("data/funcoes/09_smooth_ARMA11.R")
phi1_arma11<- dbent[["mod_arma11"]][["phi1_arma11_dent"]]
theta1_arma11 <- dbent[["mod_arma11"]][["theta1_arma11_dent"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_arma11ent<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_arma11(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_arma11_ent <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_arma11ent[[i]][["fit"]][["par"]]), 4)
      convergence <- run_arma11ent[[i]][["fit"]][["convergence"]]
      log_like <- run_arma11ent[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_arma11_ent) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                              "slope","seasonal","irregular", "sampl_error",
                              "convergence","log_like")

## Após iniciais, seleção do modelo:

arma11_ent <- run_arma11ent[[which(
  ini_arma11_ent$log_like == min(ini_arma11_ent$log_like[ini_arma11_ent$convergence == 0], na.rm = TRUE) &
    ini_arma11_ent$convergence == 0
)]]

# Verificando a convergência

conver_arma11<-rbind(arma11_ent$fit$convergence)
colnames(conver_arma11)<-c("convergence")

# Parâmetros estimados:

parametros_arma11<-rbind(c(round(exp(arma11_ent$fit$par),4)))
row.names(parametros_arma11)<-c("BSM_error")
colnames(parametros_arma11)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_arma11<-rbind(2*(arma11_ent$fit$value)+2*5)
colnames(AIC_arma11)<-"AIC"

BIC_arma11<-2*(arma11_ent$fit$value)+2*5*log(arma11_ent$T)

# Matriz Hessiana

all(eigen(arma11_ent$fit$hessian, only.values = TRUE)$values > 0) # true

# Diagnosticando os resíduos

lista_arma11<-list(arma11_ent)
testes_arma11<-sapply(lista_arma11, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                       round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                       teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_arma11<-t(testes_arma11)
row.names(testes_arma11)<-c("BSM_error")
colnames(testes_arma11)<-c("Shapiro","Box","H")
resultadosent_arma11<-cbind(conver_arma11, parametros_arma11, testes_arma11, AIC_arma11, BIC_arma11)
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
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("02 - Smooth Colar e Entorno Metropolitano de BH (ARMA(1,1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE ARMA(1,1)

figtend_arma11<-window(ts.union(ts(arma11_ent$ts.original, start = 2012, frequency = 4),ts(arma11_ent$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_arma11<-window(ts.union(ts(arma11_ent$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_arma11<-window(ts.union(ts(arma11_ent$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_arma11<-window(ts.union(ts(arma11_ent$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

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

#result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
#result_mods_deso[["02-Colar e Entorno Metropolitano de BH"]][["sinal_smooth_arma11ent"]]<-arma11_ent$ts.signal
#result_mods_deso[["02-Colar e Entorno Metropolitano de BH"]][["cv_sinal_smooth_arma11ent"]]<-arma11_ent$cv.signal
#saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
#rm(result_mods_deso)

## GRÁFICO UNIFICADO:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_ent <- window(ts.union(
  ts(ma1_ent$ts.original, start = 2012, frequency = 4),
  ts(ar1_ent$ts.signal, start = 2012, frequency = 4),
  ts(ma1_ent$ts.signal, start = 2012, frequency = 4) 
), start=c(2013,3))
plot(fig_ent, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
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

#save.image(file = "D:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/02_mod_ent.Rdata")

### SUL DE MINAS ###############################################################

# Modelos para Sul: AR(1); MA(1), ARMA(1,1)

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

y <- sul$Total.de.desocupados/1000
se_db <- sul$sd_d/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO AR(1)

source("data/funcoes/07_smooth_AR1.R")
phi1_ar1 <- dbsul[["mod_ar1"]][["phi1_ar1_dsul"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ar1sul<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.smooth_ar1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ar1_sul <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ar1sul[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ar1sul[[i]][["fit"]][["convergence"]]
      log_like <- run_ar1sul[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ar1_sul) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Após iniciais, seleção do modelo:

ar1_sul <- run_ar1sul[[which(
  ini_ar1_sul$log_like == min(ini_ar1_sul$log_like[ini_ar1_sul$convergence == 0], na.rm = TRUE) & 
    ini_ar1_sul$convergence == 0
)]]

# Verificando a convergência

conver_ar1<-rbind(ar1_sul$fit$convergence)
colnames(conver_ar1)<-c("convergence")

# Parâmetros estimados:

parametros_ar1<-rbind(c(round(exp(ar1_sul$fit$par),4)))
row.names(parametros_ar1)<-c("BSM_error")
colnames(parametros_ar1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1<-rbind(2*(ar1_sul$fit$value)+2*5)
colnames(AIC_ar1)<-"AIC"

BIC_ar1<-2*(ar1_sul$fit$value)+2*5*log(ar1_sul$T)

# Matriz Hessiana

all(eigen(ar1_sul$fit$hessian, only.values = TRUE)$values > 0) # true

# Diagnosticando os resíduos

lista_ar1<-list(ar1_sul)
testes_ar1<-sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1<-t(testes_ar1)
row.names(testes_ar1)<-c("BSM_error")
colnames(testes_ar1)<-c("Shapiro","Box","H")
resultadossul_ar1<-cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
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
mtext("03 - Smooth Sul de Minas (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_sul$ts.original, start = 2012, frequency = 4),ts(ar1_sul$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_sul$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_sul$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_sul$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

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
mtext("03 - Smooth Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

#result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
#result_mods_deso[["03-Sul de Minas"]][["sinal_smooth_ar1sul"]]<-ar1_sul$ts.signal
#result_mods_deso[["03-Sul de Minas"]][["cv_sinal_smooth_ar1sul"]]<-ar1_sul$cv.signal
#saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
#rm(result_mods_deso)

#### MODELO MA(1)

source("data/funcoes/08_smooth_MA1.R")
theta1_ma1 <- dbsul[["mod_ma1"]][["theta1_ma1_dsul"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ma1sul<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_ma1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ma1_sul <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ma1sul[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ma1sul[[i]][["fit"]][["convergence"]]
      log_like <- run_ma1sul[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ma1_sul) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Após iniciais, seleção do modelo:

ma1_sul <- run_ma1sul[[which(
  ini_ma1_sul$log_like == min(ini_ma1_sul$log_like[ini_ma1_sul$convergence == 0], na.rm = TRUE) &
    ini_ma1_sul$convergence == 0
)]]

# Verificando a convergência

conver_ma1<-rbind(ma1_sul$fit$convergence)
colnames(conver_ma1)<-c("convergence")

# Parâmetros estimados:

parametros_ma1<-rbind(c(round(exp(ma1_sul$fit$par),4)))
row.names(parametros_ma1)<-c("BSM_error")
colnames(parametros_ma1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1<-rbind(2*(ma1_sul$fit$value)+2*5)
colnames(AIC_ma1)<-"AIC"

BIC_ma1<-2*(ma1_sul$fit$value)+2*5*log(ma1_sul$T)

# Matriz Hessiana

all(eigen(ma1_sul$fit$hessian, only.values = TRUE)$values > 0) # true

# Diagnosticando os resíduos

lista_ma1<-list(ma1_sul)
testes_ma1<-sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1<-t(testes_ma1)
row.names(testes_ma1)<-c("BSM_error")
colnames(testes_ma1)<-c("Shapiro","Box","H")
resultadossul_ma1<-cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
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
mtext("03 - Smooth Sul de Minas (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE MA(1)

figtend_ma1<-window(ts.union(ts(ma1_sul$ts.original, start = 2012, frequency = 4),ts(ma1_sul$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ma1<-window(ts.union(ts(ma1_sul$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ma1<-window(ts.union(ts(ma1_sul$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ma1<-window(ts.union(ts(ma1_sul$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

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
mtext("03 - Smooth Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["03-Sul de Minas"]][["sinal_smooth_ma1sul"]]<-ma1_sul$ts.signal
result_mods_deso[["03-Sul de Minas"]][["cv_sinal_smooth_ma1sul"]]<-ma1_sul$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)


#### MODELO ARMA(1,1)

source("data/funcoes/09_smooth_ARMA11.R")
phi1_arma11<- dbsul[["mod_arma11"]][["phi1_arma11_dsul"]]
theta1_arma11 <- dbsul[["mod_arma11"]][["theta1_arma11_dsul"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_arma11sul<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_arma11(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_arma11_sul <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_arma11sul[[i]][["fit"]][["par"]]), 4)
      convergence <- run_arma11sul[[i]][["fit"]][["convergence"]]
      log_like <- run_arma11sul[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_arma11_sul) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                              "slope","seasonal","irregular", "sampl_error",
                              "convergence","log_like")

## Após iniciais, seleção do modelo:

arma11_sul <- run_arma11sul[[which(
  ini_arma11_sul$log_like == min(ini_arma11_sul$log_like[ini_arma11_sul$convergence == 0], na.rm = TRUE) &
    ini_arma11_sul$convergence == 0
)]]

# Verificando a convergência

conver_arma11<-rbind(arma11_sul$fit$convergence)
colnames(conver_arma11)<-c("convergence")

# Parâmetros estimados:

parametros_arma11<-rbind(c(round(exp(arma11_sul$fit$par),4)))
row.names(parametros_arma11)<-c("BSM_error")
colnames(parametros_arma11)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_arma11<-rbind(2*(arma11_sul$fit$value)+2*5)
colnames(AIC_arma11)<-"AIC"

BIC_arma11<-2*(arma11_sul$fit$value)+2*5*log(arma11_sul$T)

# Matriz Hessiana

all(eigen(arma11_sul$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_arma11<-list(arma11_sul)
testes_arma11<-sapply(lista_arma11, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                       round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                       teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_arma11<-t(testes_arma11)
row.names(testes_arma11)<-c("BSM_error")
colnames(testes_arma11)<-c("Shapiro","Box","H")
resultadosul_arma11<-cbind(conver_arma11, parametros_arma11, testes_arma11, AIC_arma11, BIC_arma11)
resultadosul_arma11

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
mtext("03 - Sul de Minas (ARMA(1,1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

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
mtext("03 - Smooth Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["03-Sul de Minas"]][["sinal_smooth_arma11sul"]]<-arma11_sul$ts.signal
result_mods_deso[["03-Sul de Minas"]][["cv_sinal_smooth_arma11sul"]]<-arma11_sul$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)

## GRÁFICO UNIFICADO:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_sul <- window(ts.union(
  ts(ma1_sul$ts.original, start = 2012, frequency = 4),
  ts(ar1_sul$ts.signal, start = 2012, frequency = 4),
  ts(ma1_sul$ts.signal, start = 2012, frequency = 4),
  ts(arma11_sul$ts.signal,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_sul, plot.type = "single", col = c(1,4,2,3), ylab="", xlab="", lty = c(1,1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação AR(1): model-based",
                            "Sinal da Desocupação MA(1)",
                            "Sinal da Desocupação ARMA(1,1)"),
       lty = c(1,1,1,1), col = c(1,4,2,3), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_sul.cv <- window(ts.union(
  ts((ma1_sul$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_sul$cv.signal, start = 2012, frequency = 4),
  ts(ma1_sul$cv.signal, start = 2012, frequency = 4),
  ts(arma11_sul$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_sul.cv, plot.type = "single", col = c(1,4,2,3), ylab="", xlab="", lty = c(1,1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados AR(1): model-based",
                             "Sinal CV desocupados MA(1): model-based",
                             "Sinal CV desocupados ARMA(1,1)"),
       lty = c(1,1,1,1), col = c(1,4,2,3), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Sul de Minas (AR, MA e ARMA)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/03_mod_sul.Rdata")

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

y <- trg$Total.de.desocupados/1000
se_db <- trg$sd_d/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO AR(1)

source("data/funcoes/07_smooth_AR1.R")
phi1_ar1 <- dbtrg[["mod_ar1"]][["phi1_ar1_dtrg"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ar1trg<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_ar1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ar1_trg <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ar1trg[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ar1trg[[i]][["fit"]][["convergence"]]
      log_like <- run_ar1trg[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ar1_trg) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Após iniciais, seleção do modelo:

ar1_trg <- run_ar1trg[[which(
  ini_ar1_trg$log_like == min(ini_ar1_trg$log_like[ini_ar1_trg$convergence == 0], na.rm = TRUE) &
    ini_ar1_trg$convergence == 0
)]]

# Verificando a convergência

conver_ar1<-rbind(ar1_trg$fit$convergence)
colnames(conver_ar1)<-c("convergence")

# Parâmetros estimados:

parametros_ar1<-rbind(c(round(exp(ar1_trg$fit$par),4)))
row.names(parametros_ar1)<-c("BSM_error")
colnames(parametros_ar1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1<-rbind(2*(ar1_trg$fit$value)+2*5)
colnames(AIC_ar1)<-"AIC"

BIC_ar1<-2*(ar1_trg$fit$value)+2*5*log(ar1_trg$T)

# Matriz Hessiana

all(eigen(ar1_trg$fit$hessian, only.values = TRUE)$values > 0) # true

# Diagnosticando os resíduos

lista_ar1<-list(ar1_trg)
testes_ar1<-sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1<-t(testes_ar1)
row.names(testes_ar1)<-c("BSM_error")
colnames(testes_ar1)<-c("Shapiro","Box","H")
resultadostrg_ar1<-cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
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
mtext("04 - Triângulo Mineiro (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_trg$ts.original, start = 2012, frequency = 4),ts(ar1_trg$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_trg$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_trg$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_trg$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

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
mtext("04 - Triângulo Mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["04-Triângulo Mineiro"]][["sinal_smooth_ar1trg"]]<-ar1_trg$ts.signal
result_mods_deso[["04-Triângulo Mineiro"]][["cv_sinal_smooth_ar1trg"]]<-ar1_trg$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)



#### MODELO MA(1)

source("data/funcoes/08_smooth_MA1.R")
theta1_ma1 <- dbtrg[["mod_ma1"]][["theta1_ma1_dtrg"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ma1trg<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_ma1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ma1_trg <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ma1trg[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ma1trg[[i]][["fit"]][["convergence"]]
      log_like <- run_ma1trg[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ma1_trg) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Após iniciais, seleção do modelo:

ma1_trg <- run_ma1trg[[which(
  ini_ma1_trg$log_like == min(ini_ma1_trg$log_like[ini_ma1_trg$convergence == 0], na.rm = TRUE) &
    ini_ma1_trg$convergence == 0
)]]

# Verificando a convergência

conver_ma1<-rbind(ma1_trg$fit$convergence)
colnames(conver_ma1)<-c("convergence")

# Parâmetros estimados:

parametros_ma1<-rbind(c(round(exp(ma1_trg$fit$par),4)))
row.names(parametros_ma1)<-c("BSM_error")
colnames(parametros_ma1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1<-rbind(2*(ma1_trg$fit$value)+2*5)
colnames(AIC_ma1)<-"AIC"

BIC_ma1<-2*(ma1_trg$fit$value)+2*5*log(ma1_trg$T)

# Matriz Hessiana

all(eigen(ma1_trg$fit$hessian, only.values = TRUE)$values > 0) # true

# Diagnosticando os resíduos

lista_ma1<-list(ma1_trg)
testes_ma1<-sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1<-t(testes_ma1)
row.names(testes_ma1)<-c("BSM_error")
colnames(testes_ma1)<-c("Shapiro","Box","H")
resultadostrg_ma1<-cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
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
mtext("04 - Triângulo Mineiro (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

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
mtext("04 - Triângulo Mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["04-Triângulo Mineiro"]][["sinal_smooth_ma1trg"]]<-ma1_trg$ts.signal
result_mods_deso[["04-Triângulo Mineiro"]][["cv_sinal_smooth_ma1trg"]]<-ma1_trg$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)


#### GRÁFICO UNIFICADO

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_trg <- window(ts.union(
  ts(ma1_trg$ts.original, start = 2012, frequency = 4),
  ts(ar1_trg$ts.signal, start = 2012, frequency = 4),
  ts(ma1_trg$ts.signal, start = 2012, frequency = 4) 
), start=c(2013,3))
plot(fig_trg, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação AR(1): model-based",
                            "Sinal da Desocupação MA(1)"),
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
mtext("04 - Triângulo Mineiro (AR e MA)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/04_mod_trg.Rdata")

### ZONA DA MATA ###############################################################
# Modelos para Mata: AR(1); MA(1), ARMA(1,1)

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

y <- mat$Total.de.desocupados/1000
se_db <- mat$sd_d/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO AR(1)

source("data/funcoes/07_smooth_AR1.R")
phi1_ar1 <- dbmat[["mod_ar1"]][["phi1_ar1_dmat"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ar1mat<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_ar1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ar1_mat <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ar1mat[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ar1mat[[i]][["fit"]][["convergence"]]
      log_like <- run_ar1mat[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ar1_mat) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Após iniciais, seleção do modelo:

ar1_mat <- run_ar1mat[[which(
  ini_ar1_mat$log_like == min(ini_ar1_mat$log_like[ini_ar1_mat$convergence == 0], na.rm = TRUE) &
    ini_ar1_mat$convergence == 0
)]]

# Verificando a convergência

conver_ar1<-rbind(ar1_mat$fit$convergence)
colnames(conver_ar1)<-c("convergence")

# Parâmetros estimados:

parametros_ar1<-rbind(c(round(exp(ar1_mat$fit$par),4)))
row.names(parametros_ar1)<-c("BSM_error")
colnames(parametros_ar1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1<-rbind(2*(ar1_mat$fit$value)+2*5)
colnames(AIC_ar1)<-"AIC"

BIC_ar1<-2*(ar1_mat$fit$value)+2*5*log(ar1_mat$T)

# Matriz Hessiana

all(eigen(ar1_mat$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ar1<-list(ar1_mat)
testes_ar1<-sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1<-t(testes_ar1)
row.names(testes_ar1)<-c("BSM_error")
colnames(testes_ar1)<-c("Shapiro","Box","H")
resultadosmat_ar1<-cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
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
mtext("05 - Zona da Mata (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

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
mtext("05 - Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["05-Zona da Mata"]][["sinal_smooth_ar1mat"]]<-ar1_mat$ts.signal
result_mods_deso[["05-Zona da Mata"]][["cv_sinal_smooth_ar1mat"]]<-ar1_mat$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)



#### MODELO MA(1)

source("data/funcoes/08_smooth_MA1.R")
theta1_ma1 <- dbmat[["mod_ma1"]][["theta1_ma1_dmat"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ma1mat<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_ma1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ma1_mat <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ma1mat[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ma1mat[[i]][["fit"]][["convergence"]]
      log_like <- run_ma1mat[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ma1_mat) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Após iniciais, seleção do modelo:

ma1_mat <- run_ma1mat[[which(
  ini_ma1_mat$log_like == min(ini_ma1_mat$log_like[ini_ma1_mat$convergence == 0], na.rm = TRUE) &
    ini_ma1_mat$convergence == 0
)]]

# Verificando a convergência

conver_ma1<-rbind(ma1_mat$fit$convergence)
colnames(conver_ma1)<-c("convergence")

# Parâmetros estimados:

parametros_ma1<-rbind(c(round(exp(ma1_mat$fit$par),4)))
row.names(parametros_ma1)<-c("BSM_error")
colnames(parametros_ma1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1<-rbind(2*(ma1_mat$fit$value)+2*5)
colnames(AIC_ma1)<-"AIC"

BIC_ma1<-2*(ma1_mat$fit$value)+2*5*log(ma1_mat$T)

# Matriz Hessiana

all(eigen(ma1_mat$fit$hessian, only.values = TRUE)$values > 0) # true

# Diagnosticando os resíduos

lista_ma1<-list(ma1_mat)
testes_ma1<-sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1<-t(testes_ma1)
row.names(testes_ma1)<-c("BSM_error")
colnames(testes_ma1)<-c("Shapiro","Box","H")
resultadosmat_ma1<-cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
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
mtext("05-Zona da Mata (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

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
mtext("05 - Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["05-Zona da Mata"]][["sinal_smooth_ma1mat"]]<-ma1_mat$ts.signal
result_mods_deso[["05-Zona da Mata"]][["cv_sinal_smooth_ma1mat"]]<-ma1_mat$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)



#### MODELO ARMA(1,1)

source("data/funcoes/09_smooth_ARMA11.R")
phi1_arma11<- dbmat[["mod_arma11"]][["phi1_arma11_dmat"]]
theta1_arma11 <- dbmat[["mod_arma11"]][["theta1_arma11_dmat"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_arma11mat<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_arma11(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_arma11_mat <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_arma11mat[[i]][["fit"]][["par"]]), 4)
      convergence <- run_arma11mat[[i]][["fit"]][["convergence"]]
      log_like <- run_arma11mat[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_arma11_mat) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                              "slope","seasonal","irregular", "sampl_error",
                              "convergence","log_like")

## Após iniciais, seleção do modelo:

arma11_mat <- run_arma11mat[[which(
  ini_arma11_mat$log_like == min(ini_arma11_mat$log_like[ini_arma11_mat$convergence == 0], na.rm = TRUE) &
    ini_arma11_mat$convergence == 0
)]]

# Verificando a convergência

conver_arma11<-rbind(arma11_mat$fit$convergence)
colnames(conver_arma11)<-c("convergence")

# Parâmetros estimados:

parametros_arma11<-rbind(c(round(exp(arma11_mat$fit$par),4)))
row.names(parametros_arma11)<-c("BSM_error")
colnames(parametros_arma11)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_arma11<-rbind(2*(arma11_mat$fit$value)+2*5)
colnames(AIC_arma11)<-"AIC"

BIC_arma11<-2*(arma11_mat$fit$value)+2*5*log(arma11_mat$T)

# Matriz Hessiana

all(eigen(arma11_mat$fit$hessian, only.values = TRUE)$values > 0) # true

# Diagnosticando os resíduos

lista_arma11<-list(arma11_mat)
testes_arma11<-sapply(lista_arma11, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                       round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                       teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_arma11<-t(testes_arma11)
row.names(testes_arma11)<-c("BSM_error")
colnames(testes_arma11)<-c("Shapiro","Box","H")
resultadosmat_arma11<-cbind(conver_arma11, parametros_arma11, testes_arma11, AIC_arma11, BIC_arma11)
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
mtext("05-Zona da Mata (ARMA(1,1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE ARMA(1,1)

figtend_arma11<-window(ts.union(ts(arma11_mat$ts.original, start = 2012, frequency = 4),ts(arma11_mat$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_arma11<-window(ts.union(ts(arma11_mat$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_arma11<-window(ts.union(ts(arma11_mat$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_arma11<-window(ts.union(ts(arma11_mat$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

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
mtext("05 - Smooth Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["05-Zona da Mata"]][["sinal_smooth_arma11mat"]]<-arma11_mat$ts.signal
result_mods_deso[["05-Zona da Mata"]][["cv_sinal_smooth_arma11mat"]]<-arma11_mat$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)

## GRÁFICO UNIFICADO:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_mat <- window(ts.union(
  ts(ma1_mat$ts.original, start = 2012, frequency = 4),
  ts(ar1_mat$ts.signal, start = 2012, frequency = 4),
  ts(ma1_mat$ts.signal, start = 2012, frequency = 4),
  ts(arma11_mat$ts.signal,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_mat, plot.type = "single", col = c(1,4,2,3), ylab="", xlab="", lty = c(1,1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação AR(1): model-based",
                            "Sinal da Desocupação MA(1)",
                            "Sinal da Desocupação ARMA(1,1)"),
       lty = c(1,1,1,1), col = c(1,4,2,3), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_mat.cv <- window(ts.union(
  ts((ma1_mat$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_mat$cv.signal, start = 2012, frequency = 4),
  ts(ma1_mat$cv.signal, start = 2012, frequency = 4),
  ts(arma11_mat$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_mat.cv, plot.type = "single", col = c(1,4,2,3), ylab="", xlab="", lty = c(1,1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados AR(1): model-based",
                             "Sinal CV desocupados MA(1): model-based",
                             "Sinal CV desocupados ARMA(1,1)"),
       lty = c(1,1,1,1), col = c(1,4,2,3), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05-Zona da Mata (AR, MA e ARMA)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

#save.image(file = "D:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/05_mod_mat.Rdata")

### NORTE DE MINAS GERAIS ######################################################
# Modelos para Norte: AR(1); MA(1); ARMA(1,1)

rm(list = ls())

# UCM: # 43.22508 # 0.0009056865 # 142.2077

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

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO AR(1)

source("data/funcoes/07_smooth_AR1.R")
phi1_ar1 <- dbnrt[["mod_ar1"]][["phi1_ar1_dnrt"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ar1nrt<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_ar1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ar1_nrt <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ar1nrt[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ar1nrt[[i]][["fit"]][["convergence"]]
      log_like <- run_ar1nrt[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ar1_nrt) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Após iniciais, seleção do modelo:

ar1_nrt <- run_ar1nrt[[which(
  ini_ar1_nrt$log_like == min(ini_ar1_nrt$log_like[ini_ar1_nrt$convergence == 0], na.rm = TRUE) &
    ini_ar1_nrt$convergence == 0
)]]

# Verificando a convergência

conver_ar1<-rbind(ar1_nrt$fit$convergence)
colnames(conver_ar1)<-c("convergence")

# Parâmetros estimados:

parametros_ar1<-rbind(c(round(exp(ar1_nrt$fit$par),4)))
row.names(parametros_ar1)<-c("BSM_error")
colnames(parametros_ar1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1<-rbind(2*(ar1_nrt$fit$value)+2*5)
colnames(AIC_ar1)<-"AIC"

BIC_ar1<-2*(ar1_nrt$fit$value)+2*5*log(ar1_nrt$T)

# Matriz Hessiana

all(eigen(ar1_nrt$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ar1<-list(ar1_nrt)
testes_ar1<-sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1<-t(testes_ar1)
row.names(testes_ar1)<-c("BSM_error")
colnames(testes_ar1)<-c("Shapiro","Box","H")
resultadosnrt_ar1<-cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
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
mtext("06-Norte de Minas (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_nrt$ts.original, start = 2012, frequency = 4),ts(ar1_nrt$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_nrt$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_nrt$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_nrt$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

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
mtext("06-Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["06-Norte de Minas"]][["sinal_smooth_ar1nrt"]]<-ar1_nrt$ts.signal
result_mods_deso[["06-Norte de Minas"]][["cv_sinal_smooth_ar1nrt"]]<-ar1_nrt$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)



#### MODELO MA(1)

source("data/funcoes/08_smooth_MA1.R")
theta1_ma1 <- dbnrt[["mod_ma1"]][["theta1_ma1_dnrt"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ma1nrt<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_ma1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ma1_nrt <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ma1nrt[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ma1nrt[[i]][["fit"]][["convergence"]]
      log_like <- run_ma1nrt[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ma1_nrt) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Após iniciais, seleção do modelo:

ma1_nrt <- run_ma1nrt[[which(
  ini_ma1_nrt$log_like == min(ini_ma1_nrt$log_like[ini_ma1_nrt$convergence == 0], na.rm = TRUE) &
    ini_ma1_nrt$convergence == 0
)]]

# Verificando a convergência

conver_ma1<-rbind(ma1_nrt$fit$convergence)
colnames(conver_ma1)<-c("convergence")

# Parâmetros estimados:

parametros_ma1<-rbind(c(round(exp(ma1_nrt$fit$par),4)))
row.names(parametros_ma1)<-c("BSM_error")
colnames(parametros_ma1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1<-rbind(2*(ma1_nrt$fit$value)+2*5)
colnames(AIC_ma1)<-"AIC"

BIC_ma1<-2*(ma1_nrt$fit$value)+2*5*log(ma1_nrt$T)

# Matriz Hessiana

all(eigen(ma1_nrt$fit$hessian, only.values = TRUE)$values > 0) # true

# Diagnosticando os resíduos

lista_ma1<-list(ma1_nrt)
testes_ma1<-sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1<-t(testes_ma1)
row.names(testes_ma1)<-c("BSM_error")
colnames(testes_ma1)<-c("Shapiro","Box","H")
resultadosnrt_ma1<-cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
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
mtext("06-Norte de Minas MA(1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

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
mtext("06-Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["06-Norte de Minas"]][["sinal_smooth_ma1nrt"]]<-ma1_nrt$ts.signal
result_mods_deso[["06-Norte de Minas"]][["cv_sinal_smooth_ma1nrt"]]<-ma1_nrt$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)



#### MODELO ARMA(1,1)

source("data/funcoes/09_smooth_ARMA11.R")
phi1_arma11<- dbnrt[["mod_arma11"]][["phi1_arma11_dnrt"]]
theta1_arma11 <- dbnrt[["mod_arma11"]][["theta1_arma11_dnrt"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_arma11nrt<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_arma11(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_arma11_nrt <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_arma11nrt[[i]][["fit"]][["par"]]), 4)
      convergence <- run_arma11nrt[[i]][["fit"]][["convergence"]]
      log_like <- run_arma11nrt[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_arma11_nrt) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                              "slope","seasonal","irregular", "sampl_error",
                              "convergence","log_like")

## Após iniciais, seleção do modelo:

arma11_nrt <- run_arma11nrt[[which(
  ini_arma11_nrt$log_like == min(ini_arma11_nrt$log_like[ini_arma11_nrt$convergence == 0], na.rm = TRUE) &
    ini_arma11_nrt$convergence == 0
)]]

# Verificando a convergência

conver_arma11<-rbind(arma11_nrt$fit$convergence)
colnames(conver_arma11)<-c("convergence")

# Parâmetros estimados:

parametros_arma11<-rbind(c(round(exp(arma11_nrt$fit$par),4)))
row.names(parametros_arma11)<-c("BSM_error")
colnames(parametros_arma11)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_arma11<-rbind(2*(arma11_nrt$fit$value)+2*5)
colnames(AIC_arma11)<-"AIC"

BIC_arma11<-2*(arma11_nrt$fit$value)+2*5*log(arma11_nrt$T)

# Matriz Hessiana

all(eigen(arma11_nrt$fit$hessian, only.values = TRUE)$values > 0) # TRUE

# Diagnosticando os resíduos

lista_arma11<-list(arma11_nrt)
testes_arma11<-sapply(lista_arma11, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                       round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                       teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_arma11<-t(testes_arma11)
row.names(testes_arma11)<-c("BSM_error")
colnames(testes_arma11)<-c("Shapiro","Box","H")
resultadosnrt_arma11<-cbind(conver_arma11, parametros_arma11, testes_arma11, AIC_arma11, BIC_arma11)
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
mtext("06-Norte de Minas (ARMA(1,1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE ARMA(1,1)

figtend_arma11<-window(ts.union(ts(arma11_nrt$ts.original, start = 2012, frequency = 4),ts(arma11_nrt$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_arma11<-window(ts.union(ts(arma11_nrt$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_arma11<-window(ts.union(ts(arma11_nrt$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_arma11<-window(ts.union(ts(arma11_nrt$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

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
mtext("06-Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["06-Norte de Minas"]][["sinal_smooth_arma11nrt"]]<-arma11_nrt$ts.signal
result_mods_deso[["06-Norte de Minas"]][["cv_sinal_smooth_arma11nrt"]]<-arma11_nrt$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)

## GRÁFICO UNIFICADO:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_nrt <- window(ts.union(
  ts(ma1_nrt$ts.original, start = 2012, frequency = 4),
  ts(ar1_nrt$ts.signal, start = 2012, frequency = 4),
  ts(ma1_nrt$ts.signal, start = 2012, frequency = 4),
  ts(arma11_nrt$ts.signal,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_nrt, plot.type = "single", col = c(1,4,2,3), ylab="", xlab="", lty = c(1,1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação AR(1): model-based",
                            "Sinal da Desocupação MA(1)",
                            "Sinal da Desocupação ARMA(1,1)"),
       lty = c(1,1,1,1), col = c(1,4,2,3), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_nrt.cv <- window(ts.union(
  ts((ma1_nrt$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_nrt$cv.signal, start = 2012, frequency = 4),
  ts(ma1_nrt$cv.signal, start = 2012, frequency = 4),
  ts(arma11_nrt$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_nrt.cv, plot.type = "single", col = c(1,4,2,3), ylab="", xlab="", lty = c(1,1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados AR(1): model-based",
                             "Sinal CV desocupados MA(1): model-based",
                             "Sinal CV desocupados ARMA(1,1)"),
       lty = c(1,1,1,1), col = c(1,4,2,3), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06-Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

# save.image(file = "D:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/06_mod_nrt.Rdata")

### VALE DO RIO DOCE ###########################################################
# Modelos para Vale: AR(1);

rm(list = ls())

# UCM: # 28.41725 # 1.194057 # 87.2518

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

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO AR(1)

source("data/funcoes/07_smooth_AR1.R")
phi1_ar1 <- dbvl[["mod_ar1"]][["phi1_ar1_drio"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ar1vl<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_ar1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ar1_vl <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ar1vl[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ar1vl[[i]][["fit"]][["convergence"]]
      log_like <- run_ar1vl[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ar1_vl) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                          "slope","seasonal","irregular", "sampl_error",
                          "convergence","log_like")

## Após iniciais, seleção do modelo:

ar1_vl <- run_ar1vl[[which(
  ini_ar1_vl$log_like == min(ini_ar1_vl$log_like[ini_ar1_vl$convergence == 0], na.rm = TRUE) &
    ini_ar1_vl$convergence == 0
)]]

# Verificando a convergência

conver_ar1<-rbind(ar1_vl$fit$convergence)
colnames(conver_ar1)<-c("convergence")

# Parâmetros estimados:

parametros_ar1<-rbind(c(round(exp(ar1_vl$fit$par),4)))
row.names(parametros_ar1)<-c("BSM_error")
colnames(parametros_ar1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1<-rbind(2*(ar1_vl$fit$value)+2*5)
colnames(AIC_ar1)<-"AIC"

BIC_ar1<-2*(ar1_vl$fit$value)+2*5*log(ar1_vl$T)

# Matriz Hessiana

all(eigen(ar1_vl$fit$hessian, only.values = TRUE)$values > 0) # true

# Diagnosticando os resíduos

lista_ar1<-list(ar1_vl)
testes_ar1<-sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1<-t(testes_ar1)
row.names(testes_ar1)<-c("BSM_error")
colnames(testes_ar1)<-c("Shapiro","Box","H")
resultadosvl_ar1<-cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
resultadosvl_ar1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_vl$ts.original, start = 2012, frequency = 4),
  ts(ar1_vl$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((ar1_vl$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_vl$cv.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados: model-based"),
       lty = c(1, 1), col = c(1, 4), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06-Norte de Minas (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_vl$ts.original, start = 2012, frequency = 4),ts(ar1_vl$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_vl$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_vl$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_vl$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

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
mtext("07-Vale do Rio Doce (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["07-Vale do Rio Doce"]][["sinal_smooth_ar1vl"]]<-ar1_vl$ts.signal
result_mods_deso[["07-Vale do Rio Doce"]][["cv_sinal_smooth_ar1vl"]]<-ar1_vl$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)

# Salvando o .Rdata

#save.image(file = "D:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/07_mod_val.Rdata")

### CENTRAL ####################################################################
# Modelos para Central: AR(1); MA(1)

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

y <- cen$Total.de.desocupados/1000
se_db <- cen$sd_d/1000
cv_db <- se_db/y

# Parâmetros iniciais:

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO AR(1)

source("data/funcoes/07_smooth_AR1.R")
phi1_ar1 <- dbcen[["mod_ar1"]][["phi1_ar1_dcen"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ar1cen<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_ar1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ar1_cen <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ar1cen[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ar1cen[[i]][["fit"]][["convergence"]]
      log_like <- run_ar1cen[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ar1_cen) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Após iniciais, seleção do modelo:

ar1_cen <- run_ar1cen[[which(
  ini_ar1_cen$log_like == min(ini_ar1_cen$log_like[ini_ar1_cen$convergence == 0], na.rm = TRUE) &
    ini_ar1_cen$convergence == 0
)]]

# Verificando a convergência

conver_ar1<-rbind(ar1_cen$fit$convergence)
colnames(conver_ar1)<-c("convergence")

# Parâmetros estimados:

parametros_ar1<-rbind(c(round(exp(ar1_cen$fit$par),4)))
row.names(parametros_ar1)<-c("BSM_error")
colnames(parametros_ar1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1<-rbind(2*(ar1_cen$fit$value)+2*5)
colnames(AIC_ar1)<-"AIC"

BIC_ar1<-2*(ar1_cen$fit$value)+2*5*log(ar1_cen$T)

# Matriz Hessiana

all(eigen(ar1_cen$fit$hessian, only.values = TRUE)$values > 0) # true

# Diagnosticando os resíduos

lista_ar1<-list(ar1_cen)
testes_ar1<-sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1<-t(testes_ar1)
row.names(testes_ar1)<-c("BSM_error")
colnames(testes_ar1)<-c("Shapiro","Box","H")
resultadoscen_ar1<-cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
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
mtext("08-Central (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_cen$ts.original, start = 2012, frequency = 4),ts(ar1_cen$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_cen$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_cen$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_cen$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

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
mtext("08-Central", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["08-Central"]][["sinal_smooth_ar1cen"]]<-ar1_cen$ts.signal
result_mods_deso[["08-Central"]][["cv_sinal_smooth_ar1cen"]]<-ar1_cen$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)



#### MODELO MA(1)

source("data/funcoes/08_smooth_MA1.R")
theta1_ma1 <- dbcen[["mod_ma1"]][["theta1_ma1_dcen"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ma1cen<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_ma1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ma1_cen <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ma1cen[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ma1cen[[i]][["fit"]][["convergence"]]
      log_like <- run_ma1cen[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ma1_cen) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                           "slope","seasonal","irregular", "sampl_error",
                           "convergence","log_like")

## Após iniciais, seleção do modelo:

ma1_cen <- run_ma1cen[[which(
  ini_ma1_cen$log_like == min(ini_ma1_cen$log_like[ini_ma1_cen$convergence == 0], na.rm = TRUE) &
    ini_ma1_cen$convergence == 0
)]]

# Verificando a convergência

conver_ma1<-rbind(ma1_cen$fit$convergence)
colnames(conver_ma1)<-c("convergence")

# Parâmetros estimados:

parametros_ma1<-rbind(c(round(exp(ma1_cen$fit$par),4)))
row.names(parametros_ma1)<-c("BSM_error")
colnames(parametros_ma1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1<-rbind(2*(ma1_cen$fit$value)+2*5)
colnames(AIC_ma1)<-"AIC"

BIC_ma1<-2*(ma1_cen$fit$value)+2*5*log(ma1_cen$T)

# Matriz Hessiana

all(eigen(ma1_cen$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ma1<-list(ma1_cen)
testes_ma1<-sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1<-t(testes_ma1)
row.names(testes_ma1)<-c("BSM_error")
colnames(testes_ma1)<-c("Shapiro","Box","H")
resultadoscen_ma1<-cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
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
mtext("08-Central (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

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
mtext("08 - Central", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["08-Central"]][["sinal_smooth_ma1cen"]]<-ma1_cen$ts.signal
result_mods_deso[["08-Central"]][["cv_sinal_smooth_ma1cen"]]<-ma1_cen$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)


#### GRÁFICO UNIFICADO

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_cen <- window(ts.union(
  ts(ma1_cen$ts.original, start = 2012, frequency = 4),
  ts(ar1_cen$ts.signal, start = 2012, frequency = 4),
  ts(ma1_cen$ts.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_cen, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação AR(1): model-based",
                            "Sinal da Desocupação MA(1)"),
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
mtext("08 - Central (AR e MA)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/08_mod_cen.Rdata")


### MINAS GERAIS ###############################################################

rm(list = ls())

# UCM: # 53.27499 # 0.001126252 # 83.0792

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

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

#### MODELO AR(1)

source("data/funcoes/07_smooth_AR1.R")
phi1_ar1 <- dbmg[["mod_ar1"]][["phi1_ar1_dmg"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ar1mg<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_ar1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ar1_mg <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ar1mg[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ar1mg[[i]][["fit"]][["convergence"]]
      log_like <- run_ar1mg[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ar1_mg) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                          "slope","seasonal","irregular", "sampl_error",
                          "convergence","log_like")

## Após iniciais, seleção do modelo:

ar1_mg <- run_ar1mg[[which(
  ini_ar1_mg$log_like == min(ini_ar1_mg$log_like[ini_ar1_mg$convergence == 0], na.rm = TRUE) &
    ini_ar1_mg$convergence == 0
)]]

# Verificando a convergência

conver_ar1<-rbind(ar1_mg$fit$convergence)
colnames(conver_ar1)<-c("convergence")

# Parâmetros estimados:

parametros_ar1<-rbind(c(round(exp(ar1_mg$fit$par),4)))
row.names(parametros_ar1)<-c("BSM_error")
colnames(parametros_ar1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ar1<-rbind(2*(ar1_mg$fit$value)+2*5)
colnames(AIC_ar1)<-"AIC"

BIC_ar1<-2*(ar1_mg$fit$value)+2*5*log(ar1_mg$T)

# Matriz Hessiana

all(eigen(ar1_mg$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ar1<-list(ar1_mg)
testes_ar1<-sapply(lista_ar1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ar1<-t(testes_ar1)
row.names(testes_ar1)<-c("BSM_error")
colnames(testes_ar1)<-c("Shapiro","Box","H")
resultadosmg_ar1<-cbind(conver_ar1, parametros_ar1, testes_ar1, AIC_ar1, BIC_ar1)
resultadosmg_ar1

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ar1_mg$ts.original, start = 2012, frequency = 4),
  ts(ar1_mg$ts.signal, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1, plot.type = "single", col = c(1, 4), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
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
mtext("09 - Minas Gerais (AR(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## GRÁFICO DE ANÁLISE AR(1)

figtend_ar1<-window(ts.union(ts(ar1_mg$ts.original, start = 2012, frequency = 4),ts(ar1_mg$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_ar1<-window(ts.union(ts(ar1_mg$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_ar1<-window(ts.union(ts(ar1_mg$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_ar1<-window(ts.union(ts(ar1_mg$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

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
mtext("09 - Minas Gerais", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["09 - Minas Gerais"]][["sinal_smooth_ar1mg"]]<-ar1_mg$ts.signal
result_mods_deso[["09 - Minas Gerais"]][["cv_sinal_smooth_ar1mg"]]<-ar1_mg$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)



#### MODELO MA(1)

source("data/funcoes/08_smooth_MA1.R")
theta1_ma1 <- dbmg[["mod_ma1"]][["theta1_ma1_dmg"]]

# Estimação do Modelo

numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
save.image("partial.Rdata")
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

start_time <- Sys.time()
run_ma1mg<-parLapply(cl,1:nrow(grid_error), function(i) tryCatch(f.smooth_ma1(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

stopCluster(cl)
showConnections()

# Avaliação das iterações:

ini_ma1_mg <- cbind(
  round(exp(grid_error), 4),
  do.call(rbind, lapply(1:nrow(grid_error), function(i) {
    tryCatch({
      params <- round(exp(run_ma1mg[[i]][["fit"]][["par"]]), 4)
      convergence <- run_ma1mg[[i]][["fit"]][["convergence"]]
      log_like <- run_ma1mg[[i]][["fit"]][["value"]]
      c(params, convergence, log_like)
    }, error = function(e) rep(NA, 6))
  }))
)

colnames(ini_ma1_mg) <- c("slope_ini","seasonal_ini","irregular_ini","sampl_error_ini",
                          "slope","seasonal","irregular", "sampl_error",
                          "convergence","log_like")

## Após iniciais, seleção do modelo:

ma1_mg <- run_ma1mg[[which(
  ini_ma1_mg$log_like == min(ini_ma1_mg$log_like[ini_ma1_mg$convergence == 0], na.rm = TRUE) &
    ini_ma1_mg$convergence == 0
)]]

# Verificando a convergência

conver_ma1<-rbind(ma1_mg$fit$convergence)
colnames(conver_ma1)<-c("convergence")

# Parâmetros estimados:

parametros_ma1<-rbind(c(round(exp(ma1_mg$fit$par),4)))
row.names(parametros_ma1)<-c("BSM_error")
colnames(parametros_ma1)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC_ma1<-rbind(2*(ma1_mg$fit$value)+2*5)
colnames(AIC_ma1)<-"AIC"

BIC_ma1<-2*(ma1_mg$fit$value)+2*5*log(ma1_mg$T)

# Matriz Hessiana

all(eigen(ma1_mg$fit$hessian, only.values = TRUE)$values > 0) # false

# Diagnosticando os resíduos

lista_ma1<-list(ma1_mg)
testes_ma1<-sapply(lista_ma1, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_ma1<-t(testes_ma1)
row.names(testes_ma1)<-c("BSM_error")
colnames(testes_ma1)<-c("Shapiro","Box","H")
resultadosmg_ma1<-cbind(conver_ma1, parametros_ma1, testes_ma1, AIC_ma1, BIC_ma1)
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
mtext("09 - Minas Gerais (MA(1))", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

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

plot(figsample_ar1, plot.type = "single", col = c(4), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(4), bty = 'n', lwd = c(2))
mtext("Desocupados", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("09 - Minas Gerais", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

result_mods_deso<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
result_mods_deso[["09 - Minas Gerais"]][["sinal_smooth_ma1mg"]]<-ma1_mg$ts.signal
result_mods_deso[["09 - Minas Gerais"]][["cv_sinal_smooth_ma1mg"]]<-ma1_mg$cv.signal
saveRDS(result_mods_deso, file = "D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
rm(result_mods_deso)


#### GRÁFICO UNIFICADO

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_mg <- window(ts.union(
  ts(ma1_mg$ts.original, start = 2012, frequency = 4),
  ts(ar1_mg$ts.signal, start = 2012, frequency = 4),
  ts(ma1_mg$ts.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_mg, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da desocupação AR(1): model-based",
                            "Sinal da Desocupação MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_mg.cv <- window(ts.union(
  ts((ma1_mg$cv.original) * 100, start = 2012, frequency = 4),
  ts(ar1_mg$cv.signal, start = 2012, frequency = 4),
  ts(ma1_mg$cv.signal, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_mg.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados AR(1): model-based",
                             "Sinal CV desocupados MA(1): model-based"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("09 - Minas Gerais (AR e MA)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Salvando o .Rdata

save.image(file = "D:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/09_mod_mg.Rdata")


