################################################################################
##                          MODELO BSM UNIVARIADO                             ##
################################################################################

library(dlm)
library(tidyverse)
library(beepr)
library(parallel)
library(rucm)

# Observação do pacote ucm:
  # Para rodar o modelo, é necessário transformar os objetos em ts
    # Caso contrário, o modelo irá reportar erro na dimensão dos objetos

options(scipen=999)

## Anotações:
  # Três modelos serão feitos para cada região:
    # dlm sem erro amostral
    # ucm smoothing
    # dlm com erro amostral
  
### MODELO BH ##################################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/06_teste_bsm.R")
source("data/funcoes/07_teste_bsm_error.R")

## Dados:

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
bh<-baseestr0324$`01-Belo Horizonte`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtbh<-baseal0324$`01-Belo Horizonte` ## Arquivo "cru", saída direta da rotina da base por rotação
dbbh<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/01_params_bh.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

y <- bh$Total.de.desocupados/1000
se_db <- bh$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbbh$parerro_d

# Modelo DLM sem erro amostral:
  # Parâmetros tese

par_1<-c(log(22.42))
par_2<-c(log(0.001)) 
par_3<-c(log(41.25)) 

## Input dos parâmetros iniciais do modelo

grid_dlm <- expand.grid(par_1,par_2,par_3)

### Estimação do Modelo

modelo_dlm_bsm_ini<-lapply(1:nrow(grid_dlm), function(i)  tryCatch(f.teste_bsm(y,grid_dlm[i,]),error=function(e) {rep(NA,4)}))

modelo_dlm_bsm<-modelo_dlm_bsm_ini[[1]]

## Verificando a convergência
  
convergencia_dlm<-rbind(modelo_dlm_bsm$fit$convergence)
colnames(convergencia_dlm)<-c("convergence") 

## Parâmetros estimados:

parametros_dlm<-rbind(c(round(exp(modelo_dlm_bsm$fit$par),4)))
row.names(parametros_dlm)<-c("BSM_DLM")
colnames(parametros_dlm)<-c("Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AICdlm<-rbind(2*(modelo_dlm_bsm$fit$value)+2*5)
colnames(AICdlm)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_dlm_bsm$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos_dlm<-list(modelo_dlm_bsm)
testes_dlm<-sapply(lista_modelos_dlm, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_dlm<-t(testes_dlm)
row.names(testes_dlm)<-c("BSM_DLM")
colnames(testes_dlm)<-c("Shapiro","Box","H")

resultados_dlm<-cbind(convergencia_dlm,parametros_dlm,testes_dlm, AICdlm)
resultados_dlm

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_dlm_bsm$ts.original,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_dlm_bsm$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)


### Modelo UCM

# Transformando os dados em série temporal:
  # Também é necessário diminuir a escala dos dados, senão a matriz de variância não é estimada

y_red<-ts(y, start = c(2012, 1), frequency = 4)
ep_db<-ts(se_db, start = c(2012, 1), frequency = 4)
cv_db<-ts(cv_db,start=c(2012,1),frequency = 4)

# Modelo com suavização

cnobh_sm<-ucm(y_red~0,data=y_red,level=TRUE, level.var = 0, slope = TRUE, season = TRUE, season.length = 4, cycle = FALSE, irregular = TRUE)

bhtrend_sm<-(cnobh_sm$s.level)+(cnobh_sm$s.slope)
bhtrend_sm<-(bhtrend_sm)*1000

ts.plot((y*1000), bhtrend_sm, col = c("black", "red"),lty = c(1, 2),lwd = 2, main = "01-Belo Horizonte",
        ylab = "Total de Desocupados",xlab = "Tempo")
legend("bottom", legend = c("Total de desocupados", "Tendência Estimada Smooth"), col = c("black", "red"), lty = c(1, 2), lwd = 2)

  # Resgatando as variâncias:

cnobh_sm$est.var.level #nula por causa do modelo smoothing
cnobh_sm$est.var.slope # 26.15096
cnobh_sm$est.var.season # 0.0001269569 
cnobh_sm$irr.var # 75.98691


### Modelo DLM com erro amostral

# Parâmetros iniciais:

par_1<-c(log(22.42))
par_2<-c(log(0.001)) 
par_3<-c(log(41.25))
par_4<-c(log(0.40))

# Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Estimação do Modelo

modelo_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

modelo_bsm_error<-modelo_bsm_error_ini[[1]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes,AIC)
resultados

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_bsm_error$ts.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_bsm_error$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                              "CV desocupados: model-based",
                              "Tendência CV desocupados (model-based)"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### ENTORNO METROPOLITANO ###############################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/06_teste_bsm.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para o Entorno

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
ent<-baseestr0324$`02-Entorno metropolitano de BH`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtent<-baseal0324$`02-Entorno metropolitano de BH` ## Arquivo "cru", saída direta da rotina da base por rotação
dbent<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/02_params_ent.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- (ent$Total.de.desocupados)/1000
se_db <- (ent$sd_d)/1000
cv_db <- se_db/y
par_ar_erro <- dbent$parerro_d

# Modelo DLM sem erro amostral:
# Parâmetros tese

par_1<-c(log(115.07))
par_2<-c(log(0.001)) 
par_3<-c(log(69.04)) 

## Input dos parâmetros iniciais do modelo

grid_dlm <- expand.grid(par_1,par_2,par_3)

### Estimação do Modelo

modelo_dlm_bsm_ini<-lapply(1:nrow(grid_dlm), function(i)  tryCatch(f.teste_bsm(y,grid_dlm[i,]),error=function(e) {rep(NA,4)}))

modelo_dlm_bsm<-modelo_dlm_bsm_ini[[1]]

## Verificando a convergência

convergencia_dlm<-rbind(modelo_dlm_bsm$fit$convergence)
colnames(convergencia_dlm)<-c("convergence") 

## Parâmetros estimados:

parametros_dlm<-rbind(c(round(exp(modelo_dlm_bsm$fit$par),4)))
row.names(parametros_dlm)<-c("BSM_DLM")
colnames(parametros_dlm)<-c("Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AICdlm<-rbind(2*(modelo_dlm_bsm$fit$value)+2*5)
colnames(AICdlm)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_dlm_bsm$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos_dlm<-list(modelo_dlm_bsm)
testes_dlm<-sapply(lista_modelos_dlm, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_dlm<-t(testes_dlm)
row.names(testes_dlm)<-c("BSM_DLM")
colnames(testes_dlm)<-c("Shapiro","Box","H")

resultados_dlm<-cbind(convergencia_dlm,parametros_dlm,testes_dlm, AICdlm)
resultados_dlm

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_dlm_bsm$ts.original,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_dlm_bsm$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)


### Teste modelo UCM ENT:
  # Utilizando modelo sm com a série escalonada

y_red<-ts(y, start = c(2012, 1), frequency = 4)
ep_db<-ts(se_db, start = c(2012, 1), frequency = 4)
cv_db<-ts(cv_db,start=c(2012,1),frequency = 4)

cnoent_sm<-ucm(y_red~0,data=y_red,level=TRUE, level.var = 0, slope = TRUE, season = TRUE, season.length = 4, cycle = FALSE, irregular = TRUE)

enttrend_sm<-(cnoent_sm$s.level)+(cnoent_sm$s.slope)
enttrend_sm<-(enttrend_sm)*1000

ts.plot((y*1000), enttrend_sm, col = c("black", "red"),lty = c(1, 2),lwd = 2, main = "02-Entorno Metropolitano de Belo Horizonte",
        ylab = "Total de Desocupados",xlab = "Tempo")
legend("topleft", legend = c("Total de Desocupados", "Tendência Estimada Smooth"), col = c("black", "red"), lty = c(1, 2), lwd = 2)

# Resgatando as variâncias:

cnoent_sm$est.var.level # nula por causa do modelo smoothing
cnoent_sm$est.var.slope # 106.7323
cnoent_sm$est.var.season # 0.00348154  
cnoent_sm$irr.var # 106.8907


# Modelo DLM com sample error

# Parâmetros iniciais:

par_1<-c(log(115.07))
par_2<-c(log(0.001)) 
par_3<-c(log(69.04)) 
par_4<-c(log(0.11))

# Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Estimação do Modelo

modelo_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

modelo_bsm_error<-modelo_bsm_error_ini[[1]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes,AIC)
resultados

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_bsm_error$ts.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_bsm_error$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "CV desocupados: model-based",
                             "Tendência CV desocupados (model-based)"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### COLAR METROPOLITANO DE BH ##################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/06_teste_bsm.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para o Colar BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
col<-baseestr0324$`03-Colar metropolitano de BH`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtcol<-baseal0324$`03-Colar metropolitano de BH` ## Arquivo "cru", saída direta da rotina da base por rotação
dbcol<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/03_params_col.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- col$Total.de.desocupados/1000
se_db <- col$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbcol$parerro_d

# Modelo DLM sem erro amostral:
# Parâmetros tese

par_1<-c(log(2.97))
par_2<-c(log(0.001)) 
par_3<-c(log(0.001)) 

## Input dos parâmetros iniciais do modelo

grid_dlm <- expand.grid(par_1,par_2,par_3)

### Estimação do Modelo

modelo_dlm_bsm_ini<-lapply(1:nrow(grid_dlm), function(i)  tryCatch(f.teste_bsm(y,grid_dlm[i,]),error=function(e) {rep(NA,4)}))

modelo_dlm_bsm<-modelo_dlm_bsm_ini[[1]]

## Verificando a convergência

convergencia_dlm<-rbind(modelo_dlm_bsm$fit$convergence)
colnames(convergencia_dlm)<-c("convergence") 

## Parâmetros estimados:

parametros_dlm<-rbind(c(round(exp(modelo_dlm_bsm$fit$par),4)))
row.names(parametros_dlm)<-c("BSM_DLM")
colnames(parametros_dlm)<-c("Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AICdlm<-rbind(2*(modelo_dlm_bsm$fit$value)+2*5)
colnames(AICdlm)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_dlm_bsm$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos_dlm<-list(modelo_dlm_bsm)
testes_dlm<-sapply(lista_modelos_dlm, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_dlm<-t(testes_dlm)
row.names(testes_dlm)<-c("BSM_DLM")
colnames(testes_dlm)<-c("Shapiro","Box","H")

resultados_dlm<-cbind(convergencia_dlm,parametros_dlm,testes_dlm, AICdlm)
resultados_dlm

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_dlm_bsm$ts.original,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_dlm_bsm$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)


### Teste modelo UCM COL:
# Utilizando modelo sm com a série escalonada

y_red<-ts(y, start = c(2012, 1), frequency = 4)
ep_db<-ts(se_db, start = c(2012, 1), frequency = 4)
cv_db<-ts(cv_db,start=c(2012,1),frequency = 4)

cnocol_sm<-ucm(y_red~0,data=y_red,level=TRUE, level.var = 0, slope = TRUE, season = TRUE, season.length = 4, cycle = FALSE, irregular = TRUE)

coltrend_sm<-(cnocol_sm$s.level)+(cnocol_sm$s.slope)
coltrend_sm<-(coltrend_sm)*1000

ts.plot((y*1000), coltrend_sm, col = c("black", "red"),lty = c(1, 2),lwd = 2, main = "03-Colar Metropolitano de Belo Horizonte",
        ylab = "Total de Desocupados",xlab = "Tempo")
legend("bottom", legend = c("Total de Desocupados", "Tendência Estimada Smooth"), col = c("black", "red"), lty = c(1, 2), lwd = 2)

# Resgatando as variâncias:

cnocol_sm$est.var.level # nula por causa do modelo smoothing
cnocol_sm$est.var.slope # 3.830586
cnocol_sm$est.var.season # 0.00000616888  
cnocol_sm$irr.var # 10.51971


# Modelo DLM com sample error

# Parâmetros iniciais:

par_1<-c(log(2.97))
par_2<-c(log(0.001)) 
par_3<-c(log(0.001)) 
par_4<-c(log(0.49))

# Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Estimação do Modelo

modelo_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

modelo_bsm_error<-modelo_bsm_error_ini[[1]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes,AIC)
resultados

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_bsm_error$ts.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_bsm_error$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "CV desocupados: model-based",
                             "Tendência CV desocupados (model-based)"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### RIDE de Brasília em Minas ##################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/06_teste_bsm.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para RIDE

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
rid<-baseestr0324$`04-RIDE de Brasília em Minas`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtrid<-baseal0324$`04-RIDE de Brasília em Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbrid<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/04_params_rid.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- rid$Total.de.desocupados/1000
se_db <- rid$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbrid$parerro_d

# Modelo DLM sem erro amostral:
# Parâmetros tese

par_1<-c(log(0.001))
par_2<-c(log(0.001)) 
par_3<-c(log(0.94)) 

## Input dos parâmetros iniciais do modelo

grid_dlm <- expand.grid(par_1,par_2,par_3)

### Estimação do Modelo

modelo_dlm_bsm_ini<-lapply(1:nrow(grid_dlm), function(i)  tryCatch(f.teste_bsm(y,grid_dlm[i,]),error=function(e) {rep(NA,4)}))

modelo_dlm_bsm<-modelo_dlm_bsm_ini[[1]]

## Verificando a convergência

convergencia_dlm<-rbind(modelo_dlm_bsm$fit$convergence)
colnames(convergencia_dlm)<-c("convergence") 

## Parâmetros estimados:

parametros_dlm<-rbind(c(round(exp(modelo_dlm_bsm$fit$par),4)))
row.names(parametros_dlm)<-c("BSM_DLM")
colnames(parametros_dlm)<-c("Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AICdlm<-rbind(2*(modelo_dlm_bsm$fit$value)+2*5)
colnames(AICdlm)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_dlm_bsm$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos_dlm<-list(modelo_dlm_bsm)
testes_dlm<-sapply(lista_modelos_dlm, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_dlm<-t(testes_dlm)
row.names(testes_dlm)<-c("BSM_DLM")
colnames(testes_dlm)<-c("Shapiro","Box","H")

resultados_dlm<-cbind(convergencia_dlm,parametros_dlm,testes_dlm, AICdlm)
resultados_dlm

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_dlm_bsm$ts.original,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                            "Signal model-based unemployment",
                            "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_dlm_bsm$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV design-based unemployment",
                             "CV signal model-based unemployment",
                             "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### Teste modelo UCM RIDE:
# Utilizando modelo sm com a série escalonada

y_red<-ts(y, start = c(2012, 1), frequency = 4)
ep_db<-ts(se_db, start = c(2012, 1), frequency = 4)
cv_db<-ts(cv_db,start=c(2012,1),frequency = 4)

cnorid_sm<-ucm(y_red~0,data=y_red,level=TRUE, level.var = 0, slope = TRUE, season = TRUE, season.length = 4, cycle = FALSE, irregular = TRUE)

ridtrend_sm<-(cnorid_sm$s.level)+(cnorid_sm$s.slope)
ridtrend_sm<-(ridtrend_sm)*1000

ts.plot((y*1000), ridtrend_sm, col = c("black", "red"),lty = c(1, 2),lwd = 2, main = "04-RIDE de Brasília em Minas",
        ylab = "Total de Desocupados",xlab = "Tempo")
legend("bottom", legend = c("Total de Desocupados", "Tendência Estimada Smooth"), col = c("black", "red"), lty = c(1, 2), lwd = 2)

# Resgatando as variâncias:

cnorid_sm$est.var.level # nula por causa do modelo smoothing
cnorid_sm$est.var.slope # 0.03001628
cnorid_sm$est.var.season # 0.004274386   
cnorid_sm$irr.var # 1.105705

# Modelo DLM com sample error

# Parâmetros iniciais:

par_1<-c(log(0.001))
par_2<-c(log(0.001)) 
par_3<-c(log(0.94)) 
par_4<-c(log(0.44))

# Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Estimação do Modelo

modelo_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

modelo_bsm_error<-modelo_bsm_error_ini[[1]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes,AIC)
resultados

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_bsm_error$ts.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_bsm_error$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "CV desocupados: model-based",
                             "Tendência CV desocupados (model-based)"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### SUL DE MINAS ###############################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/06_teste_bsm.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para o Sul

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
sul<-baseestr0324$`05-Sul de Minas`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtsul<-baseal0324$`05-Sul de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbsul<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/05_params_sul.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- sul$Total.de.desocupados/1000
se_db <- sul$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbsul$parerro_d

# Modelo DLM sem erro amostral:
# Parâmetros tese

par_1<-c(log(13.88))
par_2<-c(log(0.001)) 
par_3<-c(log(0.001)) 

## Input dos parâmetros iniciais do modelo

grid_dlm <- expand.grid(par_1,par_2,par_3)

### Estimação do Modelo

modelo_dlm_bsm_ini<-lapply(1:nrow(grid_dlm), function(i)  tryCatch(f.teste_bsm(y,grid_dlm[i,]),error=function(e) {rep(NA,4)}))

modelo_dlm_bsm<-modelo_dlm_bsm_ini[[1]]

## Verificando a convergência

convergencia_dlm<-rbind(modelo_dlm_bsm$fit$convergence)
colnames(convergencia_dlm)<-c("convergence") 

## Parâmetros estimados:

parametros_dlm<-rbind(c(round(exp(modelo_dlm_bsm$fit$par),4)))
row.names(parametros_dlm)<-c("BSM_DLM")
colnames(parametros_dlm)<-c("Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AICdlm<-rbind(2*(modelo_dlm_bsm$fit$value)+2*5)
colnames(AICdlm)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_dlm_bsm$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos_dlm<-list(modelo_dlm_bsm)
testes_dlm<-sapply(lista_modelos_dlm, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_dlm<-t(testes_dlm)
row.names(testes_dlm)<-c("BSM_DLM")
colnames(testes_dlm)<-c("Shapiro","Box","H")

resultados_dlm<-cbind(convergencia_dlm,parametros_dlm,testes_dlm, AICdlm)
resultados_dlm

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_dlm_bsm$ts.original,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_dlm_bsm$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV design-based unemployment",
                             "CV signal model-based unemployment",
                             "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### Teste modelo UCM SUL:
# Utilizando modelo sm com a série escalonada

y_red<-ts(y, start = c(2012, 1), frequency = 4)
ep_db<-ts(se_db, start = c(2012, 1), frequency = 4)
cv_db<-ts(cv_db,start=c(2012,1),frequency = 4)

cnosul_sm<-ucm(y_red~0,data=y_red,level=TRUE, level.var = 0, slope = TRUE, season = TRUE, season.length = 4, cycle = FALSE, irregular = TRUE)

sultrend_sm<-(cnosul_sm$s.level)+(cnosul_sm$s.slope)
sultrend_sm<-(sultrend_sm)*1000

ts.plot((y)*1000, sultrend_sm, col = c("black", "red"),lty = c(1, 2),lwd = 2, main = "05-Sul de Minas",
        ylab = "Total de Desocupados",xlab = "Tempo")
legend("bottom", legend = c("Total de Desocupados", "Tendência Estimada Smooth"), col = c("black", "red"), lty = c(1, 2), lwd = 2)

# Resgatando as variâncias:

cnosul_sm$est.var.level # nula por causa do modelo smoothing
cnosul_sm$est.var.slope # 56.85534
cnosul_sm$est.var.season # 0.0008044187   
cnosul_sm$irr.var # 90.04413

# Modelo DLM com sample error

# Parâmetros iniciais:

par_1<-c(log(13.88))
par_2<-c(log(0.001)) 
par_3<-c(log(0.001)) 
par_4<-c(log(1.03))

# Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Estimação do Modelo

modelo_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

modelo_bsm_error<-modelo_bsm_error_ini[[1]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes,AIC)
resultados

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_bsm_error$ts.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_bsm_error$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "CV desocupados: model-based",
                             "Tendência CV desocupados (model-based)"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### TRIÂNGULO MINEIRO ##########################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/06_teste_bsm.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para Triângulo Mineiro

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
trg<-baseestr0324$`06-Triângulo Mineiro`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dttrg<-baseal0324$`06-Triângulo Mineiro` ## Arquivo "cru", saída direta da rotina da base por rotação
dbtrg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/06_params_trg.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- trg$Total.de.desocupados/1000
se_db <- trg$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbtrg$parerro_d

# Modelo DLM sem erro amostral:
# Parâmetros tese

par_1<-c(log(0.001))
par_2<-c(log(0.001)) 
par_3<-c(log(0.001)) 

## Input dos parâmetros iniciais do modelo

grid_dlm <- expand.grid(par_1,par_2,par_3)

### Estimação do Modelo

modelo_dlm_bsm_ini<-lapply(1:nrow(grid_dlm), function(i)  tryCatch(f.teste_bsm(y,grid_dlm[i,]),error=function(e) {rep(NA,4)}))

modelo_dlm_bsm<-modelo_dlm_bsm_ini[[1]]

## Verificando a convergência

convergencia_dlm<-rbind(modelo_dlm_bsm$fit$convergence)
colnames(convergencia_dlm)<-c("convergence") 

## Parâmetros estimados:

parametros_dlm<-rbind(c(round(exp(modelo_dlm_bsm$fit$par),4)))
row.names(parametros_dlm)<-c("BSM_DLM")
colnames(parametros_dlm)<-c("Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AICdlm<-rbind(2*(modelo_dlm_bsm$fit$value)+2*5)
colnames(AICdlm)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_dlm_bsm$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos_dlm<-list(modelo_dlm_bsm)
testes_dlm<-sapply(lista_modelos_dlm, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_dlm<-t(testes_dlm)
row.names(testes_dlm)<-c("BSM_DLM")
colnames(testes_dlm)<-c("Shapiro","Box","H")

resultados_dlm<-cbind(convergencia_dlm,parametros_dlm,testes_dlm, AICdlm)
resultados_dlm

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_dlm_bsm$ts.original,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_dlm_bsm$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV design-based unemployment",
                             "CV signal model-based unemployment",
                             "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### Teste modelo UCM TRG:
# Utilizando modelo sm com a série escalonada

y_red<-ts(y, start = c(2012, 1), frequency = 4)
ep_db<-ts(se_db, start = c(2012, 1), frequency = 4)
cv_db<-ts(cv_db,start=c(2012,1),frequency = 4)

cnotrg_sm<-ucm(y_red~0,data=y_red,level=TRUE, level.var = 0, slope = TRUE, season = TRUE, season.length = 4, cycle = FALSE, irregular = TRUE)

trgtrend_sm<-(cnotrg_sm$s.level)+(cnotrg_sm$s.slope)
trgtrend_sm<-(trgtrend_sm)*1000

ts.plot((y*1000), trgtrend_sm, col = c("black", "red"),lty = c(1, 2),lwd = 2, main = "06-Triângulo Mineiro",
        ylab = "Total de Desocupados",xlab = "Tempo")
legend("bottom", legend = c("Total de Desocupados", "Tendência Estimada Smooth"), col = c("black", "red"), lty = c(1, 2), lwd = 2)

# Resgatando as variâncias:

cnotrg_sm$est.var.level # nula por causa do modelo smoothing
cnotrg_sm$est.var.slope # 11.70026 
cnotrg_sm$est.var.season # 0.00006308897   
cnotrg_sm$irr.var # 84.38331

# Modelo DLM com sample error

# Parâmetros iniciais:

par_1<-c(log(0.001))
par_2<-c(log(0.001)) 
par_3<-c(log(0.001)) 
par_4<-c(log(1.53))

# Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Estimação do Modelo

modelo_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

modelo_bsm_error<-modelo_bsm_error_ini[[1]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes,AIC)
resultados

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_bsm_error$ts.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_bsm_error$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "CV desocupados: model-based",
                             "Tendência CV desocupados (model-based)"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### ZONA DA MATA ###############################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/06_teste_bsm.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para a Zona da Mata

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
mat<-baseestr0324$`07-Mata de Minas Gerais`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtmat<-baseal0324$`07-Mata de Minas Gerais` ## Arquivo "cru", saída direta da rotina da base por rotação
dbmat<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/07_params_mat.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- mat$Total.de.desocupados/1000
se_db <- mat$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbmat$parerro_d

# Modelo DLM sem erro amostral:
# Parâmetros tese

par_1<-c(log(9.72))
par_2<-c(log(0.001)) 
par_3<-c(log(0.001)) 

## Input dos parâmetros iniciais do modelo

grid_dlm <- expand.grid(par_1,par_2,par_3)

### Estimação do Modelo

modelo_dlm_bsm_ini<-lapply(1:nrow(grid_dlm), function(i)  tryCatch(f.teste_bsm(y,grid_dlm[i,]),error=function(e) {rep(NA,4)}))

modelo_dlm_bsm<-modelo_dlm_bsm_ini[[1]]

## Verificando a convergência

convergencia_dlm<-rbind(modelo_dlm_bsm$fit$convergence)
colnames(convergencia_dlm)<-c("convergence") 

## Parâmetros estimados:

parametros_dlm<-rbind(c(round(exp(modelo_dlm_bsm$fit$par),4)))
row.names(parametros_dlm)<-c("BSM_DLM")
colnames(parametros_dlm)<-c("Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AICdlm<-rbind(2*(modelo_dlm_bsm$fit$value)+2*5)
colnames(AICdlm)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_dlm_bsm$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos_dlm<-list(modelo_dlm_bsm)
testes_dlm<-sapply(lista_modelos_dlm, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_dlm<-t(testes_dlm)
row.names(testes_dlm)<-c("BSM_DLM")
colnames(testes_dlm)<-c("Shapiro","Box","H")

resultados_dlm<-cbind(convergencia_dlm,parametros_dlm,testes_dlm, AICdlm)
resultados_dlm

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_dlm_bsm$ts.original,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                            "Signal model-based unemployment",
                            "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_dlm_bsm$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV design-based unemployment",
                             "CV signal model-based unemployment",
                             "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### Teste modelo UCM ZONA DA MATA:
# Utilizando modelo sm com a série escalonada

y_red<-ts(y, start = c(2012, 1), frequency = 4)
ep_db<-ts(se_db, start = c(2012, 1), frequency = 4)
cv_db<-ts(cv_db,start=c(2012,1),frequency = 4)

cnomat_sm<-ucm(y_red~0,data=y_red,level=TRUE, level.var = 0, slope = TRUE, season = TRUE, season.length = 4, cycle = FALSE, irregular = TRUE)

mattrend_sm<-(cnomat_sm$s.level)+(cnomat_sm$s.slope)
mattrend_sm<-(mattrend_sm)*1000

ts.plot((y*1000), mattrend_sm, col = c("black", "red"),lty = c(1, 2),lwd = 2, main = "07- Zona da Mata",
        ylab = "Total de Desocupados",xlab = "Tempo")
legend("bottom", legend = c("Total de Desocupados", "Tendência Estimada Smooth"), col = c("black", "red"), lty = c(1, 2), lwd = 2)

# Resgatando as variâncias:

cnomat_sm$est.var.level # nula por causa do modelo smoothing
cnomat_sm$est.var.slope # 16.0373  
cnomat_sm$est.var.season # 0.0004886218   
cnomat_sm$irr.var # 82.69742

# Modelo DLM com sample error

# Parâmetros iniciais:

par_1<-c(log(9.72))
par_2<-c(log(0.001)) 
par_3<-c(log(0.001)) 
par_4<-c(log(0.94999)) # Ao colocar exatamente 0.95 a matriz V se torna singular

# Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Estimação do Modelo

modelo_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

modelo_bsm_error<-modelo_bsm_error_ini[[1]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes,AIC)
resultados

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_bsm_error$ts.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_bsm_error$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "CV desocupados: model-based",
                             "Tendência CV desocupados (model-based)"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### NORTE DE MINAS GERAIS ######################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/06_teste_bsm.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para o Norte

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
nrt<-baseestr0324$`08-Norte de Minas`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtnrt<-baseal0324$`08-Norte de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbnrt<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/08_params_nrt.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- nrt$Total.de.desocupados/1000
se_db <- nrt$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbnrt$parerro_d

# Modelo DLM sem erro amostral:
# Parâmetros tese

par_1<-c(log(16.63))
par_2<-c(log(0.001)) 
par_3<-c(log(50.97)) 

## Input dos parâmetros iniciais do modelo

grid_dlm <- expand.grid(par_1,par_2,par_3)

### Estimação do Modelo

modelo_dlm_bsm_ini<-lapply(1:nrow(grid_dlm), function(i)  tryCatch(f.teste_bsm(y,grid_dlm[i,]),error=function(e) {rep(NA,4)}))

modelo_dlm_bsm<-modelo_dlm_bsm_ini[[1]]

## Verificando a convergência

convergencia_dlm<-rbind(modelo_dlm_bsm$fit$convergence)
colnames(convergencia_dlm)<-c("convergence") 

## Parâmetros estimados:

parametros_dlm<-rbind(c(round(exp(modelo_dlm_bsm$fit$par),4)))
row.names(parametros_dlm)<-c("BSM_DLM")
colnames(parametros_dlm)<-c("Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AICdlm<-rbind(2*(modelo_dlm_bsm$fit$value)+2*5)
colnames(AICdlm)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_dlm_bsm$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos_dlm<-list(modelo_dlm_bsm)
testes_dlm<-sapply(lista_modelos_dlm, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_dlm<-t(testes_dlm)
row.names(testes_dlm)<-c("BSM_DLM")
colnames(testes_dlm)<-c("Shapiro","Box","H")

resultados_dlm<-cbind(convergencia_dlm,parametros_dlm,testes_dlm, AICdlm)
resultados_dlm

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_dlm_bsm$ts.original,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                            "Signal model-based unemployment",
                            "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_dlm_bsm$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV design-based unemployment",
                             "CV signal model-based unemployment",
                             "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### Teste modelo UCM NORTE:
# Utilizando modelo sm com a série escalonada

y_red<-ts(y, start = c(2012, 1), frequency = 4)
ep_db<-ts(se_db, start = c(2012, 1), frequency = 4)
cv_db<-ts(cv_db,start=c(2012,1),frequency = 4)

cnonrt_sm<-ucm(y_red~0,data=y_red,level=TRUE, level.var = 0, slope = TRUE, season = TRUE, season.length = 4, cycle = FALSE, irregular = TRUE)

nrttrend_sm<-(cnonrt_sm$s.level)+(cnonrt_sm$s.slope)
nrttrend_sm<-(nrttrend_sm)*1000

ts.plot((y*1000), nrttrend_sm, col = c("black", "red"),lty = c(1, 2),lwd = 2, main = "08-Norte de Minas",
        ylab = "Total de Desocupados",xlab = "Tempo")
legend("bottom", legend = c("Total de Desocupados", "Tendência Estimada Smooth"), col = c("black", "red"), lty = c(1, 2), lwd = 2)

# Resgatando as variâncias:

cnonrt_sm$est.var.level # nula por causa do modelo smoothing
cnonrt_sm$est.var.slope # 43.22508   
cnonrt_sm$est.var.season # 0.0009056865    
cnonrt_sm$irr.var # 142.2077

# Modelo DLM com sample error

# Parâmetros iniciais:

par_1<-c(log(16.63))
par_2<-c(log(0.001)) 
par_3<-c(log(50.97)) 
par_4<-c(log(0.83))

# Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Estimação do Modelo

modelo_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

modelo_bsm_error<-modelo_bsm_error_ini[[1]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes,AIC)
resultados

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_bsm_error$ts.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_bsm_error$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "CV desocupados: model-based",
                             "Tendência CV desocupados (model-based)"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### VALE DO RIO DOCE ###########################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/06_teste_bsm.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para o vale

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
vl<-baseestr0324$`09-Vale do Rio Doce`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtvl<-baseal0324$`09-Vale do Rio Doce` ## Arquivo "cru", saída direta da rotina da base por rotação
dbvl<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/09_params_rio.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- vl$Total.de.desocupados/1000
se_db <- vl$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbvl$parerro_d

# Modelo DLM sem erro amostral:
# Parâmetros tese

par_1<-c(log(27.04))
par_2<-c(log(0.001)) 
par_3<-c(log(25.01)) 

## Input dos parâmetros iniciais do modelo

grid_dlm <- expand.grid(par_1,par_2,par_3)

### Estimação do Modelo

modelo_dlm_bsm_ini<-lapply(1:nrow(grid_dlm), function(i)  tryCatch(f.teste_bsm(y,grid_dlm[i,]),error=function(e) {rep(NA,4)}))

modelo_dlm_bsm<-modelo_dlm_bsm_ini[[1]]

## Verificando a convergência

convergencia_dlm<-rbind(modelo_dlm_bsm$fit$convergence)
colnames(convergencia_dlm)<-c("convergence") 

## Parâmetros estimados:

parametros_dlm<-rbind(c(round(exp(modelo_dlm_bsm$fit$par),4)))
row.names(parametros_dlm)<-c("BSM_DLM")
colnames(parametros_dlm)<-c("Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AICdlm<-rbind(2*(modelo_dlm_bsm$fit$value)+2*5)
colnames(AICdlm)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_dlm_bsm$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos_dlm<-list(modelo_dlm_bsm)
testes_dlm<-sapply(lista_modelos_dlm, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_dlm<-t(testes_dlm)
row.names(testes_dlm)<-c("BSM_DLM")
colnames(testes_dlm)<-c("Shapiro","Box","H")

resultados_dlm<-cbind(convergencia_dlm,parametros_dlm,testes_dlm, AICdlm)
resultados_dlm

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_dlm_bsm$ts.original,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                            "Signal model-based unemployment",
                            "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_dlm_bsm$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV design-based unemployment",
                             "CV signal model-based unemployment",
                             "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### Teste modelo UCM VALE:
# Utilizando modelo sm com a série escalonada

y_red<-ts(y, start = c(2012, 1), frequency = 4)
ep_db<-ts(se_db, start = c(2012, 1), frequency = 4)
cv_db<-ts(cv_db,start=c(2012,1),frequency = 4)

cnovl_sm<-ucm(y_red~0,data=y_red,level=TRUE, level.var = 0, slope = TRUE, season = TRUE, season.length = 4, cycle = FALSE, irregular = TRUE)

vltrend_sm<-(cnovl_sm$s.level)+(cnovl_sm$s.slope)
vltrend_sm<-(vltrend_sm)*1000

ts.plot((y*1000), vltrend_sm, col = c("black", "red"),lty = c(1, 2),lwd = 2, main = "09-Vale do Rio Doce",
        ylab = "Total de Desocupados",xlab = "Tempo")
legend("bottom", legend = c("Total de Desocupados", "Tendência Estimada Smooth"), col = c("black", "red"), lty = c(1, 2), lwd = 2)

# Resgatando as variâncias:

cnovl_sm$est.var.level # nula por causa do modelo smoothing
cnovl_sm$est.var.slope # 28.41725    
cnovl_sm$est.var.season # 1.194057     
cnovl_sm$irr.var # 87.2518

# Modelo DLM com sample error

# Parâmetros iniciais:

par_1<-c(log(27.04))
par_2<-c(log(0.001)) 
par_3<-c(log(25.01)) 
par_4<-c(log(0.39))

# Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Estimação do Modelo

modelo_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

modelo_bsm_error<-modelo_bsm_error_ini[[1]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes,AIC)
resultados

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_bsm_error$ts.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_bsm_error$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "CV desocupados: model-based",
                             "Tendência CV desocupados (model-based)"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### CENTRAL ####################################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/06_teste_bsm.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para central 

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
cen<-baseestr0324$`10-Central`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtcen<-baseal0324$`10-Central` ## Arquivo "cru", saída direta da rotina da base por rotação
dbcen<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/10_params_cen.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- cen$Total.de.desocupados/1000
se_db <- cen$sd_d/1000
cv_db <- se_db/y
par_ar_erro <- dbcen$parerro_d

# Modelo DLM sem erro amostral:
# Parâmetros tese

par_1<-c(log(0.001))
par_2<-c(log(0.001)) 
par_3<-c(log(0.001)) 

## Input dos parâmetros iniciais do modelo

grid_dlm <- expand.grid(par_1,par_2,par_3)

### Estimação do Modelo

modelo_dlm_bsm_ini<-lapply(1:nrow(grid_dlm), function(i)  tryCatch(f.teste_bsm(y,grid_dlm[i,]),error=function(e) {rep(NA,4)}))

modelo_dlm_bsm<-modelo_dlm_bsm_ini[[1]]

## Verificando a convergência

convergencia_dlm<-rbind(modelo_dlm_bsm$fit$convergence)
colnames(convergencia_dlm)<-c("convergence") 

## Parâmetros estimados:

parametros_dlm<-rbind(c(round(exp(modelo_dlm_bsm$fit$par),4)))
row.names(parametros_dlm)<-c("BSM_DLM")
colnames(parametros_dlm)<-c("Slope","Seasonal","Irregular")

# Critérios de informação: AIC e BIC

AICdlm<-rbind(2*(modelo_dlm_bsm$fit$value)+2*5)
colnames(AICdlm)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_dlm_bsm$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos_dlm<-list(modelo_dlm_bsm)
testes_dlm<-sapply(lista_modelos_dlm, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                         round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                         teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes_dlm<-t(testes_dlm)
row.names(testes_dlm)<-c("BSM_DLM")
colnames(testes_dlm)<-c("Shapiro","Box","H")

resultados_dlm<-cbind(convergencia_dlm,parametros_dlm,testes_dlm, AICdlm)
resultados_dlm

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_dlm_bsm$ts.original,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                            "Signal model-based unemployment",
                            "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_dlm_bsm$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.signal,start = 2012,frequency=4),
  ts(modelo_dlm_bsm$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV design-based unemployment",
                             "CV signal model-based unemployment",
                             "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### Teste modelo UCM CENTRAL:
# Utilizando modelo sm com a série escalonada

y_red<-ts(y, start = c(2012, 1), frequency = 4)
ep_db<-ts(se_db, start = c(2012, 1), frequency = 4)
cv_db<-ts(cv_db,start=c(2012,1),frequency = 4)

cnocen_sm<-ucm(y_red~0,data=y_red,level=TRUE, level.var = 0, slope = TRUE, season = TRUE, season.length = 4, cycle = FALSE, irregular = TRUE)

centrend_sm<-(cnocen_sm$s.level)+(cnocen_sm$s.slope)
centrend_sm<-(centrend_sm)*1000

ts.plot((y*1000), centrend_sm, col = c("black", "red"),lty = c(1, 2),lwd = 2, main = "10-Central",
        ylab = "Total de Desocupados",xlab = "Tempo")
legend("bottom", legend = c("Total de Desocupados", "Tendência Estimada Smooth"), col = c("black", "red"), lty = c(1, 2), lwd = 2)

# Resgatando as variâncias:

cnocen_sm$est.var.level # nula por causa do modelo smoothing
cnocen_sm$est.var.slope # 53.27499     
cnocen_sm$est.var.season # 0.001126252      
cnocen_sm$irr.var # 83.0792

# Modelo DLM com sample error

# Parâmetros iniciais:

par_1<-c(log(0.001))
par_2<-c(log(0.001)) 
par_3<-c(log(0.001)) 
par_4<-c(log(1.93))

# Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Estimação do Modelo

modelo_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

modelo_bsm_error<-modelo_bsm_error_ini[[1]]

# Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

# Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Slope","Seasonal","Irregular","Sample Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

# Matriz Hessiana

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes,AIC)
resultados

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_bsm_error$ts.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts((modelo_bsm_error$cv.original)*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "CV desocupados: model-based",
                             "Tendência CV desocupados (model-based)"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)
