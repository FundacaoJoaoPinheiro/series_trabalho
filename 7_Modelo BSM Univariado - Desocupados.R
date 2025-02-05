################################################################################
##                          MODELO BSM UNIVARIADO                             ##
################################################################################

library(dlm)
library(tidyverse)
library(beepr)
library(parallel)

options(scipen=999)

## Anotações
  # Função 3a modificada para input correto dos hiperparâmetros
    # Na parte dos params

  # A diferença entre a 07_teste_bsm_error e a 09_modelo3a é o input da sazonalidade. Na f.modelo3a ela é feita por dummies
    # Outra diferença: ordenação nos params dentro da função
      # Qual o motivo dessa ordenação?

  # Apesar da não convergência, a RIDE apresentou um dos melhores gráficos (f.teste_bsm_error + hiperparametros tese)

  # Modelos que convergiram f.teste_bsm:
    # Entorno; Zona da Mata; Central
      # Entretanto, nenhum desses 3 modelos apresentaram uma matriz hessiana positiva semidefinida

  # Modelos que convergiram f.modelo3a:
    # Entorno; COLAR; MATA; VALE
  
### MODELO BH ##################################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")
source("data/funcoes/09_modelo3a.R")
source("data/funcoes/10_modelo3b.R")
source("data/funcoes/11_modelo3c.R")
source("data/funcoes/12_modelo3d.R")

## Carregando bases e definindo objeto para BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
bh<-baseestr0324$`01-Belo Horizonte`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtbh<-baseal0324$`01-Belo Horizonte` ## Arquivo "cru", saída direta da rotina da base por rotação
dbbh<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/01_params_bh.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- bh$Total.de.desocupados
se_db <- bh$sd_d
cv_db <- bh$CV.desocupados
par_ar_erro <- dbbh$parerro_d
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
  # Conforme recomendação, estipulando inicialmente seq(1)
  # Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-c(log(22.42)) #par_1<-seq(-6,6,3)
par_2<-c(log(0.001)) #par_2<-seq(-6,6,3)
par_3<-c(log(41.25)) #par_3<-seq(-6,6,3)
par_4<-c(log(0.4)) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend

# Teste: parâmetros alterados para rodar a função modelo3c
  # Alteração por conta do input da matriz V
    # Tentei colocar o <-c(log(0.4)) assim como nas funções anteriores, porém, o resultado do modelo foi nulo

#par_5<-c(log(22.42))
#par_6<-seq(-6,6,3)
#par_7<-c(log(41.25))
#par_8<-c(log(0.4))

## Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)
# grid_error <- expand.grid(par_5,par_6,par_7,par_8)

# Obs: Tendo em vista que os parâmetros iniciais foram definidos conforme a tabela 14 da tese, o processamento paralelo foi retirado
  # Ademais, tendo os parâmetros iniciais, a etapa de avaliação dos hiperparâmetros foi retirada

### Estimação do Modelo

# Primeiro e terceiro modelos ocultados em relação à rotina de referência

modelos_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

# "Eliminando" o formato de sublistas do objeto
  # Para a função 3c: sublista que "deu certo" foi a 4

modelo_bsm_error<-modelos_bsm_error_ini[[1]]

## Verificando a convergência
  # Ainda assim não convergiu

convergencia<-rbind(modelo_bsm_error$fit$convergence)
colnames(convergencia)<-c("convergence") 

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))
row.names(parametros)<-c("BSM_error")
colnames(parametros)<-c("Slope","Seasonal","Irregular","Sampling Error") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

#BIC<-rbind(
#  2*(modelo_bsm$fit$value)+2*4*log(modelo_bsm_error$T),
#  2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T),
#colnames(BIC)<-"BIC"

# Avaliando a matriz hessiana
  # Deve ser positiva definida

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Anotações e resultados das funções baseados nos parâmetros retirados da tese:
  # f.modelo3a: convergiu e matriz hessiana positiva
  # f.modelo3b: não convergiu e matriz hessiana não positiva
  # f.modelo3c: convergiu e matriz hessiana não positiva
  # f.modelo3d: não convergiu e matriz hessiana não positiva

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC)
resultados

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))

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
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

# Teste Função 3a BH:

testefun3a<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.modelo3a(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
teste3a<-testefun3a[[1]]
convergencia3a<-rbind(teste3a$fit$convergence) # Sem convergência -> 52
colnames(convergencia3a)<-c("convergence")
parametros3a<-rbind(c(round(exp(teste3a$fit$par),4)))
row.names(parametros3a)<-c("BSM_error")
colnames(parametros3a)<-c("Slope","Seasonal","Irregular","Sampling Error")
AIC3a<- rbind(2*(teste3a$fit$value)+2*5)
colnames(AIC3a)<-"AIC"
all(eigen(teste3a$fit$hessian, only.values = TRUE)$values > 0) # False
lista_modelos3a<-list(teste3a)
testes3a<-sapply(lista_modelos3a, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes3a<-t(testes3a)
row.names(testes3a)<-c("BSM_error")
colnames(testes3a)<-c("Shapiro","Box","H")
resultados3a<-cbind(convergencia3a,parametros3a,testes3a, AIC3a)
resultados3a

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(teste3a$ts.original,start = 2012,frequency=4),
  ts(teste3a$ts.signal,start = 2012,frequency=4),
  ts(teste3a$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts(teste3a$cv.original,start = 2012,frequency=4),
  ts(teste3a$cv.signal,start = 2012,frequency=4),
  ts(teste3a$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### ENTORNO METROPOLITANO ###############################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")
source("data/funcoes/09_modelo3a.R")

## Carregando bases e definindo objeto para o Entorno

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
ent<-baseestr0324$`02-Entorno metropolitano de BH`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtent<-baseal0324$`02-Entorno metropolitano de BH` ## Arquivo "cru", saída direta da rotina da base por rotação
dbent<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/02_params_ent.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- ent$Total.de.ocupados
se_db <- ent$sd_d
cv_db <- ent$CV.desocupados
par_ar_erro <- dbent$parerro_d
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-c(log(115.07)) #par_1<-seq(-6,6,3)
par_2<-c(log(0.001)) #par_2<-seq(-6,6,3)
par_3<-c(log(69.04)) #par_3<-seq(-6,6,3)
par_4<-c(log(0.11)) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend

## Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Obs: Tendo em vista que os parâmetros iniciais foram definidos conforme a tabela 14 da tese, o processamento paralelo foi retirado
  # Ademais, tendo os parâmetros iniciais, a etapa de avaliação dos hiperparâmetros foi retirada

### Estimação do Modelo

# Primeiro e terceiro modelos ocultados em relação à rotina de referência

modelos_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

# "Eliminando" o formato de sublistas do objeto
# Para a função 3c: sublista que "deu certo" foi a 4

modelo_bsm_error<-modelos_bsm_error_ini[[1]]

## Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence") 

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Slope","Seasonal","Irregular","Sampling Error") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

#BIC<-rbind(
#  2*(modelo_bsm$fit$value)+2*4*log(modelo_bsm_error$T),
#  2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T),
#colnames(BIC)<-"BIC"

# Avaliando a matriz hessiana
# Deve ser positiva definida

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Anotações e resultados das funções baseados nos parâmetros retirados da tese:
  # f.teste_bsm_error: convergiu e matriz hessiana não positiva

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC)
resultados

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))

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
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)


fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

# Teste Função 3a ENT:

testefun3a<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.modelo3a(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
teste3a<-testefun3a[[1]]
convergencia3a<-rbind(teste3a$fit$convergence) # Convergiu
colnames(convergencia3a)<-c("convergence")
parametros3a<-rbind(c(round(exp(teste3a$fit$par),4)))
row.names(parametros3a)<-c("BSM_error")
colnames(parametros3a)<-c("Slope","Seasonal","Irregular","Sampling Error")
AIC3a<- rbind(2*(teste3a$fit$value)+2*5)
colnames(AIC3a)<-"AIC"
all(eigen(teste3a$fit$hessian, only.values = TRUE)$values > 0) # False
lista_modelos3a<-list(teste3a)
testes3a<-sapply(lista_modelos3a, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                     round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                     teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes3a<-t(testes3a)
row.names(testes3a)<-c("BSM_error")
colnames(testes3a)<-c("Shapiro","Box","H")
resultados3a<-cbind(convergencia3a,parametros3a,testes3a, AIC3a)
resultados3a

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(teste3a$ts.original,start = 2012,frequency=4),
  ts(teste3a$ts.signal,start = 2012,frequency=4),
  ts(teste3a$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts(teste3a$cv.original,start = 2012,frequency=4),
  ts(teste3a$cv.signal,start = 2012,frequency=4),
  ts(teste3a$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### COLAR METROPOLITANO DE BH ##################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")
source("data/funcoes/09_modelo3a.R")

## Carregando bases e definindo objeto para o Colar BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
col<-baseestr0324$`03-Colar metropolitano de BH`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtcol<-baseal0324$`03-Colar metropolitano de BH` ## Arquivo "cru", saída direta da rotina da base por rotação
dbcol<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/03_params_col.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- col$Total.de.desocupados
se_db <- col$sd_d
cv_db <- col$CV.desocupados
par_ar_erro <- dbcol$parerro_d
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-c(log(2.97)) #par_1<-seq(-6,6,3)
par_2<-c(log(0.001)) #par_2<-seq(-6,6,3)
par_3<-c(log(0.001)) #par_3<-seq(-6,6,3)
par_4<-c(log(0.49)) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend

## Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Obs: Tendo em vista que os parâmetros iniciais foram definidos conforme a tabela 14 da tese, o processamento paralelo foi retirado
# Ademais, tendo os parâmetros iniciais, a etapa de avaliação dos hiperparâmetros foi retirada

### Estimação do Modelo

# Primeiro e terceiro modelos ocultados em relação à rotina de referência

modelos_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

# "Eliminando" o formato de sublistas do objeto

modelo_bsm_error<-modelos_bsm_error_ini[[1]]

## Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence") 

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Slope","Seasonal","Irregular","Sampling Error") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

#BIC<-rbind(
#  2*(modelo_bsm$fit$value)+2*4*log(modelo_bsm_error$T),
#  2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T),
#colnames(BIC)<-"BIC"

# Avaliando a matriz hessiana
# Deve ser positiva definida

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Anotações e resultados das funções baseados nos parâmetros retirados da tese:
  # f.teste_bsm_error: não convergiu e matriz hessiana positiva
  # f.modelo3a: não convergiu e matriz hessiana não positiva
# Resultados totalmente fora de ordem

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC)
resultados

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))

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
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)


fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

# Teste Função 3a COL:

testefun3a<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.modelo3a(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
teste3a<-testefun3a[[1]]
convergencia3a<-rbind(teste3a$fit$convergence) # Convergiu
colnames(convergencia3a)<-c("convergence")
parametros3a<-rbind(c(round(exp(teste3a$fit$par),4)))
row.names(parametros3a)<-c("BSM_error")
colnames(parametros3a)<-c("Slope","Seasonal","Irregular","Sampling Error")
AIC3a<- rbind(2*(teste3a$fit$value)+2*5)
colnames(AIC3a)<-"AIC"
all(eigen(teste3a$fit$hessian, only.values = TRUE)$values > 0) # False
lista_modelos3a<-list(teste3a)
testes3a<-sapply(lista_modelos3a, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                     round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                     teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes3a<-t(testes3a)
row.names(testes3a)<-c("BSM_error")
colnames(testes3a)<-c("Shapiro","Box","H")
resultados3a<-cbind(convergencia3a,parametros3a,testes3a, AIC3a)
resultados3a

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(teste3a$ts.original,start = 2012,frequency=4),
  ts(teste3a$ts.signal,start = 2012,frequency=4),
  ts(teste3a$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts(teste3a$cv.original,start = 2012,frequency=4),
  ts(teste3a$cv.signal,start = 2012,frequency=4),
  ts(teste3a$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### RIDE de Brasília em Minas ##################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")
source("data/funcoes/09_modelo3a.R")

## Carregando bases e definindo objeto para RIDE

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
rid<-baseestr0324$`04-RIDE de Brasília em Minas`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtrid<-baseal0324$`04-RIDE de Brasília em Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbrid<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/04_params_rid.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- rid$Total.de.desocupados
se_db <- rid$sd_d
cv_db <- rid$CV.desocupados
par_ar_erro <- dbrid$parerro_d
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-c(log(0.001)) #par_1<-seq(-6,6,3)
par_2<-c(log(0.001)) #par_2<-seq(-6,6,3)
par_3<-c(log(0.94)) #par_3<-seq(-6,6,3)
par_4<-c(log(0.44)) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend

## Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Obs: Tendo em vista que os parâmetros iniciais foram definidos conforme a tabela 14 da tese, o processamento paralelo foi retirado
  # Ademais, tendo os parâmetros iniciais, a etapa de avaliação dos hiperparâmetros foi retirada

### Estimação do Modelo

# Primeiro e terceiro modelos ocultados em relação à rotina de referência

modelos_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

# "Eliminando" o formato de sublistas do objeto

modelo_bsm_error<-modelos_bsm_error_ini[[1]]

## Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence") 

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Slope","Seasonal","Irregular","Sampling Error") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

#BIC<-rbind(
#  2*(modelo_bsm$fit$value)+2*4*log(modelo_bsm_error$T),
#  2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T),
#colnames(BIC)<-"BIC"

# Avaliando a matriz hessiana
  # Deve ser positiva definida

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Anotações e resultados das funções baseados nos parâmetros retirados da tese:
  # f.teste_bsm_error: não convergiu e matriz hessiana positiva

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC)
resultados

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))

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
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)


fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

# Teste Função 3a RIDE:

testefun3a<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.modelo3a(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
teste3a<-testefun3a[[1]]
convergencia3a<-rbind(teste3a$fit$convergence) # Não Convergiu
colnames(convergencia3a)<-c("convergence")
parametros3a<-rbind(c(round(exp(teste3a$fit$par),4)))
row.names(parametros3a)<-c("BSM_error")
colnames(parametros3a)<-c("Slope","Seasonal","Irregular","Sampling Error")
AIC3a<- rbind(2*(teste3a$fit$value)+2*5)
colnames(AIC3a)<-"AIC"
all(eigen(teste3a$fit$hessian, only.values = TRUE)$values > 0) # False
lista_modelos3a<-list(teste3a)
testes3a<-sapply(lista_modelos3a, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                     round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                     teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes3a<-t(testes3a)
row.names(testes3a)<-c("BSM_error")
colnames(testes3a)<-c("Shapiro","Box","H")
resultados3a<-cbind(convergencia3a,parametros3a,testes3a, AIC3a)
resultados3a

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(teste3a$ts.original,start = 2012,frequency=4),
  ts(teste3a$ts.signal,start = 2012,frequency=4),
  ts(teste3a$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts(teste3a$cv.original,start = 2012,frequency=4),
  ts(teste3a$cv.signal,start = 2012,frequency=4),
  ts(teste3a$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### SUL DE MINAS ###############################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")
source("data/funcoes/09_modelo3a.R")

## Carregando bases e definindo objeto para o Sul

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
sul<-baseestr0324$`05-Sul de Minas`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtsul<-baseal0324$`05-Sul de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbsul<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/05_params_sul.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- sul$Total.de.desocupados
se_db <- sul$sd_d
cv_db <- sul$CV.desocupados
par_ar_erro <- dbsul$parerro_d
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-c(log(13.88)) #par_1<-seq(-6,6,3)
par_2<-c(log(0.001)) #par_2<-seq(-6,6,3)
par_3<-c(log(0.001)) #par_3<-seq(-6,6,3)
par_4<-c(log(1.03)) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend

## Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Obs: Tendo em vista que os parâmetros iniciais foram definidos conforme a tabela 14 da tese, o processamento paralelo foi retirado
# Ademais, tendo os parâmetros iniciais, a etapa de avaliação dos hiperparâmetros foi retirada

### Estimação do Modelo

# Primeiro e terceiro modelos ocultados em relação à rotina de referência

modelos_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

# "Eliminando" o formato de sublistas do objeto

modelo_bsm_error<-modelos_bsm_error_ini[[1]]

## Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence") 

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Slope","Seasonal","Irregular","Sampling Error") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

#BIC<-rbind(
#  2*(modelo_bsm$fit$value)+2*4*log(modelo_bsm_error$T),
#  2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T),
#colnames(BIC)<-"BIC"


# Avaliando a matriz hessiana
# Deve ser positiva definida

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Anotações e resultados das funções baseados nos parâmetros retirados da tese:
# f.teste_bsm_error: não convergiu e matriz hessiana positiva

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC)
resultados

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))

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
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)


fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

# Teste Função 3a sul:

testefun3a<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.modelo3a(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
teste3a<-testefun3a[[1]]
convergencia3a<-rbind(teste3a$fit$convergence) # Não Convergiu
colnames(convergencia3a)<-c("convergence")
parametros3a<-rbind(c(round(exp(teste3a$fit$par),4)))
row.names(parametros3a)<-c("BSM_error")
colnames(parametros3a)<-c("Slope","Seasonal","Irregular","Sampling Error")
AIC3a<- rbind(2*(teste3a$fit$value)+2*5)
colnames(AIC3a)<-"AIC"
all(eigen(teste3a$fit$hessian, only.values = TRUE)$values > 0) # False
lista_modelos3a<-list(teste3a)
testes3a<-sapply(lista_modelos3a, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                     round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                     teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes3a<-t(testes3a)
row.names(testes3a)<-c("BSM_error")
colnames(testes3a)<-c("Shapiro","Box","H")
resultados3a<-cbind(convergencia3a,parametros3a,testes3a, AIC3a)
resultados3a

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(teste3a$ts.original,start = 2012,frequency=4),
  ts(teste3a$ts.signal,start = 2012,frequency=4),
  ts(teste3a$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts(teste3a$cv.original,start = 2012,frequency=4),
  ts(teste3a$cv.signal,start = 2012,frequency=4),
  ts(teste3a$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### TRIÂNGULO MINEIRO ##########################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")
source("data/funcoes/09_modelo3a.R")

## Carregando bases e definindo objeto para Triângulo Mineiro

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
trg<-baseestr0324$`06-Triângulo Mineiro`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dttrg<-baseal0324$`06-Triângulo Mineiro` ## Arquivo "cru", saída direta da rotina da base por rotação
dbtrg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/06_params_trg.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- trg$Total.de.desocupados
se_db <- trg$sd_d
cv_db <- trg$CV.desocupados
par_ar_erro <- dbtrg$parerro_d
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-c(log(0.001)) #par_1<-seq(-6,6,3)
par_2<-c(log(0.001)) #par_2<-seq(-6,6,3)
par_3<-c(log(0.001)) #par_3<-seq(-6,6,3)
par_4<-c(log(1.53)) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend

## Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Obs: Tendo em vista que os parâmetros iniciais foram definidos conforme a tabela 14 da tese, o processamento paralelo foi retirado
# Ademais, tendo os parâmetros iniciais, a etapa de avaliação dos hiperparâmetros foi retirada

### Estimação do Modelo

# Primeiro e terceiro modelos ocultados em relação à rotina de referência

modelos_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

# "Eliminando" o formato de sublistas do objeto

modelo_bsm_error<-modelos_bsm_error_ini[[1]]

## Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence") 

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Slope","Seasonal","Irregular","Sampling Error") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

#BIC<-rbind(
#  2*(modelo_bsm$fit$value)+2*4*log(modelo_bsm_error$T),
#  2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T),
#colnames(BIC)<-"BIC"


# Avaliando a matriz hessiana
# Deve ser positiva definida

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Anotações e resultados das funções baseados nos parâmetros retirados da tese:
# f.teste_bsm_error: não convergiu e matriz hessiana não positiva

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC)
resultados

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))

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
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)


fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

# Teste Função 3a trg:

testefun3a<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.modelo3a(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
teste3a<-testefun3a[[1]]
convergencia3a<-rbind(teste3a$fit$convergence) # Não Convergiu
colnames(convergencia3a)<-c("convergence")
parametros3a<-rbind(c(round(exp(teste3a$fit$par),4)))
row.names(parametros3a)<-c("BSM_error")
colnames(parametros3a)<-c("Slope","Seasonal","Irregular","Sampling Error")
AIC3a<- rbind(2*(teste3a$fit$value)+2*5)
colnames(AIC3a)<-"AIC"
all(eigen(teste3a$fit$hessian, only.values = TRUE)$values > 0) # False
lista_modelos3a<-list(teste3a)
testes3a<-sapply(lista_modelos3a, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                     round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                     teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes3a<-t(testes3a)
row.names(testes3a)<-c("BSM_error")
colnames(testes3a)<-c("Shapiro","Box","H")
resultados3a<-cbind(convergencia3a,parametros3a,testes3a, AIC3a)
resultados3a

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(teste3a$ts.original,start = 2012,frequency=4),
  ts(teste3a$ts.signal,start = 2012,frequency=4),
  ts(teste3a$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts(teste3a$cv.original,start = 2012,frequency=4),
  ts(teste3a$cv.signal,start = 2012,frequency=4),
  ts(teste3a$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### ZONA DA MATA ###############################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")
source("data/funcoes/09_modelo3a.R")

## Carregando bases e definindo objeto para a Zona da Mata

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
mat<-baseestr0324$`07-Mata de Minas Gerais`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtmat<-baseal0324$`07-Mata de Minas Gerais` ## Arquivo "cru", saída direta da rotina da base por rotação
dbmat<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/07_params_mat.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- mat$Total.de.desocupados
se_db <- mat$sd_d
cv_db <- mat$CV.desocupados
par_ar_erro <- dbmat$parerro_d
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-c(log(9.72)) #par_1<-seq(-6,6,3)
par_2<-c(log(0.001)) #par_2<-seq(-6,6,3)
par_3<-c(log(0.001)) #par_3<-seq(-6,6,3)
par_4<-c(log(0.95)) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend

## Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Obs: Tendo em vista que os parâmetros iniciais foram definidos conforme a tabela 14 da tese, o processamento paralelo foi retirado
# Ademais, tendo os parâmetros iniciais, a etapa de avaliação dos hiperparâmetros foi retirada

### Estimação do Modelo

# Primeiro e terceiro modelos ocultados em relação à rotina de referência

modelos_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

# "Eliminando" o formato de sublistas do objeto

modelo_bsm_error<-modelos_bsm_error_ini[[1]]

## Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence") 

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Slope","Seasonal","Irregular","Sampling Error") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

#BIC<-rbind(
#  2*(modelo_bsm$fit$value)+2*4*log(modelo_bsm_error$T),
#  2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T),
#colnames(BIC)<-"BIC"


# Avaliando a matriz hessiana
# Deve ser positiva definida

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Anotações e resultados das funções baseados nos parâmetros retirados da tese:
  # f.teste_bsm_error: convergiu e matriz hessiana não positiva

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC)
resultados

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))

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
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)


fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

# Teste Função 3a mata:

testefun3a<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.modelo3a(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
teste3a<-testefun3a[[1]]
convergencia3a<-rbind(teste3a$fit$convergence) # Convergiu
colnames(convergencia3a)<-c("convergence")
parametros3a<-rbind(c(round(exp(teste3a$fit$par),4)))
row.names(parametros3a)<-c("BSM_error")
colnames(parametros3a)<-c("Slope","Seasonal","Irregular","Sampling Error")
AIC3a<- rbind(2*(teste3a$fit$value)+2*5)
colnames(AIC3a)<-"AIC"
all(eigen(teste3a$fit$hessian, only.values = TRUE)$values > 0) # False
lista_modelos3a<-list(teste3a)
testes3a<-sapply(lista_modelos3a, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                     round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                     teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes3a<-t(testes3a)
row.names(testes3a)<-c("BSM_error")
colnames(testes3a)<-c("Shapiro","Box","H")
resultados3a<-cbind(convergencia3a,parametros3a,testes3a, AIC3a)
resultados3a

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(teste3a$ts.original,start = 2012,frequency=4),
  ts(teste3a$ts.signal,start = 2012,frequency=4),
  ts(teste3a$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts(teste3a$cv.original,start = 2012,frequency=4),
  ts(teste3a$cv.signal,start = 2012,frequency=4),
  ts(teste3a$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### NORTE DE MINAS GERAIS ######################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")
source("data/funcoes/09_modelo3a.R")

## Carregando bases e definindo objeto para o Norte

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
nrt<-baseestr0324$`08-Norte de Minas`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtnrt<-baseal0324$`08-Norte de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbnrt<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/08_params_nrt.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- nrt$Total.de.desocupados
se_db <- nrt$sd_d
cv_db <- nrt$CV.desocupados
par_ar_erro <- dbnrt$parerro_d
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-c(log(16.63)) #par_1<-seq(-6,6,3)
par_2<-c(log(0.001)) #par_2<-seq(-6,6,3)
par_3<-c(log(50.97)) #par_3<-seq(-6,6,3)
par_4<-c(log(0.83)) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend

## Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Obs: Tendo em vista que os parâmetros iniciais foram definidos conforme a tabela 14 da tese, o processamento paralelo foi retirado
# Ademais, tendo os parâmetros iniciais, a etapa de avaliação dos hiperparâmetros foi retirada

### Estimação do Modelo

# Primeiro e terceiro modelos ocultados em relação à rotina de referência

modelos_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

# "Eliminando" o formato de sublistas do objeto

modelo_bsm_error<-modelos_bsm_error_ini[[1]]

## Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence") 

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Slope","Seasonal","Irregular","Sampling Error") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

#BIC<-rbind(
#  2*(modelo_bsm$fit$value)+2*4*log(modelo_bsm_error$T),
#  2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T),
#colnames(BIC)<-"BIC"


# Avaliando a matriz hessiana
# Deve ser positiva definida

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Anotações e resultados das funções baseados nos parâmetros retirados da tese:
# f.teste_bsm_error: não convergiu e matriz hessiana positiva

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC)
resultados

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))

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
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)


fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

# Teste Função 3a norte:

testefun3a<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.modelo3a(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
teste3a<-testefun3a[[1]]
convergencia3a<-rbind(teste3a$fit$convergence) # Não Convergiu
colnames(convergencia3a)<-c("convergence")
parametros3a<-rbind(c(round(exp(teste3a$fit$par),4)))
row.names(parametros3a)<-c("BSM_error")
colnames(parametros3a)<-c("Slope","Seasonal","Irregular","Sampling Error")
AIC3a<- rbind(2*(teste3a$fit$value)+2*5)
colnames(AIC3a)<-"AIC"
all(eigen(teste3a$fit$hessian, only.values = TRUE)$values > 0) # False
lista_modelos3a<-list(teste3a)
testes3a<-sapply(lista_modelos3a, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                     round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                     teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes3a<-t(testes3a)
row.names(testes3a)<-c("BSM_error")
colnames(testes3a)<-c("Shapiro","Box","H")
resultados3a<-cbind(convergencia3a,parametros3a,testes3a, AIC3a)
resultados3a

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(teste3a$ts.original,start = 2012,frequency=4),
  ts(teste3a$ts.signal,start = 2012,frequency=4),
  ts(teste3a$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts(teste3a$cv.original,start = 2012,frequency=4),
  ts(teste3a$cv.signal,start = 2012,frequency=4),
  ts(teste3a$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### VALE DO RIO DOCE ###########################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")
source("data/funcoes/09_modelo3a.R")

## Carregando bases e definindo objeto para o vale

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
vl<-baseestr0324$`09-Vale do Rio Doce`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtvl<-baseal0324$`09-Vale do Rio Doce` ## Arquivo "cru", saída direta da rotina da base por rotação
dbvl<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/09_params_rio.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- vl$Total.de.desocupados
se_db <- vl$sd_d
cv_db <- vl$CV.desocupados
par_ar_erro <- dbvl$parerro_d
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-c(log(27.04)) #par_1<-seq(-6,6,3)
par_2<-c(log(0.001)) #par_2<-seq(-6,6,3)
par_3<-c(log(25.01)) #par_3<-seq(-6,6,3)
par_4<-c(log(0.39)) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend

## Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Obs: Tendo em vista que os parâmetros iniciais foram definidos conforme a tabela 14 da tese, o processamento paralelo foi retirado
# Ademais, tendo os parâmetros iniciais, a etapa de avaliação dos hiperparâmetros foi retirada

### Estimação do Modelo

# Primeiro e terceiro modelos ocultados em relação à rotina de referência

modelos_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

# "Eliminando" o formato de sublistas do objeto

modelo_bsm_error<-modelos_bsm_error_ini[[1]]

## Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence") 

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Slope","Seasonal","Irregular","Sampling Error") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

#BIC<-rbind(
#  2*(modelo_bsm$fit$value)+2*4*log(modelo_bsm_error$T),
#  2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T),
#colnames(BIC)<-"BIC"


# Avaliando a matriz hessiana
# Deve ser positiva definida

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Anotações e resultados das funções baseados nos parâmetros retirados da tese:
  # f.teste_bsm_error: não convergiu e matriz hessiana positiva

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC)
resultados

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))

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
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)


fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

# Teste Função 3a vale:

testefun3a<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.modelo3a(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
teste3a<-testefun3a[[1]]
convergencia3a<-rbind(teste3a$fit$convergence) # Convergiu
colnames(convergencia3a)<-c("convergence")
parametros3a<-rbind(c(round(exp(teste3a$fit$par),4)))
row.names(parametros3a)<-c("BSM_error")
colnames(parametros3a)<-c("Slope","Seasonal","Irregular","Sampling Error")
AIC3a<- rbind(2*(teste3a$fit$value)+2*5)
colnames(AIC3a)<-"AIC"
all(eigen(teste3a$fit$hessian, only.values = TRUE)$values > 0) # False
lista_modelos3a<-list(teste3a)
testes3a<-sapply(lista_modelos3a, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                     round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                     teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes3a<-t(testes3a)
row.names(testes3a)<-c("BSM_error")
colnames(testes3a)<-c("Shapiro","Box","H")
resultados3a<-cbind(convergencia3a,parametros3a,testes3a, AIC3a)
resultados3a

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(teste3a$ts.original,start = 2012,frequency=4),
  ts(teste3a$ts.signal,start = 2012,frequency=4),
  ts(teste3a$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts(teste3a$cv.original,start = 2012,frequency=4),
  ts(teste3a$cv.signal,start = 2012,frequency=4),
  ts(teste3a$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

### CENTRAL ####################################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")
source("data/funcoes/09_modelo3a.R")

## Carregando bases e definindo objeto para central 

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
cen<-baseestr0324$`10-Central`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtcen<-baseal0324$`10-Central` ## Arquivo "cru", saída direta da rotina da base por rotação
dbcen<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/10_params_cen.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- cen$Total.de.desocupados
se_db <- cen$sd_d
cv_db <- cen$CV.desocupados
par_ar_erro <- dbcen$parerro_d
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-c(log(0.001)) #par_1<-seq(-6,6,3)
par_2<-c(log(0.001)) #par_2<-seq(-6,6,3)
par_3<-c(log(0.001)) #par_3<-seq(-6,6,3)
par_4<-c(log(1.93)) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend

## Input dos parâmetros iniciais do modelo

grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Obs: Tendo em vista que os parâmetros iniciais foram definidos conforme a tabela 14 da tese, o processamento paralelo foi retirado
# Ademais, tendo os parâmetros iniciais, a etapa de avaliação dos hiperparâmetros foi retirada

### Estimação do Modelo

# Primeiro e terceiro modelos ocultados em relação à rotina de referência

modelos_bsm_error_ini<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))

# "Eliminando" o formato de sublistas do objeto

modelo_bsm_error<-modelos_bsm_error_ini[[1]]

## Verificando a convergência

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence") 

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Slope","Seasonal","Irregular","Sampling Error") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

#BIC<-rbind(
#  2*(modelo_bsm$fit$value)+2*4*log(modelo_bsm_error$T),
#  2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T),
#colnames(BIC)<-"BIC"

# Avaliando a matriz hessiana
# Deve ser positiva definida

all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)

# Anotações e resultados das funções baseados nos parâmetros retirados da tese:
# f.teste_bsm_error: convergiu e matriz hessiana não positiva

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC)
resultados

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))

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
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)


fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

# Teste Função 3a central:

testefun3a<-lapply(1:nrow(grid_error), function(i)  tryCatch(f.modelo3a(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
teste3a<-testefun3a[[1]]
convergencia3a<-rbind(teste3a$fit$convergence) # Não Convergiu
colnames(convergencia3a)<-c("convergence")
parametros3a<-rbind(c(round(exp(teste3a$fit$par),4)))
row.names(parametros3a)<-c("BSM_error")
colnames(parametros3a)<-c("Slope","Seasonal","Irregular","Sampling Error")
AIC3a<- rbind(2*(teste3a$fit$value)+2*5)
colnames(AIC3a)<-"AIC"
all(eigen(teste3a$fit$hessian, only.values = TRUE)$values > 0) # False
lista_modelos3a<-list(teste3a)
testes3a<-sapply(lista_modelos3a, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4),
                                                     round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                     teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes3a<-t(testes3a)
row.names(testes3a)<-c("BSM_error")
colnames(testes3a)<-c("Shapiro","Box","H")
resultados3a<-cbind(convergencia3a,parametros3a,testes3a, AIC3a)
resultados3a

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(teste3a$ts.original,start = 2012,frequency=4),
  ts(teste3a$ts.signal,start = 2012,frequency=4),
  ts(teste3a$ts.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("Design-based unemployment",
                             "Signal model-based unemployment",
                             "Trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts(teste3a$cv.original,start = 2012,frequency=4),
  ts(teste3a$cv.signal,start = 2012,frequency=4),
  ts(teste3a$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)

