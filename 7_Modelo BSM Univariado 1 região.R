################################################################################
##                          MODELO BSM UNIVARIADO                             ##
################################################################################

library(dlm)
library(tidyverse)
library(beepr)
library(parallel)

### BELO HORIZONTE - OCUPADA ###################################################

### TESTE INICIAL - MODELO "POR EXTENSO" #######################################

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/02_modelo_bsm.R")
source("data/funcoes/03_modelo_bsm_error.R")
source("data/funcoes/04_modelo_bsm_error_1.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/06_teste_bsm.R")
source("data/funcoes/07_teste_bsm_error.R")
source("data/funcoes/08_teste_bsm_error_1.R")

## Carregando bases e definindo objeto para BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
bh<-baseestr0324$`01-Belo Horizonte`
baserot <- readRDS("D:/FJP2425/Programacao/data/baserot0324.RDS")
dtbh<-baserot$`01-Belo Horizonte` ## Arquivo "cru", saída direta da rotina da base por rotação
dbbh<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/01_params_bh.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- bh$Total.de.ocupados
se_db <- bh$sd_o
cv_db <- bh$CV.ocupados
par_ar_erro <- dbbh$parerro_o
#plot(y, type="l")
#plot(cv_db*100, type="l")


## Estipulando parâmetros iniciais:
  # Conforme recomendação, estipulando inicialmente seq(1)
    # Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0)
par_5<-c(1)

## Possibilidades do modelo

## Possibilidades do modelo

grid <- expand.grid(par_1,par_2,par_3)
grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# No momento, essa repetição foi feita para realizar testes ao longo do código

comb1 <- expand.grid(par_1, par_2, par_3)
colnames(comb1) <- c("par_1", "par_2", "par_3")

comb2<-expand.grid(par_1, par_2, par_3, par_4)
colnames(comb2) <- c("par_1", "par_2", "par_3", "par_4")

comb3<-expand.grid(par_1, par_2, par_3, par_4, par_5)
colnames(comb3) <- c("par_1", "par_2", "par_3", "par_4","par_5")

## Ajuste do formato dos valores iniciais para o modelo

inicial <- as.matrix(comb2)


## Início do estudo da função

# Definindo o modelo

modelo<- list("fn"=function(params){
  m = dlmModPoly(2) + dlmModTrig(4)
  W = matrix(0,5,5)
  W[1, 1] <- exp(params[1])
  W[2, 2] <- exp(params[2])
  W[3, 3] <- exp(params[3])
  m$W <- W
  V =  exp(params[4])
  m$V <- V
  return(m)
})

i0 <- inicial[1:75,] # Com três parâmetros, a função também deu erro

modelo$initial<-i0

# Estimando hiperparâmetros:

modelo$fit <- dlmMLE(y, modelo$initial,modelo$fn, hessian=T,control = list(maxit = 10^8))

modelo$mod <- modelo$fn(modelo$fit$par)

# Aplicando o Filtro de Kalman:
  # Séries filtradas e suavizadas:

modelo$filtered <- dlmFilter(y,modelo$mod)
modelo$smoothed <- dlmSmooth(modelo$filtered)
modelo$m <- dropFirst(modelo$filtered$m)
modelo$sm <- dropFirst(modelo$smoothed$s)
modelo$res <- residuals(modelo$filtered,sd=FALSE)

# Definindo variável:

modelo$d<-length(modelo$mod$m0)+1
modelo$T<-length(y)

# Estatísticas de interesse:

modelo$ts.original<- y
modelo$ts.trend <- modelo[["m"]][,1]
modelo$ts.slope <- modelo[["m"]][,2]

modelo$ts.seasonal <- modelo[["m"]][,3]+modelo[["m"]][,4]+modelo[["m"]][,5]
modelo$ts.signal <- modelo$ts.trend +modelo$ts.seasonal
modelo$ts.irregular <- modelo$ts.original-(modelo$ts.signal)
modelo$ts.seasonal_adj <- modelo$ts.trend+modelo$ts.irregular

# Calculando Erro Padrão:

mse.list_bsm = dlmSvd2var(modelo[["filtered"]][["U.C"]], modelo[["filtered"]][["D.C"]])
se.mat_bsm = dropFirst(t(sapply(mse.list_bsm, FUN=function(x) sqrt(diag(x)))))

# Vetores indicadores da soma de estados:

c_sinal_bsm <- matrix(c(1,0,1,0, # somando tendência e os seis componentes sazonais
                        1),1,5) # indica com 1 qual a coluna(estado) quer considerar*caso trigonométrico
c_seasonal_bsm <- matrix(c(0,0,1,0, # somando os seis componentes sazonais
                           1),1,5) # indica com 1 qual a coluna(estado) quer considerar*caso trigonométrico

se.mat_bsm_sinal = dropFirst((sapply(mse.list_bsm, function(i) sqrt(c_sinal_bsm%*%i%*%t(c_sinal_bsm)) )))
se.mat_bsm_seasonal = dropFirst((sapply(mse.list_bsm, function(i) sqrt(c_seasonal_bsm%*%i%*%t(c_seasonal_bsm)) )))

modelo$se.original<-bh$sd_o
modelo$se.trend <- se.mat_bsm[,1]
modelo$se.slope <- se.mat_bsm[,2]
modelo$se.seasonal <- se.mat_bsm_seasonal
modelo$se.signal <- se.mat_bsm_sinal

modelo$cv.original<-bh$CV.ocupados
modelo$cv.trend<- modelo$se.trend/modelo$ts.trend*100
modelo$cv.signal<- modelo$se.signal/modelo$ts.signal*100

# Estatísticas de interesse -> suavizadas

modelo$ts.sm.trend <- modelo[["sm"]][,1]
modelo$ts.sm.slope <- modelo[["sm"]][,2]
modelo$ts.sm.seasonal <- modelo[["sm"]][,3]+modelo[["sm"]][,5]
modelo$ts.sm.signal <- modelo$ts.trend +modelo$ts.seasonal
modelo$ts.sm.irregular <- modelo$ts.original-(modelo$ts.signal)
modelo$ts.sm.seasonal_adj <- modelo$ts.trend+modelo$ts.irregular 

# Calculando o erro padrão

mse.list_bsm.sm = dlmSvd2var(modelo[["smoothed"]][["U.S"]], modelo[["smoothed"]][["D.S"]])
se.mat_bsm.sm = dropFirst(t(sapply(mse.list_bsm.sm, FUN=function(x) sqrt(diag(x)))))

# Mais vetores indicadores necessários para o state space

c_sinal_bsm.sm <- matrix(c(1,0,1,1, 
                           1),1,5) 
c_seasonal_bsm.sm <- matrix(c(0,0,1,1, 
                              1),1,5) 

se.mat_bsm_sinal.sm = dropFirst((sapply(mse.list_bsm.sm, function(i) sqrt(c_sinal_bsm.sm%*%i%*%t(c_sinal_bsm.sm)) )))
se.mat_bsm_seasonal.sm = dropFirst((sapply(mse.list_bsm.sm, function(i) sqrt(c_seasonal_bsm.sm%*%i%*%t(c_seasonal_bsm.sm)) )))

modelo$se.sm.trend <- se.mat_bsm.sm[,1]
modelo$se.sm.slope <- se.mat_bsm.sm[,2]
modelo$se.sm.seasonal <- se.mat_bsm_seasonal.sm
modelo$se.sm.signal <- se.mat_bsm_sinal.sm

modelo$cv.sm.trend<- modelo$se.sm.trend/modelo$ts.sm.trend*100
modelo$cv.sm.slope<- modelo$se.sm.slope/modelo$ts.sm.slope*100
modelo$cv.sm.signal<- modelo$se.sm.signal/modelo$ts.sm.signal*100

# Objeto para armazenar resultados:

a<-data.frame(
  level_ini = modelo$initial[,1],
  slope_ini = modelo$initial[,2],  
  seasonal_ini = modelo$initial[,3], 
  irregular_ini = modelo$initial[,4],
  
  level = modelo$fit$par[,1],  
  slope = modelo$fit$par[,2],  
  seasonal = modelo$fit$par[,3], 
  irregular = modelo$fit$par[,4],
  
  convergence = modelo$fit$convergence,
  log_like = modelo$fit$value 
)

# Avaliações

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


modelo_bsm<- modelo$fit$value # Falha: log_like apresentou um único resultado

convergencia <- rbind (c(modelo$fit$convergence))

parametros<-rbind(c(round(exp(modelo$fit$par),4),NA))

row.names(parametros)<-c(
  "BSM")

# AIC e BIC
AIC<-rbind(
  2*(modelo$fit$value)+2*4)
colnames(AIC)<-"AIC"

#BIC<-rbind(
#  2*(modelo_bsm$fit$value)+2*4*log(modelo_bsm_error_1$T),
#  2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error_1$T),
#  2*(modelo_bsm_error_1$fit$value)+2*4*log(modelo_bsm_error_1$T))
#colnames(BIC)<-"BIC"

# verificar se é positiva definida

all(eigen(modelo$fit$hessian, only.values = TRUE)$values > 0) # Nessa abordagem, não foi positiva definida. Talvez o erro esteja na otimização

# Diagnóstico dos resíduos:

testes<-sapply(modelo, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)


### TESTE MODELO UTILIZANDO DIRETAMENTE A FUNÇÃO ###############################

rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/02_modelo_bsm.R")
source("data/funcoes/03_modelo_bsm_error.R")
source("data/funcoes/04_modelo_bsm_error_1.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/06_teste_bsm.R")
source("data/funcoes/07_teste_bsm_error.R")
source("data/funcoes/08_teste_bsm_error_1.R")


## Carregando bases e definindo objeto para BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
bh<-baseestr0324$`01-Belo Horizonte`
baserot <- readRDS("D:/FJP2425/Programacao/data/baserot0324.RDS")
dtbh<-baserot$`01-Belo Horizonte` ## Arquivo "cru", saída direta da rotina da base por rotação
dbbh<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/01_params_bh.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- bh$Total.de.ocupados
se_db <- bh$sd_o
cv_db <- bh$CV.ocupados
par_ar_erro <- dbbh$parerro_o
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
  # Conforme recomendação, estipulando inicialmente seq(1)
  # Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-seq(0) # estava: c(0)
par_5<-seq(0) # Para teste # Estava: c(1)


## Possibilidades do modelo

grid <- expand.grid(par_1,par_2,par_3)
grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# No momento, essa repetição foi feita para realizar testes ao longo do código

comb1 <- expand.grid(par_1, par_2, par_3)
colnames(comb1) <- c("par_1", "par_2", "par_3")

comb2<-expand.grid(par_1, par_2, par_3, par_4)
colnames(comb2) <- c("par_1", "par_2", "par_3", "par_4")

comb3<-expand.grid(par_1, par_2, par_3, par_4, par_5)
colnames(comb3) <- c("par_1", "par_2", "par_3", "par_4","par_5")

numCores<-detectCores()
numCores

# criando clusters
cl<- makeCluster(numCores-1)

# salva imagem
save.image("partial.Rdata")

# envia dados para cada um dos cluster
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})

## Modelos com processamento paralelo

# Primeiro modelo: por que ao rodar com apenas 3 parâmetros temos um erro no primeiro modelo?

start_time <- Sys.time()
modelos_bsm_ini<-parLapply(cl,1:nrow(comb2), function(i)  tryCatch(f.teste_bsm(y,comb2[i,]),error=function(e) {rep(NA,3)})) # Posso desconsiderar
end_time <- Sys.time()
end_time - start_time

# Para o segundo modelo, foi necessário criar um 5 parâmetro inicial. Antes de fazer isso, estava obtendo apenas NAs
  # Mesmo adicionando um parâmetro, ainda obtive muitos NAs nas sublistas

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(comb3), function(i)  tryCatch(f.modelo_bsm_error(y,comb3[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

# Terceiro modelo:
  # Maior tempo de processamento
    # Também posso desconsiderar

start_time <- Sys.time()
modelos_bsm_error_1_ini<-parLapply(cl,1:nrow(comb3), function(i)  tryCatch(f.modelo_bsm_error_1(y,comb3[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

# stop cluster
stopCluster(cl)
showConnections()


## Modelos sem processamento paralelo:
  # Essas linhas também são opção para o caso em que o objeto "grid" apresente algum erro

# Por que ao rodar com apenas 3 parâmetros temos um erro no primeiro modelo?

start_time <- Sys.time()
modelos_bsm_ini <- lapply(1:nrow(comb2), function(i) {
  tryCatch({
    linha <- as.numeric(comb2[i, ])
    f.modelo_bsm(y, linha)
  }, error = function(e) {
    rep(NA, 3)
  })
})
end_time <- Sys.time()
print(end_time - start_time)

start_time <- Sys.time()
modelos_bsm_error_ini <- lapply(1:nrow(comb3), function(i){
  tryCatch({
    linha <-as.numeric(comb3[i, ])
    f.modelo_bsm_error(y,linha)
  }, error = function(e){
    rep(NA,4)
  })
})
end_time <- Sys.time()
print(end_time - start_time)

start_time <- Sys.time()
modelos_bsm_error_1_ini <- lapply(1:nrow(comb3), function(i){
  tryCatch({
    linha <-as.numeric(comb3[i, ])
    f.modelo_bsm_error_1(y,linha)
  }, error = function(e){
    rep(NA,4)
  })
})
end_time <- Sys.time()
print(end_time - start_time)


## Etapa de avaliação:
  # Hiperparâmetros iniciais vs estimados
  # Convergência

a <- cbind(round(exp(comb2),4),
           t(sapply(1:nrow(comb2), function(i) tryCatch( c(round(exp(modelos_bsm_ini[[i]][["fit"]][["par"]]),4),
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


b <- cbind(round(exp(comb3),4),
           t(sapply(1:nrow(comb3), function(i) tryCatch( c(round(exp(modelos_bsm_error_ini[[i]][["fit"]][["par"]]),4),
                                                                modelos_bsm_error_ini[[i]][["fit"]][["convergence"]],
                                                                modelos_bsm_error_ini[[i]][["fit"]][["value"]]),error=function(e) {rep(NA,7)})))
)
colnames(b) <- c("level_ini","slope_ini","seasonal_ini","irregual_ini","sampl_error_ini",
                 "level","slope","seasonal","irregular", "sampl_error",
                 "convergence","log_like")
hist(b$level)
boxplot(b$level)
summary(b$level)

hist(b$slope)
boxplot(b$slope)
summary(b$slope)

hist(b$seasonal)
boxplot(b$seasonal)
summary(b$seasonal)

hist(b$irregular)
boxplot(b$irregular)
summary(b$irregular)

hist(b$sampl_error)
boxplot(b$sampl_error)
summary(b$sampl_error)


### Observação: no momento, o código não está rodando o modelo "c". A partir desse ponto,
    # a rotina foi alterada retirando esse modelo e seus resultados

## Estimação dos modelos após valores iniciais

modelo_bsm<- modelos_bsm_ini[[which(a$log_like==min(a$log_like,na.rm = TRUE))]]
modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]
# modelo_bsm_error_1<- modelos_bsm_error_1_ini[[which(c$log_like==min(c$log_like,na.rm = TRUE))]]


## Verificando a convergência

convergencia<-rbind(modelo_bsm$fit$convergence,
                    modelo_bsm_error$fit$convergence
)

colnames(convergencia)<-c("convergence")


## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm$fit$par),4),NA),
                  c(round(exp(modelo_bsm_error$fit$par),4))
)
row.names(parametros)<-c(
  "BSM",
  "BSM_error"
)
colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error")

# Critérios de informação: AIC e BIC

AIC<-rbind(
  2*(modelo_bsm$fit$value)+2*4,
  2*(modelo_bsm_error$fit$value)+2*5)
colnames(AIC)<-"AIC"

#BIC<-rbind(
#  2*(modelo_bsm$fit$value)+2*4*log(modelo_bsm_error$T),
#  2*(modelo_bsm_error$fit$value)+2*5*log(modelo_bsm_error$T),
#colnames(BIC)<-"BIC"


# Avaliando a matriz hessiana
  # Deve ser positiva definida

all(eigen(modelo_bsm$fit$hessian, only.values = TRUE)$values > 0)
all(eigen(modelo_bsm_error$fit$hessian, only.values = TRUE)$values > 0)
#all(eigen(modelo_bsm_error_1$fit$hessian, only.values = TRUE)$values > 0)

# Diagnosticando os resíduos

lista_modelos<-list( modelo_bsm,
                     modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c(
  "BSM",
  "BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC, BIC)
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
  ts(modelo_bsm_error$cv.original*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)


