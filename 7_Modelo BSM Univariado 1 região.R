################################################################################
##                          MODELO BSM UNIVARIADO                             ##
################################################################################

library(dlm)
library(tidyverse)
library(beepr)
library(parallel)

## Anotações
  # Modelo Sul não rodou o modelo após as estimações iniciais
  # Modelo Norte de Minas não rodou após as estimações iniciais
  # Modelo Vale do Rio Doce não rodou após as estimações iniciais
  # Modelo Central não rodou após as estimações iniciais

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

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")   # Base sem rotação
bh<-baseestr0324$`01-Belo Horizonte`
baserot <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
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
# par_5<-c(0) como o objetivo é um modelo suavizado, não será necessário este 5 parâmetro

## Possibilidades do modelo

# grid <- expand.grid(par_1,par_2,par_3)
grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# No momento, essa repetição foi feita para realizar testes ao longo do código

# comb1 <- expand.grid(par_1, par_2, par_3)
# colnames(comb1) <- c("par_1", "par_2", "par_3")

comb2<-expand.grid(par_1, par_2, par_3, par_4)
colnames(comb2) <- c("par_1", "par_2", "par_3", "par_4")

# comb3<-expand.grid(par_1, par_2, par_3, par_4, par_5)
# colnames(comb3) <- c("par_1", "par_2", "par_3", "par_4","par_5")

## Ajuste do formato dos valores iniciais para o modelo

inicial <- as.matrix(comb2)

## Início do estudo da função

# Definindo o modelo

modelo<- list("fn"=function(params){
  m = dlmModPoly(2) + dlmModTrig(4)
  W = matrix(0,5,5)
  #W[1, 1] <- exp(params[1])
  W[2, 2] <- exp(params[1])
  W[3, 3] <- exp(params[2])
  W[5, 5] <- exp(params[3]) # Alteração para corrigir a dimensão para o último termo sazonal, estava (6,6)
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


### MODELO BH ##################################################################

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
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtbh<-baseal0324$`01-Belo Horizonte` ## Arquivo "cru", saída direta da rotina da base por rotação
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
par_4<-c(0) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend


## Possibilidades do modelo

#grid <- expand.grid(par_1,par_2,par_3)
grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# No momento, essa repetição foi feita para realizar testes ao longo do código

#comb1 <- expand.grid(par_1, par_2, par_3)
#colnames(comb1) <- c("par_1", "par_2", "par_3")

comb2<-expand.grid(par_1, par_2, par_3, par_4)
colnames(comb2) <- c("par_1", "par_2", "par_3", "par_4")

#comb3<-expand.grid(par_1, par_2, par_3, par_4, par_5)
#colnames(comb3) <- c("par_1", "par_2", "par_3", "par_4","par_5")

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

## Modelo com processamento paralelo

## Primeiro e terceiro modelos ocultados em relação à rotina de referência

# Para o segundo modelo, foi necessário criar um 5 parâmetro inicial. Antes de fazer isso, estava obtendo apenas NAs
  # Mesmo adicionando um parâmetro, ainda obtive muitos NAs nas sublistas

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

# stop cluster
stopCluster(cl)
showConnections()


## Etapa de avaliação:
  # Hiperparâmetros iniciais vs estimados
  # Convergência

# Criada nova chamada de objeto
  # Quando criado com código de referência, no modelo smoothing estava ocorrendo a chamada de vetores para cada célula do df
  # Adicionado:
    # Exclui iterações que tiveram resultado  NA 

b_list <- lapply(modelos_bsm_error_ini, function(modelo) {
  if (!is.list(modelo) || is.null(modelo$initial) || is.null(modelo$fit) || is.null(modelo$mod)) {
    return(NULL)
  }
  # Valores iniciais (garante que initial_values seja um vetor válido)
  initial_values <- if (is.data.frame(modelo$initial)) modelo$initial[1, ] else rep(NA, 4)
  
  # Parâmetros estimados
  estimated_values <- if (!is.null(modelo$fit$par)) modelo$fit$par else rep(NA, 4)
  
  # Convergência e log-verossimilhança
  convergence <- if (!is.null(modelo$fit$convergence)) modelo$fit$convergence else NA
  log_like <- if (!is.null(modelo$fit$value)) modelo$fit$value else NA
  
  # Sample error inicial e final
  sampl_error_ini <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  sampl_error <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  
  # Criar um dataframe com os valores extraídos
  data.frame(
    level_ini = initial_values[1], slope_ini = initial_values[2],
    seasonal_ini = initial_values[3], irregular_ini = initial_values[4],
    sampl_error_ini = sampl_error_ini,
    
    level = estimated_values[1], slope = estimated_values[2],
    seasonal = estimated_values[3], irregular = estimated_values[4],
    sampl_error = sampl_error,
    
    convergence = convergence, log_like = log_like
  )
})

# Remove os elementos NULL da lista
b_list <- Filter(Negate(is.null), b_list)

# Combina tudo em um único dataframe, se necessário
b <- do.call(rbind, b_list)

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


# Observações: 
 # Em relação aocódigo de referência 

## Estimação do modelo após valores iniciais

# Conforme referência:
  # modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]
    # Entranto, apesar de escolher a menor log-verossimilhança, por vezes o código escolhia um modelo que não convergiu
    # Adaptação: escolha por convergência = 0 e depois menor verossimilhança

# Filtrei apenas os modelos que convergiram
b_conv<- b[b$convergence == 0, ]

# Depois apliquei o código:

modelo_bsm_error<- modelos_bsm_error_ini[[which(b_conv$log_like==min(b_conv$log_like,na.rm = TRUE))]]


## Verificando a convergência
  # Ainda assim não convergiu

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence")


## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Level","Slope","Seasonal","Irregular") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error")


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

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
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


### MODELO ENTORNO METROPOLITANO ###############################################

rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/03_modelo_bsm_error.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
ent<-baseestr0324$`02-Entorno metropolitano de BH`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtent<-baseal0324$`02-Entorno metropolitano de BH` ## Arquivo "cru", saída direta da rotina da base por rotação
dbent<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/02_params_ent.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- ent$Total.de.ocupados
se_db <- ent$sd_o
cv_db <- ent$CV.ocupados
par_ar_erro <- dbent$parerro_o
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend


## Possibilidades do modelo

#grid <- expand.grid(par_1,par_2,par_3)
grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Criando clusters:

{numCores<-detectCores()
numCores
cl<- makeCluster(numCores-1)
# salva imagem
save.image("partial.Rdata")
# envia dados para cada um dos cluster
clusterEvalQ(cl,{load("partial.Rdata")
  library(dlm)
})
}

## Modelo com processamento paralelo

## Primeiro e terceiro modelos ocultados em relação à rotina de referência

# Para o segundo modelo, foi necessário criar um 5 parâmetro inicial. Antes de fazer isso, estava obtendo apenas NAs
# Mesmo adicionando um parâmetro, ainda obtive muitos NAs nas sublistas

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

# stop cluster
stopCluster(cl)
showConnections()


## Etapa de avaliação:
# Hiperparâmetros iniciais vs estimados
# Convergência

# Criada nova chamada de objeto
# Quando criado com código de referência, no modelo smoothing estava ocorrendo a chamada de vetores para cada célula do df
# Adicionado:
# Exclui iterações que tiveram resultado  NA 

mdent_list <- lapply(modelos_bsm_error_ini, function(modelo) {
  if (!is.list(modelo) || is.null(modelo$initial) || is.null(modelo$fit) || is.null(modelo$mod)) {
    return(NULL)
  }
  # Valores iniciais (garante que initial_values seja um vetor válido)
  initial_values <- if (is.data.frame(modelo$initial)) modelo$initial[1, ] else rep(NA, 4)
  
  # Parâmetros estimados
  estimated_values <- if (!is.null(modelo$fit$par)) modelo$fit$par else rep(NA, 4)
  
  # Convergência e log-verossimilhança
  convergence <- if (!is.null(modelo$fit$convergence)) modelo$fit$convergence else NA
  log_like <- if (!is.null(modelo$fit$value)) modelo$fit$value else NA
  
  # Sample error inicial e final
  sampl_error_ini <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  sampl_error <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  
  # Criar um dataframe com os valores extraídos
  data.frame(
    level_ini = initial_values[1], slope_ini = initial_values[2],
    seasonal_ini = initial_values[3], irregular_ini = initial_values[4],
    sampl_error_ini = sampl_error_ini,
    
    level = estimated_values[1], slope = estimated_values[2],
    seasonal = estimated_values[3], irregular = estimated_values[4],
    sampl_error = sampl_error,
    
    convergence = convergence, log_like = log_like
  )
})

# Remove os elementos NULL da lista
mdent_list <- Filter(Negate(is.null), mdent_list)

# Combina tudo em um único dataframe, se necessário
mdent <- do.call(rbind, mdent_list)

hist(mdent$level)
boxplot(mdent$level)
summary(mdent$level)

hist(mdent$slope)
boxplot(mdent$slope)
summary(mdent$slope)

hist(mdent$seasonal)
boxplot(mdent$seasonal)
summary(mdent$seasonal)

hist(mdent$irregular)
boxplot(mdent$irregular)
summary(mdent$irregular)

hist(mdent$sampl_error)
boxplot(mdent$sampl_error)
summary(mdent$sampl_error)

# Observações: 
# Em relação aocódigo de referência 

## Estimação do modelo após valores iniciais

# Conforme referência:
# modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]
# Entranto, apesar de escolher a menor log-verossimilhança, por vezes o código escolhia um modelo que não convergiu
# Adaptação (teste): escolha por convergência = 0 e depois menor verossimilhança

# Filtrei apenas os modelos que convergiram
mdent_conv<- mdent[mdent$convergence == 0, ]

# Depois apliquei o código:

modelo_bsm_error<- modelos_bsm_error_ini[[which(mdent_conv$log_like==min(mdent_conv$log_like,na.rm = TRUE))]]

## Verificando a convergência
# Ainda assim não convergiu

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence")


## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Level","Slope","Seasonal","Irregular") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error")


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

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC, BIC)
resultados

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))


{par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
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
mtext("Year", side = 1, line = 3)}

{fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topright", legend = c("CV design-based unemployment",
                              "CV signal model-based unemployment",
                              "CV trend model-based unemployment"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Year", side = 1, line = 3)}


### COLAR METROPOLITANO DE BH ##################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/03_modelo_bsm_error.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
col<-baseestr0324$`03-Colar metropolitano de BH`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtcol<-baseal0324$`03-Colar metropolitano de BH` ## Arquivo "cru", saída direta da rotina da base por rotação
dbcol<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/03_params_col.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- col$Total.de.ocupados
se_db <- col$sd_o
cv_db <- col$CV.ocupados
par_ar_erro <- dbcol$parerro_o
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend


## Possibilidades do modelo

#grid <- expand.grid(par_1,par_2,par_3)
grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Criando clusters:

{numCores<-detectCores()
  numCores
  cl<- makeCluster(numCores-1)
  # salva imagem
  save.image("partial.Rdata")
  # envia dados para cada um dos cluster
  clusterEvalQ(cl,{load("partial.Rdata")
    library(dlm)
  })
}

## Modelo com processamento paralelo

## Primeiro e terceiro modelos ocultados em relação à rotina de referência

# Para o segundo modelo, foi necessário criar um 5 parâmetro inicial. Antes de fazer isso, estava obtendo apenas NAs
# Mesmo adicionando um parâmetro, ainda obtive muitos NAs nas sublistas

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

# stop cluster
stopCluster(cl)
showConnections()


## Etapa de avaliação:
# Hiperparâmetros iniciais vs estimados
# Convergência

# Criada nova chamada de objeto
# Quando criado com código de referência, no modelo smoothing estava ocorrendo a chamada de vetores para cada célula do df
# Adicionado:
# Exclui iterações que tiveram resultado  NA 

mdcol_list <- lapply(modelos_bsm_error_ini, function(modelo) {
  if (!is.list(modelo) || is.null(modelo$initial) || is.null(modelo$fit) || is.null(modelo$mod)) {
    return(NULL)
  }
  # Valores iniciais (garante que initial_values seja um vetor válido)
  initial_values <- if (is.data.frame(modelo$initial)) modelo$initial[1, ] else rep(NA, 4)
  
  # Parâmetros estimados
  estimated_values <- if (!is.null(modelo$fit$par)) modelo$fit$par else rep(NA, 4)
  
  # Convergência e log-verossimilhança
  convergence <- if (!is.null(modelo$fit$convergence)) modelo$fit$convergence else NA
  log_like <- if (!is.null(modelo$fit$value)) modelo$fit$value else NA
  
  # Sample error inicial e final
  sampl_error_ini <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  sampl_error <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  
  # Criar um dataframe com os valores extraídos
  data.frame(
    level_ini = initial_values[1], slope_ini = initial_values[2],
    seasonal_ini = initial_values[3], irregular_ini = initial_values[4],
    sampl_error_ini = sampl_error_ini,
    
    level = estimated_values[1], slope = estimated_values[2],
    seasonal = estimated_values[3], irregular = estimated_values[4],
    sampl_error = sampl_error,
    
    convergence = convergence, log_like = log_like
  )
})

# Remove os elementos NULL da lista
mdcol_list <- Filter(Negate(is.null), mdcol_list)

# Combina tudo em um único dataframe, se necessário
mdcol <- do.call(rbind, mdcol_list)

hist(mdcol$level)
boxplot(mdcol$level)
summary(mdcol$level)

hist(mdcol$slope)
boxplot(mdcol$slope)
summary(mdcol$slope)

hist(mdcol$seasonal)
boxplot(mdcol$seasonal)
summary(mdcol$seasonal)

hist(mdcol$irregular)
boxplot(mdcol$irregular)
summary(mdcol$irregular)

hist(mdcol$sampl_error)
boxplot(mdcol$sampl_error)
summary(mdcol$sampl_error)

# Observações: 
# Em relação aocódigo de referência 

## Estimação do modelo após valores iniciais

# Conforme referência:
# modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]
# Entranto, apesar de escolher a menor log-verossimilhança, por vezes o código escolhia um modelo que não convergiu
# Adaptação (teste): escolha por convergência = 0 e depois menor verossimilhança

# Filtrei apenas os modelos que convergiram
mdcol_conv<- mdcol[mdcol$convergence == 0, ] ## Apenas 25 modelos convergiram

# Depois apliquei o código:

modelo_bsm_error<- modelos_bsm_error_ini[[which(mdcol_conv$log_like==min(mdcol_conv$log_like,na.rm = TRUE))]]

## Verificando a convergência
# Ainda assim não convergiu

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence")

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Level","Slope","Seasonal","Irregular") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error"

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

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC, BIC)
resultados # Não convergiu

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))


{par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
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
  mtext("Year", side = 1, line = 3)}

{fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
  plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
  legend("topright", legend = c("CV design-based unemployment",
                                "CV signal model-based unemployment",
                                "CV trend model-based unemployment"),
         lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
  mtext("CV (%)", side = 2, line = 3)
  mtext("Year", side = 1, line = 3)}


### RIDE de Brasília em Minas ##################################################

rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/03_modelo_bsm_error.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
rid<-baseestr0324$`04-RIDE de Brasília em Minas`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtrid<-baseal0324$`04-RIDE de Brasília em Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbrid<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/04_params_rid.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- rid$Total.de.ocupados
se_db <- rid$sd_o
cv_db <- rid$CV.ocupados
par_ar_erro <- dbrid$parerro_o
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend


## Possibilidades do modelo

#grid <- expand.grid(par_1,par_2,par_3)
grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Criando clusters:

{numCores<-detectCores()
  numCores
  cl<- makeCluster(numCores-1)
  # salva imagem
  save.image("partial.Rdata")
  # envia dados para cada um dos cluster
  clusterEvalQ(cl,{load("partial.Rdata")
    library(dlm)
  })
}

## Modelo com processamento paralelo

## Primeiro e terceiro modelos ocultados em relação à rotina de referência

# Para o segundo modelo, foi necessário criar um 5 parâmetro inicial. Antes de fazer isso, estava obtendo apenas NAs
# Mesmo adicionando um parâmetro, ainda obtive muitos NAs nas sublistas

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

# stop cluster
stopCluster(cl)
showConnections()


## Etapa de avaliação:
# Hiperparâmetros iniciais vs estimados
# Convergência

# Criada nova chamada de objeto
# Quando criado com código de referência, no modelo smoothing estava ocorrendo a chamada de vetores para cada célula do df
# Adicionado:
# Exclui iterações que tiveram resultado  NA 

mdrid_list <- lapply(modelos_bsm_error_ini, function(modelo) {
  if (!is.list(modelo) || is.null(modelo$initial) || is.null(modelo$fit) || is.null(modelo$mod)) {
    return(NULL)
  }
  # Valores iniciais (garante que initial_values seja um vetor válido)
  initial_values <- if (is.data.frame(modelo$initial)) modelo$initial[1, ] else rep(NA, 4)
  
  # Parâmetros estimados
  estimated_values <- if (!is.null(modelo$fit$par)) modelo$fit$par else rep(NA, 4)
  
  # Convergência e log-verossimilhança
  convergence <- if (!is.null(modelo$fit$convergence)) modelo$fit$convergence else NA
  log_like <- if (!is.null(modelo$fit$value)) modelo$fit$value else NA
  
  # Sample error inicial e final
  sampl_error_ini <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  sampl_error <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  
  # Criar um dataframe com os valores extraídos
  data.frame(
    level_ini = initial_values[1], slope_ini = initial_values[2],
    seasonal_ini = initial_values[3], irregular_ini = initial_values[4],
    sampl_error_ini = sampl_error_ini,
    
    level = estimated_values[1], slope = estimated_values[2],
    seasonal = estimated_values[3], irregular = estimated_values[4],
    sampl_error = sampl_error,
    
    convergence = convergence, log_like = log_like
  )
})

# Remove os elementos NULL da lista
mdrid_list <- Filter(Negate(is.null), mdrid_list)

# Combina tudo em um único dataframe, se necessário
mdrid <- do.call(rbind, mdrid_list)

hist(mdrid$level)
boxplot(mdrid$level)
summary(mdrid$level)

hist(mdrid$slope)
boxplot(mdrid$slope)
summary(mdrid$slope)

hist(mdrid$seasonal)
boxplot(mdrid$seasonal)
summary(mdrid$seasonal)

hist(mdrid$irregular)
boxplot(mdrid$irregular)
summary(mdrid$irregular)

hist(mdrid$sampl_error)
boxplot(mdrid$sampl_error)
summary(mdrid$sampl_error)

# Observações: 
# Em relação aocódigo de referência 

## Estimação do modelo após valores iniciais

# Conforme referência:
# modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]
# Entranto, apesar de escolher a menor log-verossimilhança, por vezes o código escolhia um modelo que não convergiu
# Adaptação (teste): escolha por convergência = 0 e depois menor verossimilhança

# Filtrei apenas os modelos que convergiram
mdrid_conv<- mdrid[mdrid$convergence == 0, ] ## 43 modelos convergiram

# Depois apliquei o código:

modelo_bsm_error<- modelos_bsm_error_ini[[which(mdrid_conv$log_like==min(mdrid_conv$log_like,na.rm = TRUE))]]

## Verificando a convergência
# Ainda assim não convergiu

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence")

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Level","Slope","Seasonal","Irregular") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error"

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

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC, BIC)
resultados # Convergiu

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))


{par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
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
  mtext("Year", side = 1, line = 3)}

{fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
  plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
  legend("topright", legend = c("CV design-based unemployment",
                                "CV signal model-based unemployment",
                                "CV trend model-based unemployment"),
         lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
  mtext("CV (%)", side = 2, line = 3)
  mtext("Year", side = 1, line = 3)}


### SUL DE MINAS ###############################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/03_modelo_bsm_error.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
sul<-baseestr0324$`05-Sul de Minas`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtsul<-baseal0324$`05-Sul de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbsul<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/05_params_sul.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- sul$Total.de.ocupados
se_db <- sul$sd_o
cv_db <- sul$CV.ocupados
par_ar_erro <- dbsul$parerro_o
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend


## Possibilidades do modelo

#grid <- expand.grid(par_1,par_2,par_3)
grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Criando clusters:

{numCores<-detectCores()
  numCores
  cl<- makeCluster(numCores-1)
  # salva imagem
  save.image("partial.Rdata")
  # envia dados para cada um dos cluster
  clusterEvalQ(cl,{load("partial.Rdata")
    library(dlm)
  })
}

## Modelo com processamento paralelo

## Primeiro e terceiro modelos ocultados em relação à rotina de referência

# Para o segundo modelo, foi necessário criar um 5 parâmetro inicial. Antes de fazer isso, estava obtendo apenas NAs
# Mesmo adicionando um parâmetro, ainda obtive muitos NAs nas sublistas

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

# stop cluster
stopCluster(cl)
showConnections()

## Etapa de avaliação:
# Hiperparâmetros iniciais vs estimados
# Convergência

# Criada nova chamada de objeto
# Quando criado com código de referência, no modelo smoothing estava ocorrendo a chamada de vetores para cada célula do df
# Adicionado:
# Exclui iterações que tiveram resultado  NA 

mdsul_list <- lapply(modelos_bsm_error_ini, function(modelo) {
  if (!is.list(modelo) || is.null(modelo$initial) || is.null(modelo$fit) || is.null(modelo$mod)) {
    return(NULL)
  }
  # Valores iniciais (garante que initial_values seja um vetor válido)
  initial_values <- if (is.data.frame(modelo$initial)) modelo$initial[1, ] else rep(NA, 4)
  
  # Parâmetros estimados
  estimated_values <- if (!is.null(modelo$fit$par)) modelo$fit$par else rep(NA, 4)
  
  # Convergência e log-verossimilhança
  convergence <- if (!is.null(modelo$fit$convergence)) modelo$fit$convergence else NA
  log_like <- if (!is.null(modelo$fit$value)) modelo$fit$value else NA
  
  # Sample error inicial e final
  sampl_error_ini <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  sampl_error <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  
  # Criar um dataframe com os valores extraídos
  data.frame(
    level_ini = initial_values[1], slope_ini = initial_values[2],
    seasonal_ini = initial_values[3], irregular_ini = initial_values[4],
    sampl_error_ini = sampl_error_ini,
    
    level = estimated_values[1], slope = estimated_values[2],
    seasonal = estimated_values[3], irregular = estimated_values[4],
    sampl_error = sampl_error,
    
    convergence = convergence, log_like = log_like
  )
})

# Remove os elementos NULL da lista
mdsul_list <- Filter(Negate(is.null), mdsul_list)

# Combina tudo em um único dataframe, se necessário
mdsul <- do.call(rbind, mdsul_list)

hist(mdsul$level)
boxplot(mdsul$level)
summary(mdsul$level)

hist(mdsul$slope)
boxplot(mdsul$slope)
summary(mdsul$slope)

hist(mdsul$seasonal)
boxplot(mdsul$seasonal)
summary(mdsul$seasonal)

hist(mdsul$irregular)
boxplot(mdsul$irregular)
summary(mdsul$irregular)

hist(mdsul$sampl_error)
boxplot(mdsul$sampl_error)
summary(mdsul$sampl_error)

# Observações: 
# Em relação aocódigo de referência 

## Estimação do modelo após valores iniciais

# Conforme referência:
# modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]
# Entranto, apesar de escolher a menor log-verossimilhança, por vezes o código escolhia um modelo que não convergiu
# Adaptação (teste): escolha por convergência = 0 e depois menor verossimilhança

# Filtrei apenas os modelos que convergiram
mdsul_conv<- mdsul[mdsul$convergence == 0, ] ## 25 modelos convergiram

# Depois apliquei o código:

modelo_bsm_error<- modelos_bsm_error_ini[[which(mdsul_conv$log_like==min(mdsul_conv$log_like,na.rm = TRUE))]]

## Verificando a convergência
# Ainda assim não convergiu

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence")

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Level","Slope","Seasonal","Irregular") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error"

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

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC, BIC)
resultados # Convergiu

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))


{par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
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
  mtext("Year", side = 1, line = 3)}

{fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
  plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
  legend("topright", legend = c("CV design-based unemployment",
                                "CV signal model-based unemployment",
                                "CV trend model-based unemployment"),
         lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
  mtext("CV (%)", side = 2, line = 3)
  mtext("Year", side = 1, line = 3)}


### TRIÂNGULO MINEIRO ##########################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/03_modelo_bsm_error.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
trg<-baseestr0324$`06-Triângulo Mineiro`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dttrg<-baseal0324$`06-Triângulo Mineiro` ## Arquivo "cru", saída direta da rotina da base por rotação
dbtrg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/06_params_trg.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- trg$Total.de.ocupados
se_db <- trg$sd_o
cv_db <- trg$CV.ocupados
par_ar_erro <- dbtrg$parerro_o
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend


## Possibilidades do modelo

#grid <- expand.grid(par_1,par_2,par_3)
grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Criando clusters:

{numCores<-detectCores()
  numCores
  cl<- makeCluster(numCores-1)
  # salva imagem
  save.image("partial.Rdata")
  # envia dados para cada um dos cluster
  clusterEvalQ(cl,{load("partial.Rdata")
    library(dlm)
  })
}

## Modelo com processamento paralelo

## Primeiro e terceiro modelos ocultados em relação à rotina de referência

# Para o segundo modelo, foi necessário criar um 5 parâmetro inicial. Antes de fazer isso, estava obtendo apenas NAs
# Mesmo adicionando um parâmetro, ainda obtive muitos NAs nas sublistas

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

# stop cluster
stopCluster(cl)
showConnections()


## Etapa de avaliação:
# Hiperparâmetros iniciais vs estimados
# Convergência

# Criada nova chamada de objeto
# Quando criado com código de referência, no modelo smoothing estava ocorrendo a chamada de vetores para cada célula do df
# Adicionado:
# Exclui iterações que tiveram resultado  NA 

mdtrg_list <- lapply(modelos_bsm_error_ini, function(modelo) {
  if (!is.list(modelo) || is.null(modelo$initial) || is.null(modelo$fit) || is.null(modelo$mod)) {
    return(NULL)
  }
  # Valores iniciais (garante que initial_values seja um vetor válido)
  initial_values <- if (is.data.frame(modelo$initial)) modelo$initial[1, ] else rep(NA, 4)
  
  # Parâmetros estimados
  estimated_values <- if (!is.null(modelo$fit$par)) modelo$fit$par else rep(NA, 4)
  
  # Convergência e log-verossimilhança
  convergence <- if (!is.null(modelo$fit$convergence)) modelo$fit$convergence else NA
  log_like <- if (!is.null(modelo$fit$value)) modelo$fit$value else NA
  
  # Sample error inicial e final
  sampl_error_ini <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  sampl_error <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  
  # Criar um dataframe com os valores extraídos
  data.frame(
    level_ini = initial_values[1], slope_ini = initial_values[2],
    seasonal_ini = initial_values[3], irregular_ini = initial_values[4],
    sampl_error_ini = sampl_error_ini,
    
    level = estimated_values[1], slope = estimated_values[2],
    seasonal = estimated_values[3], irregular = estimated_values[4],
    sampl_error = sampl_error,
    
    convergence = convergence, log_like = log_like
  )
})

# Remove os elementos NULL da lista
mdtrg_list <- Filter(Negate(is.null), mdtrg_list)

# Combina tudo em um único dataframe, se necessário
mdtrg <- do.call(rbind, mdtrg_list)

hist(mdtrg$level)
boxplot(mdtrg$level)
summary(mdtrg$level)

hist(mdtrg$slope)
boxplot(mdtrg$slope)
summary(mdtrg$slope)

hist(mdtrg$seasonal)
boxplot(mdtrg$seasonal)
summary(mdtrg$seasonal)

hist(mdtrg$irregular)
boxplot(mdtrg$irregular)
summary(mdtrg$irregular)

hist(mdtrg$sampl_error)
boxplot(mdtrg$sampl_error)
summary(mdtrg$sampl_error)

# Observações: 
# Em relação aocódigo de referência 

## Estimação do modelo após valores iniciais

# Conforme referência:
# modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]
# Entranto, apesar de escolher a menor log-verossimilhança, por vezes o código escolhia um modelo que não convergiu
# Adaptação (teste): escolha por convergência = 0 e depois menor verossimilhança

# Filtrei apenas os modelos que convergiram
mdtrg_conv<- mdtrg[mdtrg$convergence == 0, ] ## 49 modelos convergiram

# Depois apliquei o código:

modelo_bsm_error<- modelos_bsm_error_ini[[which(mdtrg_conv$log_like==min(mdtrg_conv$log_like,na.rm = TRUE))]]

## Verificando a convergência
# Ainda assim não convergiu

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence")

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Level","Slope","Seasonal","Irregular") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error"

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

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC, BIC)
resultados # Convergiu

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))


{par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
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
  mtext("Year", side = 1, line = 3)}

{fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
  plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
  legend("topright", legend = c("CV design-based unemployment",
                                "CV signal model-based unemployment",
                                "CV trend model-based unemployment"),
         lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
  mtext("CV (%)", side = 2, line = 3)
  mtext("Year", side = 1, line = 3)}


### ZONA DA MATA ###############################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/03_modelo_bsm_error.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
mat<-baseestr0324$`07-Mata de Minas Gerais`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtmat<-baseal0324$`07-Mata de Minas Gerais` ## Arquivo "cru", saída direta da rotina da base por rotação
dbmat<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/07_params_mat.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- mat$Total.de.ocupados
se_db <- mat$sd_o
cv_db <- mat$CV.ocupados
par_ar_erro <- dbmat$parerro_o
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend


## Possibilidades do modelo

#grid <- expand.grid(par_1,par_2,par_3)
grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Criando clusters:

{numCores<-detectCores()
  numCores
  cl<- makeCluster(numCores-1)
  # salva imagem
  save.image("partial.Rdata")
  # envia dados para cada um dos cluster
  clusterEvalQ(cl,{load("partial.Rdata")
    library(dlm)
  })
}

## Modelo com processamento paralelo

## Primeiro e terceiro modelos ocultados em relação à rotina de referência

# Para o segundo modelo, foi necessário criar um 5 parâmetro inicial. Antes de fazer isso, estava obtendo apenas NAs
# Mesmo adicionando um parâmetro, ainda obtive muitos NAs nas sublistas

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

# stop cluster
stopCluster(cl)
showConnections()


## Etapa de avaliação:
# Hiperparâmetros iniciais vs estimados
# Convergência

# Criada nova chamada de objeto
# Quando criado com código de referência, no modelo smoothing estava ocorrendo a chamada de vetores para cada célula do df
# Adicionado:
# Exclui iterações que tiveram resultado  NA 

mdmat_list <- lapply(modelos_bsm_error_ini, function(modelo) {
  if (!is.list(modelo) || is.null(modelo$initial) || is.null(modelo$fit) || is.null(modelo$mod)) {
    return(NULL)
  }
  # Valores iniciais (garante que initial_values seja um vetor válido)
  initial_values <- if (is.data.frame(modelo$initial)) modelo$initial[1, ] else rep(NA, 4)
  
  # Parâmetros estimados
  estimated_values <- if (!is.null(modelo$fit$par)) modelo$fit$par else rep(NA, 4)
  
  # Convergência e log-verossimilhança
  convergence <- if (!is.null(modelo$fit$convergence)) modelo$fit$convergence else NA
  log_like <- if (!is.null(modelo$fit$value)) modelo$fit$value else NA
  
  # Sample error inicial e final
  sampl_error_ini <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  sampl_error <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  
  # Criar um dataframe com os valores extraídos
  data.frame(
    level_ini = initial_values[1], slope_ini = initial_values[2],
    seasonal_ini = initial_values[3], irregular_ini = initial_values[4],
    sampl_error_ini = sampl_error_ini,
    
    level = estimated_values[1], slope = estimated_values[2],
    seasonal = estimated_values[3], irregular = estimated_values[4],
    sampl_error = sampl_error,
    
    convergence = convergence, log_like = log_like
  )
})

# Remove os elementos NULL da lista
mdmat_list <- Filter(Negate(is.null), mdmat_list)

# Combina tudo em um único dataframe, se necessário
mdmat <- do.call(rbind, mdmat_list)

hist(mdmat$level)
boxplot(mdmat$level)
summary(mdmat$level)

hist(mdmat$slope)
boxplot(mdmat$slope)
summary(mdmat$slope)

hist(mdmat$seasonal)
boxplot(mdmat$seasonal)
summary(mdmat$seasonal)

hist(mdmat$irregular)
boxplot(mdmat$irregular)
summary(mdmat$irregular)

hist(mdmat$sampl_error)
boxplot(mdmat$sampl_error)
summary(mdmat$sampl_error)

# Observações: 
# Em relação aocódigo de referência 

## Estimação do modelo após valores iniciais

# Conforme referência:
# modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]
# Entranto, apesar de escolher a menor log-verossimilhança, por vezes o código escolhia um modelo que não convergiu
# Adaptação (teste): escolha por convergência = 0 e depois menor verossimilhança

# Filtrei apenas os modelos que convergiram
mdmat_conv<- mdmat[mdmat$convergence == 0, ] ## 43 modelos convergiram

# Depois apliquei o código:

modelo_bsm_error<- modelos_bsm_error_ini[[which(mdmat_conv$log_like==min(mdmat_conv$log_like,na.rm = TRUE))]]

## Verificando a convergência
# Ainda assim não convergiu

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence")

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Level","Slope","Seasonal","Irregular") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error"

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

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC, BIC)
resultados # Não Convergiu

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))


{par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
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
  mtext("Year", side = 1, line = 3)}

{fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
  plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
  legend("topright", legend = c("CV design-based unemployment",
                                "CV signal model-based unemployment",
                                "CV trend model-based unemployment"),
         lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
  mtext("CV (%)", side = 2, line = 3)
  mtext("Year", side = 1, line = 3)}


### NORTE DE MINAS GERAIS ######################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/03_modelo_bsm_error.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
nrt<-baseestr0324$`08-Norte de Minas`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtnrt<-baseal0324$`08-Norte de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbnrt<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/08_params_nrt.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- nrt$Total.de.ocupados
se_db <- nrt$sd_o
cv_db <- nrt$CV.ocupados
par_ar_erro <- dbnrt$parerro_o
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend


## Possibilidades do modelo

#grid <- expand.grid(par_1,par_2,par_3)
grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Criando clusters:

{numCores<-detectCores()
  numCores
  cl<- makeCluster(numCores-1)
  # salva imagem
  save.image("partial.Rdata")
  # envia dados para cada um dos cluster
  clusterEvalQ(cl,{load("partial.Rdata")
    library(dlm)
  })
}

## Modelo com processamento paralelo

## Primeiro e terceiro modelos ocultados em relação à rotina de referência

# Para o segundo modelo, foi necessário criar um 5 parâmetro inicial. Antes de fazer isso, estava obtendo apenas NAs
# Mesmo adicionando um parâmetro, ainda obtive muitos NAs nas sublistas

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

# stop cluster
stopCluster(cl)
showConnections()


## Etapa de avaliação:
# Hiperparâmetros iniciais vs estimados
# Convergência

# Criada nova chamada de objeto
# Quando criado com código de referência, no modelo smoothing estava ocorrendo a chamada de vetores para cada célula do df
# Adicionado:
# Exclui iterações que tiveram resultado  NA 

mdnrt_list <- lapply(modelos_bsm_error_ini, function(modelo) {
  if (!is.list(modelo) || is.null(modelo$initial) || is.null(modelo$fit) || is.null(modelo$mod)) {
    return(NULL)
  }
  # Valores iniciais (garante que initial_values seja um vetor válido)
  initial_values <- if (is.data.frame(modelo$initial)) modelo$initial[1, ] else rep(NA, 4)
  
  # Parâmetros estimados
  estimated_values <- if (!is.null(modelo$fit$par)) modelo$fit$par else rep(NA, 4)
  
  # Convergência e log-verossimilhança
  convergence <- if (!is.null(modelo$fit$convergence)) modelo$fit$convergence else NA
  log_like <- if (!is.null(modelo$fit$value)) modelo$fit$value else NA
  
  # Sample error inicial e final
  sampl_error_ini <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  sampl_error <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  
  # Criar um dataframe com os valores extraídos
  data.frame(
    level_ini = initial_values[1], slope_ini = initial_values[2],
    seasonal_ini = initial_values[3], irregular_ini = initial_values[4],
    sampl_error_ini = sampl_error_ini,
    
    level = estimated_values[1], slope = estimated_values[2],
    seasonal = estimated_values[3], irregular = estimated_values[4],
    sampl_error = sampl_error,
    
    convergence = convergence, log_like = log_like
  )
})

# Remove os elementos NULL da lista
mdnrt_list <- Filter(Negate(is.null), mdnrt_list)

# Combina tudo em um único dataframe, se necessário
mdnrt <- do.call(rbind, mdnrt_list)

hist(mdnrt$level)
boxplot(mdnrt$level)
summary(mdnrt$level)

hist(mdnrt$slope)
boxplot(mdnrt$slope)
summary(mdnrt$slope)

hist(mdnrt$seasonal)
boxplot(mdnrt$seasonal)
summary(mdnrt$seasonal)

hist(mdnrt$irregular)
boxplot(mdnrt$irregular)
summary(mdnrt$irregular)

hist(mdnrt$sampl_error)
boxplot(mdnrt$sampl_error)
summary(mdnrt$sampl_error)

# Observações: 
# Em relação aocódigo de referência 

## Estimação do modelo após valores iniciais

# Conforme referência:
# modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]
# Entranto, apesar de escolher a menor log-verossimilhança, por vezes o código escolhia um modelo que não convergiu
# Adaptação (teste): escolha por convergência = 0 e depois menor verossimilhança

# Filtrei apenas os modelos que convergiram
mdnrt_conv<- mdnrt[mdnrt$convergence == 0, ] ## 47 modelos convergiram

mdnrt_conv<-as.data.frame(mdnrt_conv)

# Depois apliquei o código:

modelo_bsm_error<- modelos_bsm_error_ini[[which(mdnrt_conv$log_like==min(mdnrt_conv$log_like,na.rm = TRUE))]]

## Verificando a convergência
# Ainda assim não convergiu

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence")

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Level","Slope","Seasonal","Irregular") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error"

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

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC, BIC)
resultados # Não Convergiu

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))


{par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
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
  mtext("Year", side = 1, line = 3)}

{fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
  plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
  legend("topright", legend = c("CV design-based unemployment",
                                "CV signal model-based unemployment",
                                "CV trend model-based unemployment"),
         lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
  mtext("CV (%)", side = 2, line = 3)
  mtext("Year", side = 1, line = 3)}


### VALE DO RIO DOCE ###########################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/03_modelo_bsm_error.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
vl<-baseestr0324$`09-Vale do Rio Doce`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtvl<-baseal0324$`09-Vale do Rio Doce` ## Arquivo "cru", saída direta da rotina da base por rotação
dbvl<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/09_params_rio.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- vl$Total.de.ocupados
se_db <- vl$sd_o
cv_db <- vl$CV.ocupados
par_ar_erro <- dbvl$parerro_o
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend


## Possibilidades do modelo

#grid <- expand.grid(par_1,par_2,par_3)
grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Criando clusters:

{numCores<-detectCores()
  numCores
  cl<- makeCluster(numCores-1)
  # salva imagem
  save.image("partial.Rdata")
  # envia dados para cada um dos cluster
  clusterEvalQ(cl,{load("partial.Rdata")
    library(dlm)
  })
}

## Modelo com processamento paralelo

## Primeiro e terceiro modelos ocultados em relação à rotina de referência

# Para o segundo modelo, foi necessário criar um 5 parâmetro inicial. Antes de fazer isso, estava obtendo apenas NAs
# Mesmo adicionando um parâmetro, ainda obtive muitos NAs nas sublistas

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

# stop cluster
stopCluster(cl)
showConnections()


## Etapa de avaliação:
# Hiperparâmetros iniciais vs estimados
# Convergência

# Criada nova chamada de objeto
# Quando criado com código de referência, no modelo smoothing estava ocorrendo a chamada de vetores para cada célula do df
# Adicionado:
# Exclui iterações que tiveram resultado  NA 

mdvl_list <- lapply(modelos_bsm_error_ini, function(modelo) {
  if (!is.list(modelo) || is.null(modelo$initial) || is.null(modelo$fit) || is.null(modelo$mod)) {
    return(NULL)
  }
  # Valores iniciais (garante que initial_values seja um vetor válido)
  initial_values <- if (is.data.frame(modelo$initial)) modelo$initial[1, ] else rep(NA, 4)
  
  # Parâmetros estimados
  estimated_values <- if (!is.null(modelo$fit$par)) modelo$fit$par else rep(NA, 4)
  
  # Convergência e log-verossimilhança
  convergence <- if (!is.null(modelo$fit$convergence)) modelo$fit$convergence else NA
  log_like <- if (!is.null(modelo$fit$value)) modelo$fit$value else NA
  
  # Sample error inicial e final
  sampl_error_ini <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  sampl_error <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  
  # Criar um dataframe com os valores extraídos
  data.frame(
    level_ini = initial_values[1], slope_ini = initial_values[2],
    seasonal_ini = initial_values[3], irregular_ini = initial_values[4],
    sampl_error_ini = sampl_error_ini,
    
    level = estimated_values[1], slope = estimated_values[2],
    seasonal = estimated_values[3], irregular = estimated_values[4],
    sampl_error = sampl_error,
    
    convergence = convergence, log_like = log_like
  )
})

# Remove os elementos NULL da lista
mdvl_list <- Filter(Negate(is.null), mdvl_list)

# Combina tudo em um único dataframe, se necessário
mdvl <- do.call(rbind, mdvl_list)

hist(mdvl$level)
boxplot(mdvl$level)
summary(mdvl$level)

hist(mdvl$slope)
boxplot(mdvl$slope)
summary(mdvl$slope)

hist(mdvl$seasonal)
boxplot(mdvl$seasonal)
summary(mdvl$seasonal)

hist(mdvl$irregular)
boxplot(mdvl$irregular)
summary(mdvl$irregular)

hist(mdvl$sampl_error)
boxplot(mdvl$sampl_error)
summary(mdvl$sampl_error)

# Observações: 
# Em relação aocódigo de referência 

## Estimação do modelo após valores iniciais

# Conforme referência:
# modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]
# Entranto, apesar de escolher a menor log-verossimilhança, por vezes o código escolhia um modelo que não convergiu
# Adaptação (teste): escolha por convergência = 0 e depois menor verossimilhança

# Filtrei apenas os modelos que convergiram
mdvl_conv<- mdvl[mdvl$convergence == 0, ] ## 38 modelos convergiram

# Depois apliquei o código:

modelo_bsm_error<- modelos_bsm_error_ini[[which(mdvl_conv$log_like==min(mdvl_conv$log_like,na.rm = TRUE))]]

## Verificando a convergência
# Ainda assim não convergiu

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence")

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Level","Slope","Seasonal","Irregular") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error"

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

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC, BIC)
resultados # Não Convergiu

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))


{par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
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
  mtext("Year", side = 1, line = 3)}

{fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
  plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
  legend("topright", legend = c("CV design-based unemployment",
                                "CV signal model-based unemployment",
                                "CV trend model-based unemployment"),
         lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
  mtext("CV (%)", side = 2, line = 3)
  mtext("Year", side = 1, line = 3)}


### CENTRAL ####################################################################
rm(list = ls())

## Funções

source("data/funcoes/01_funcoes_pseudo_erro.R")
source("data/funcoes/03_modelo_bsm_error.R")
source("data/funcoes/05_teste_H.R")
source("data/funcoes/07_teste_bsm_error.R")

## Carregando bases e definindo objeto para BH

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
cen<-baseestr0324$`10-Central`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtcen<-baseal0324$`10-Central` ## Arquivo "cru", saída direta da rotina da base por rotação
dbcen<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/10_params_cen.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- cen$Total.de.ocupados
se_db <- cen$sd_o
cv_db <- cen$CV.ocupados
par_ar_erro <- dbcen$parerro_o
#plot(y, type="l")
#plot(cv_db*100, type="l")

## Estipulando parâmetros iniciais:
# Conforme recomendação, estipulando inicialmente seq(1)
# Na rotina de referência, têm-se: seq(-6,6,3) para os objs par_1, par_2 e par_3

par_1<-seq(-6,6,3)
par_2<-seq(-6,6,3)
par_3<-seq(-6,6,3)
par_4<-c(0) # estava: c(0)
#par_5<-seq(0) # Ocultado para modelo smooth trend


## Possibilidades do modelo

#grid <- expand.grid(par_1,par_2,par_3)
grid_error <- expand.grid(par_1,par_2,par_3,par_4)

# Criando clusters:

{numCores<-detectCores()
  numCores
  cl<- makeCluster(numCores-1)
  # salva imagem
  save.image("partial.Rdata")
  # envia dados para cada um dos cluster
  clusterEvalQ(cl,{load("partial.Rdata")
    library(dlm)
  })
}

## Modelo com processamento paralelo

## Primeiro e terceiro modelos ocultados em relação à rotina de referência

# Para o segundo modelo, foi necessário criar um 5 parâmetro inicial. Antes de fazer isso, estava obtendo apenas NAs
# Mesmo adicionando um parâmetro, ainda obtive muitos NAs nas sublistas

start_time <- Sys.time()
modelos_bsm_error_ini<-parLapply(cl,1:nrow(grid_error), function(i)  tryCatch(f.teste_bsm_error(y,grid_error[i,]),error=function(e) {rep(NA,4)}))
end_time <- Sys.time()
end_time - start_time

# stop cluster
stopCluster(cl)
showConnections()


## Etapa de avaliação:
# Hiperparâmetros iniciais vs estimados
# Convergência

# Criada nova chamada de objeto
# Quando criado com código de referência, no modelo smoothing estava ocorrendo a chamada de vetores para cada célula do df
# Adicionado:
# Exclui iterações que tiveram resultado  NA 

mdcen_list <- lapply(modelos_bsm_error_ini, function(modelo) {
  if (!is.list(modelo) || is.null(modelo$initial) || is.null(modelo$fit) || is.null(modelo$mod)) {
    return(NULL)
  }
  # Valores iniciais (garante que initial_values seja um vetor válido)
  initial_values <- if (is.data.frame(modelo$initial)) modelo$initial[1, ] else rep(NA, 4)
  
  # Parâmetros estimados
  estimated_values <- if (!is.null(modelo$fit$par)) modelo$fit$par else rep(NA, 4)
  
  # Convergência e log-verossimilhança
  convergence <- if (!is.null(modelo$fit$convergence)) modelo$fit$convergence else NA
  log_like <- if (!is.null(modelo$fit$value)) modelo$fit$value else NA
  
  # Sample error inicial e final
  sampl_error_ini <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  sampl_error <- if (!is.null(modelo$mod$V)) modelo$mod$V else NA
  
  # Criar um dataframe com os valores extraídos
  data.frame(
    level_ini = initial_values[1], slope_ini = initial_values[2],
    seasonal_ini = initial_values[3], irregular_ini = initial_values[4],
    sampl_error_ini = sampl_error_ini,
    
    level = estimated_values[1], slope = estimated_values[2],
    seasonal = estimated_values[3], irregular = estimated_values[4],
    sampl_error = sampl_error,
    
    convergence = convergence, log_like = log_like
  )
})

# Remove os elementos NULL da lista
mdcen_list <- Filter(Negate(is.null), mdcen_list)

# Combina tudo em um único dataframe, se necessário
mdcen <- do.call(rbind, mdcen_list)

hist(mdcen$level)
boxplot(mdcen$level)
summary(mdcen$level)

hist(mdcen$slope)
boxplot(mdcen$slope)
summary(mdcen$slope)

hist(mdcen$seasonal)
boxplot(mdcen$seasonal)
summary(mdcen$seasonal)

hist(mdcen$irregular)
boxplot(mdcen$irregular)
summary(mdcen$irregular)

hist(mdcen$sampl_error)
boxplot(mdcen$sampl_error)
summary(mdcen$sampl_error)

# Observações: 
# Em relação aocódigo de referência 

## Estimação do modelo após valores iniciais

# Conforme referência:
# modelo_bsm_error<- modelos_bsm_error_ini[[which(b$log_like==min(b$log_like,na.rm = TRUE))]]
# Entranto, apesar de escolher a menor log-verossimilhança, por vezes o código escolhia um modelo que não convergiu
# Adaptação (teste): escolha por convergência = 0 e depois menor verossimilhança

# Filtrei apenas os modelos que convergiram
mdcen_conv<- mdcen[mdcen$convergence == 0, ] ## 33 modelos convergiram

# Depois apliquei o código:

modelo_bsm_error<- modelos_bsm_error_ini[[which(mdcen_conv$log_like==min(mdcen_conv$log_like,na.rm = TRUE))]]

## Verificando a convergência
# Ainda assim não convergiu

convergencia<-rbind(modelo_bsm_error$fit$convergence)

colnames(convergencia)<-c("convergence")

## Parâmetros estimados:

parametros<-rbind(c(round(exp(modelo_bsm_error$fit$par),4)))

row.names(parametros)<-c("BSM_error")

colnames(parametros)<-c("Level","Slope","Seasonal","Irregular") # Estava: colnames(parametros)<-c("Level","Slope","Seasonal","Irregular","Sampling Error"

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

# Diagnosticando os resíduos

lista_modelos<-list(modelo_bsm_error)
testes<-sapply(lista_modelos, function(modelo) c(round(shapiro.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]])[["p.value"]],4), # considerar depois da 13ª observação - d=13,
                                                 round((Box.test(modelo[["res"]][modelo[["d"]]:modelo[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],4),
                                                 teste_H(modelo[["res"]][modelo[["d"]]:modelo[["T"]]]))
)
testes<-t(testes)
row.names(testes)<-c("BSM_error")
colnames(testes)<-c("Shapiro","Box","H")

resultados<-cbind(convergencia,parametros,testes, AIC, BIC)
resultados # Não Convergiu

#save.image(paste("results/modelo3_D_",RG,"_1T2023.RData",sep=""))
#write.csv(resultados,paste("results/modelo3_D_resultados_",RG,"_1T2023.csv",sep=""))


{par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
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
  mtext("Year", side = 1, line = 3)}

{fig.cv_1<- window(ts.union(
  ts(modelo_bsm_error$cv.original*100,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.signal,start = 2012,frequency=4),
  ts(modelo_bsm_error$cv.trend,start = 2012,frequency=4)),start=c(2013,3))
  plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
  legend("topright", legend = c("CV design-based unemployment",
                                "CV signal model-based unemployment",
                                "CV trend model-based unemployment"),
         lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
  mtext("CV (%)", side = 2, line = 3)
  mtext("Year", side = 1, line = 3)}