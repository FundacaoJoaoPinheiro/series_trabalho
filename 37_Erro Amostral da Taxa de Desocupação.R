################################################################################
##            MODELO ERRO AMOSTRAL PARA TAXA DE DESOCUPAÇÃO                   ##
################################################################################

library(survey)
library(srvyr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(forecast)
library(writexl)
library(nleqslv)

options(scipen=999)

## Leitura da base de dados:
rm(list = ls())

base <- readRDS("C:/FJP2425/Programacao/data/dadosalin_txdesoc_8reg.rds")

## Utilizando conjunto de funções já prontas para o script

source("data/funcoes/01_funcoes_pseudo_erro.R")


### 01-BELO HORIZONTE###########################################################

dbbh<-base[["01-Belo Horizonte"]]

## Definindo variáveis adicionais
  # Para o arquivo gabarito o cálculo de K é feito conforme: (ncol(dbbh)-1)/2
    # Foi necessário ajustar conforme a base
      # Por mais que o arqv baserot0324 seja semelhante ao baseMG_k, ele contém os erros padrão para cada grupo de rotação

colnames(dbbh)
t = c(1:nrow(dbbh))
lags = 24
T = nrow(dbbh)
K = (ncol(dbbh)-1)/6   # Aqui é o número de grupos

# Valor médio dos painéis

dbbh$media_txdesoc = dbbh %>%  select(starts_with("txdesoc")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)

View(dbbh) #Verificação

# Matriz de pseudo erros

dbbh$pseudo1_txdesoc = dbbh$txdesoc_1 - dbbh$media_txdesoc
dbbh$pseudo2_txdesoc = dbbh$txdesoc_2 - dbbh$media_txdesoc
dbbh$pseudo3_txdesoc = dbbh$txdesoc_3 - dbbh$media_txdesoc
dbbh$pseudo4_txdesoc = dbbh$txdesoc_4 - dbbh$media_txdesoc
dbbh$pseudo5_txdesoc = dbbh$txdesoc_5 - dbbh$media_txdesoc

View(dbbh)

# Taxa da desocupação: Autocov; FAC e FACP

lag = c(0:24)
clc_tx_bh=as.data.frame(lag)
head(clc_tx_bh)

## Calculo autocov dos pseudoerros (Ch)

clc_tx_bh$Ch1 = Pcov2(dbbh$pseudo1_txdesoc, lag = lags + 1)
clc_tx_bh$Ch2 = Pcov2(dbbh$pseudo2_txdesoc, lag = lags + 1)
clc_tx_bh$Ch3 = Pcov2(dbbh$pseudo3_txdesoc, lag = lags + 1)
clc_tx_bh$Ch4 = Pcov2(dbbh$pseudo4_txdesoc, lag = lags + 1)
clc_tx_bh$Ch5 = Pcov2(dbbh$pseudo5_txdesoc, lag = lags + 1)

View(clc_tx_bh)

# Soma das autocovs dos pseudo-erros:

clc_tx_bh$SomaChk = clc_tx_bh$Ch1 + clc_tx_bh$Ch2 +clc_tx_bh$Ch3 +clc_tx_bh$Ch4 +clc_tx_bh$Ch5
clc_tx_bh$autocov = clc_tx_bh$SomaChk/(K^2-K)

# FAC
clc_tx_bh$fac = clc_tx_bh$SomaChk/clc_tx_bh$SomaChk[1]
View(clc_tx_bh)

# FACP
clc_tx_bh$facp = 0 
clc_tx_bh$facp[2:25] = facp_acf(clc_tx_bh$fac,lags) # Função retirada de source("data/funcoes/01_funcoes_pseudtx_erro.R")
View(clc_tx_bh)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_tx_bh$esttest = nrow(dbbh)*clc_tx_bh$facp ^ 2 
clc_tx_bh$pvalor = teste(clc_tx_bh$facp,nrow(dbbh)) # ver função teste
View(clc_tx_bh)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_bh$lag, clc_tx_bh$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_bh$lag[2:25], clc_tx_bh$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}

### Parâmetros para os modelos

rho1_tx_bh <- clc_tx_bh$fac[2]
rho2_tx_bh <- clc_tx_bh$fac[3]

#AR(1)

phi1_ar1_txbh = clc_tx_bh$fac[lag==1]

par_ar1_txbh<- data.frame(phi1_ar1_txbh)

### MA(1):

theta1_ma1_txbh <- (1 - sqrt(1 - 4 * rho1_tx_bh^2)) / (2 * rho1_tx_bh)

par_ma1_txbh<-data.frame(theta1_ma1_txbh)

### AR(2)

phi1_ar2_txbh <- (rho1_tx_bh - rho1_tx_bh * rho2_tx_bh) / (1 - rho1_tx_bh^2)
phi2_ar2_txbh <- (rho2_tx_bh - rho1_tx_bh^2) / (1 - rho1_tx_bh^2)

par_ar2_txbh<-data.frame(phi1_ar2_txbh,phi2_ar2_txbh)

### MA(2)

rho1 <- rho1_tx_bh
rho2 <- rho2_tx_bh
sistema_eq <- function(theta) {
  theta1 <- theta[1]
  theta2 <- theta[2]
  eq1 <- (-theta1 * (1 - theta2)) / (1 + theta1^2 + theta2^2) - rho1
  eq2 <- (-theta2) / (1 + theta1^2 + theta2^2) - rho2
  return(c(eq1, eq2))
}
theta_ini <- c(0, 0)
solucao <- nleqslv(theta_ini, sistema_eq)
solucao$x

par_ma2_txbh<-data.frame("theta1_ma2_txbh"=solucao$x[1],"theta2_ma2_txbh"=solucao$x[2])

### ARMA (1,1)

phi1_arma11_txbh <- rho2_tx_bh/rho1_tx_bh

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  resultado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- resultado_theta1$root
  return(theta1)
}

rho1 <- rho1_tx_bh
phi1 <- phi1_arma11_txbh

theta1_arma11_txbh <- arma11_theta1(rho1, phi1)

par_arma11_txbh <-data.frame(phi1_arma11_txbh,theta1_arma11_txbh)

params_txbh <- list("dbbh"=dbbh,"calculos_taxa_bh"=  clc_tx_bh, 
                  "taxamod_ar1" = par_ar1_txbh, "taxamod_ar2"=par_ar2_txbh,
                  "taxamod_ma1"=par_ma1_txbh,"taxamod_ma2"=par_ma2_txbh, "taxamod_arma11"=par_arma11_txbh)

saveRDS(params_txbh,file = "C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/01_params_taxa_bh.rds")


### 02-COLAR E ENTORNO METROPOLITANO DE BELO HORIZONTE##########################

dbent<-base$`02-Colar e Entorno Metropolitano de BH`

colnames(dbent)
t = c(1:nrow(dbent))
lags = 24
T = nrow(dbent)
K = (ncol(dbent)-1)/6   # Aqui é o número de grupos

# Valor médio dos painéis

dbent$media_txdesoc = dbent %>%  select(starts_with("txdesoc")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)

View(dbent) #Verificação

# Matriz de pseudo erros

dbent$pseudo1_txdesoc = dbent$txdesoc_1 - dbent$media_txdesoc
dbent$pseudo2_txdesoc = dbent$txdesoc_2 - dbent$media_txdesoc
dbent$pseudo3_txdesoc = dbent$txdesoc_3 - dbent$media_txdesoc
dbent$pseudo4_txdesoc = dbent$txdesoc_4 - dbent$media_txdesoc
dbent$pseudo5_txdesoc = dbent$txdesoc_5 - dbent$media_txdesoc

View(dbent)

# Taxa da desocupação: Autocov; FAC e FACP

lag = c(0:24)
clc_tx_ent=as.data.frame(lag)
head(clc_tx_ent)

## Calculo autocov dos pseudoerros (Ch)

clc_tx_ent$Ch1 = Pcov2(dbent$pseudo1_txdesoc, lag = lags + 1)
clc_tx_ent$Ch2 = Pcov2(dbent$pseudo2_txdesoc, lag = lags + 1)
clc_tx_ent$Ch3 = Pcov2(dbent$pseudo3_txdesoc, lag = lags + 1)
clc_tx_ent$Ch4 = Pcov2(dbent$pseudo4_txdesoc, lag = lags + 1)
clc_tx_ent$Ch5 = Pcov2(dbent$pseudo5_txdesoc, lag = lags + 1)

View(clc_tx_ent)

# Soma das autocovs dos pseudo-erros:

clc_tx_ent$SomaChk = clc_tx_ent$Ch1 + clc_tx_ent$Ch2 +clc_tx_ent$Ch3 +clc_tx_ent$Ch4 +clc_tx_ent$Ch5
clc_tx_ent$autocov = clc_tx_ent$SomaChk/(K^2-K)

# FAC
clc_tx_ent$fac = clc_tx_ent$SomaChk/clc_tx_ent$SomaChk[1]
View(clc_tx_ent)

# FACP
clc_tx_ent$facp = 0 
clc_tx_ent$facp[2:25] = facp_acf(clc_tx_ent$fac,lags) # Função retirada de source("data/funcoes/01_funcoes_pseudtx_erro.R")
View(clc_tx_ent)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_tx_ent$esttest = nrow(dbent)*clc_tx_ent$facp ^ 2 
clc_tx_ent$pvalor = teste(clc_tx_ent$facp,nrow(dbent)) # ver função teste
View(clc_tx_ent)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_ent$lag, clc_tx_ent$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_ent$lag[2:25], clc_tx_ent$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}

### Parâmetros para os modelos

rho1_tx_ent <- clc_tx_ent$fac[2]
rho2_tx_ent <- clc_tx_ent$fac[3]

#AR(1)

phi1_ar1_txent = clc_tx_ent$fac[lag==1]

par_ar1_txent<- data.frame(phi1_ar1_txent)

### MA(1):

theta1_ma1_txent <- (1 - sqrt(1 - 4 * rho1_tx_ent^2)) / (2 * rho1_tx_ent)

par_ma1_txent<-data.frame(theta1_ma1_txent)

### AR(2)

phi1_ar2_txent <- (rho1_tx_ent - rho1_tx_ent * rho2_tx_ent) / (1 - rho1_tx_ent^2)
phi2_ar2_txent <- (rho2_tx_ent - rho1_tx_ent^2) / (1 - rho1_tx_ent^2)

par_ar2_txent<-data.frame(phi1_ar2_txent,phi2_ar2_txent)

### MA(2)

rho1 <- rho1_tx_ent
rho2 <- rho2_tx_ent
sistema_eq <- function(theta) {
  theta1 <- theta[1]
  theta2 <- theta[2]
  eq1 <- (-theta1 * (1 - theta2)) / (1 + theta1^2 + theta2^2) - rho1
  eq2 <- (-theta2) / (1 + theta1^2 + theta2^2) - rho2
  return(c(eq1, eq2))
}
theta_ini <- c(0, 0)
solucao <- nleqslv(theta_ini, sistema_eq)
solucao$x

par_ma2_txent<-data.frame("theta1_ma2_txent"=solucao$x[1],"theta2_ma2_txent"=solucao$x[2])

### ARMA (1,1)

phi1_arma11_txent <- rho2_tx_ent/rho1_tx_ent

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  resultado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- resultado_theta1$root
  return(theta1)
}

rho1 <- rho1_tx_ent
phi1 <- phi1_arma11_txent

theta1_arma11_txent <- arma11_theta1(rho1, phi1)

par_arma11_txent <-data.frame(phi1_arma11_txent,theta1_arma11_txent)

params_txent <- list("dbent"=dbent,"calculos_taxa_ent"=  clc_tx_ent, 
                    "taxamod_ar1" = par_ar1_txent, "taxamod_ar2"=par_ar2_txent,
                    "taxamod_ma1"=par_ma1_txent,"taxamod_ma2"=par_ma2_txent, "taxamod_arma11"=par_arma11_txent)

saveRDS(params_txent,file = "C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/02_params_taxa_ent.rds")


### 03-SUL DE MINAS ############################################################

dbsul<-base$`03-Sul de Minas`

## Definindo variáveis adicionais
# Para o arquivo gabarito o cálculo de K é feito conforme: (ncol(dbbh)-1)/2
# Foi necessário ajustar conforme a base
# Por mais que o arqv baserot0324 seja semelhante ao baseMG_k, ele contém os erros padrão para cada grupo de rotação

colnames(dbsul)
t = c(1:nrow(dbsul))
lags = 24
T = nrow(dbsul)
K = (ncol(dbsul)-1)/6   # Aqui é o número de grupos

# Valor médio dos painéis

dbsul$media_txdesoc = dbsul %>%  select(starts_with("txdesoc")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)

View(dbsul) #Verificação

# Matriz de pseudo erros

dbsul$pseudo1_txdesoc = dbsul$txdesoc_1 - dbsul$media_txdesoc
dbsul$pseudo2_txdesoc = dbsul$txdesoc_2 - dbsul$media_txdesoc
dbsul$pseudo3_txdesoc = dbsul$txdesoc_3 - dbsul$media_txdesoc
dbsul$pseudo4_txdesoc = dbsul$txdesoc_4 - dbsul$media_txdesoc
dbsul$pseudo5_txdesoc = dbsul$txdesoc_5 - dbsul$media_txdesoc

View(dbsul)

# Taxa da desocupação: Autocov; FAC e FACP

lag = c(0:24)
clc_tx_sul=as.data.frame(lag)
head(clc_tx_sul)

## Calculo autocov dos pseudoerros (Ch)

clc_tx_sul$Ch1 = Pcov2(dbsul$pseudo1_txdesoc, lag = lags + 1)
clc_tx_sul$Ch2 = Pcov2(dbsul$pseudo2_txdesoc, lag = lags + 1)
clc_tx_sul$Ch3 = Pcov2(dbsul$pseudo3_txdesoc, lag = lags + 1)
clc_tx_sul$Ch4 = Pcov2(dbsul$pseudo4_txdesoc, lag = lags + 1)
clc_tx_sul$Ch5 = Pcov2(dbsul$pseudo5_txdesoc, lag = lags + 1)

View(clc_tx_sul)

# Soma das autocovs dos pseudo-erros:

clc_tx_sul$SomaChk = clc_tx_sul$Ch1 + clc_tx_sul$Ch2 +clc_tx_sul$Ch3 +clc_tx_sul$Ch4 +clc_tx_sul$Ch5
clc_tx_sul$autocov = clc_tx_sul$SomaChk/(K^2-K)

# FAC
clc_tx_sul$fac = clc_tx_sul$SomaChk/clc_tx_sul$SomaChk[1]
View(clc_tx_sul)

# FACP
clc_tx_sul$facp = 0 
clc_tx_sul$facp[2:25] = facp_acf(clc_tx_sul$fac,lags) # Função retirada de source("data/funcoes/01_funcoes_pseudtx_erro.R")
View(clc_tx_sul)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_tx_sul$esttest = nrow(dbsul)*clc_tx_sul$facp ^ 2 
clc_tx_sul$pvalor = teste(clc_tx_sul$facp,nrow(dbsul)) # ver função teste
View(clc_tx_sul)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_sul$lag, clc_tx_sul$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_sul$lag[2:25], clc_tx_sul$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}

### Parâmetros para os modelos

rho1_tx_sul <- clc_tx_sul$fac[2]
rho2_tx_sul <- clc_tx_sul$fac[3]

#AR(1)

phi1_ar1_txsul = clc_tx_sul$fac[lag==1]

par_ar1_txsul<- data.frame(phi1_ar1_txsul)

### MA(1):

theta1_ma1_txsul <- (1 - sqrt(1 - 4 * rho1_tx_sul^2)) / (2 * rho1_tx_sul)

par_ma1_txsul<-data.frame(theta1_ma1_txsul)

### AR(2)

phi1_ar2_txsul <- (rho1_tx_sul - rho1_tx_sul * rho2_tx_sul) / (1 - rho1_tx_sul^2)
phi2_ar2_txsul <- (rho2_tx_sul - rho1_tx_sul^2) / (1 - rho1_tx_sul^2)

par_ar2_txsul<-data.frame(phi1_ar2_txsul,phi2_ar2_txsul)

### MA(2)

rho1 <- rho1_tx_sul
rho2 <- rho2_tx_sul
sistema_eq <- function(theta) {
  theta1 <- theta[1]
  theta2 <- theta[2]
  eq1 <- (-theta1 * (1 - theta2)) / (1 + theta1^2 + theta2^2) - rho1
  eq2 <- (-theta2) / (1 + theta1^2 + theta2^2) - rho2
  return(c(eq1, eq2))
}
theta_ini <- c(0, 0)
solucao <- nleqslv(theta_ini, sistema_eq)
solucao$x

par_ma2_txsul<-data.frame("theta1_ma2_txsul"=solucao$x[1],"theta2_ma2_txsul"=solucao$x[2])

### ARMA (1,1)

phi1_arma11_txsul <- rho2_tx_sul/rho1_tx_sul

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  resultado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- resultado_theta1$root
  return(theta1)
}

rho1 <- rho1_tx_sul
phi1 <- phi1_arma11_txsul

theta1_arma11_txsul <- arma11_theta1(rho1, phi1)

par_arma11_txsul <-data.frame(phi1_arma11_txsul,theta1_arma11_txsul)

params_txsul <- list("dbsul"=dbsul,"calculos_taxa_sul"=  clc_tx_sul, 
                     "taxamod_ar1" = par_ar1_txsul, "taxamod_ar2"=par_ar2_txsul,
                     "taxamod_ma1"=par_ma1_txsul,"taxamod_ma2"=par_ma2_txsul, "taxamod_arma11"=par_arma11_txsul)

saveRDS(params_txsul,file = "C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/03_params_taxa_sul.rds")

### 04 - TRIÂNGULO MINEIRO #####################################################

dbtrg <- base$`04-Triângulo Mineiro`

## Definindo variáveis adicionais
# Para o arquivo gabarito o cálculo de K é feito conforme: (ncol(dbbh)-1)/2
# Foi necessário ajustar conforme a base
# Por mais que o arqv baserot0324 seja semelhante ao baseMG_k, ele contém os erros padrão para cada grupo de rotação

colnames(dbtrg)
t = c(1:nrow(dbtrg))
lags = 24
T = nrow(dbtrg)
K = (ncol(dbtrg)-1)/6   # Aqui é o número de grupos

# Valor médio dos painéis

dbtrg$media_txdesoc = dbtrg %>%  select(starts_with("txdesoc")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)

View(dbtrg) #Verificação

# Matriz de pseudo erros

dbtrg$pseudo1_txdesoc = dbtrg$txdesoc_1 - dbtrg$media_txdesoc
dbtrg$pseudo2_txdesoc = dbtrg$txdesoc_2 - dbtrg$media_txdesoc
dbtrg$pseudo3_txdesoc = dbtrg$txdesoc_3 - dbtrg$media_txdesoc
dbtrg$pseudo4_txdesoc = dbtrg$txdesoc_4 - dbtrg$media_txdesoc
dbtrg$pseudo5_txdesoc = dbtrg$txdesoc_5 - dbtrg$media_txdesoc

View(dbtrg)

# Taxa da desocupação: Autocov; FAC e FACP

lag = c(0:24)
clc_tx_trg=as.data.frame(lag)
head(clc_tx_trg)

## Calculo autocov dos pseudoerros (Ch)

clc_tx_trg$Ch1 = Pcov2(dbtrg$pseudo1_txdesoc, lag = lags + 1)
clc_tx_trg$Ch2 = Pcov2(dbtrg$pseudo2_txdesoc, lag = lags + 1)
clc_tx_trg$Ch3 = Pcov2(dbtrg$pseudo3_txdesoc, lag = lags + 1)
clc_tx_trg$Ch4 = Pcov2(dbtrg$pseudo4_txdesoc, lag = lags + 1)
clc_tx_trg$Ch5 = Pcov2(dbtrg$pseudo5_txdesoc, lag = lags + 1)

View(clc_tx_trg)

# Soma das autocovs dos pseudo-erros:

clc_tx_trg$SomaChk = clc_tx_trg$Ch1 + clc_tx_trg$Ch2 +clc_tx_trg$Ch3 +clc_tx_trg$Ch4 +clc_tx_trg$Ch5
clc_tx_trg$autocov = clc_tx_trg$SomaChk/(K^2-K)

# FAC
clc_tx_trg$fac = clc_tx_trg$SomaChk/clc_tx_trg$SomaChk[1]
View(clc_tx_trg)

# FACP
clc_tx_trg$facp = 0 
clc_tx_trg$facp[2:25] = facp_acf(clc_tx_trg$fac,lags) # Função retirada de source("data/funcoes/01_funcoes_pseudtx_erro.R")
View(clc_tx_trg)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_tx_trg$esttest = nrow(dbtrg)*clc_tx_trg$facp ^ 2 
clc_tx_trg$pvalor = teste(clc_tx_trg$facp,nrow(dbtrg)) # ver função teste
View(clc_tx_trg)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_trg$lag, clc_tx_trg$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_trg$lag[2:25], clc_tx_trg$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}

### Parâmetros para os modelos

rho1_tx_trg <- clc_tx_trg$fac[2]
rho2_tx_trg <- clc_tx_trg$fac[3]

#AR(1)

phi1_ar1_txtrg = clc_tx_trg$fac[lag==1]

par_ar1_txtrg<- data.frame(phi1_ar1_txtrg)

### MA(1):

theta1_ma1_txtrg <- (1 - sqrt(1 - 4 * rho1_tx_trg^2)) / (2 * rho1_tx_trg)

par_ma1_txtrg<-data.frame(theta1_ma1_txtrg)

### AR(2)

phi1_ar2_txtrg <- (rho1_tx_trg - rho1_tx_trg * rho2_tx_trg) / (1 - rho1_tx_trg^2)
phi2_ar2_txtrg <- (rho2_tx_trg - rho1_tx_trg^2) / (1 - rho1_tx_trg^2)

par_ar2_txtrg<-data.frame(phi1_ar2_txtrg,phi2_ar2_txtrg)

### MA(2)

rho1 <- rho1_tx_trg
rho2 <- rho2_tx_trg
sistema_eq <- function(theta) {
  theta1 <- theta[1]
  theta2 <- theta[2]
  eq1 <- (-theta1 * (1 - theta2)) / (1 + theta1^2 + theta2^2) - rho1
  eq2 <- (-theta2) / (1 + theta1^2 + theta2^2) - rho2
  return(c(eq1, eq2))
}
theta_ini <- c(0, 0)
solucao <- nleqslv(theta_ini, sistema_eq)
solucao$x

par_ma2_txtrg<-data.frame("theta1_ma2_txtrg"=solucao$x[1],"theta2_ma2_txtrg"=solucao$x[2])

### ARMA (1,1)

phi1_arma11_txtrg <- rho2_tx_trg/rho1_tx_trg

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  retrgtado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- retrgtado_theta1$root
  return(theta1)
}

rho1 <- rho1_tx_trg
phi1 <- phi1_arma11_txtrg

theta1_arma11_txtrg <- arma11_theta1(rho1, phi1)

par_arma11_txtrg <-data.frame(phi1_arma11_txtrg,theta1_arma11_txtrg)

params_txtrg <- list("dbtrg"=dbtrg,"calculos_taxa_trg"=  clc_tx_trg, 
                     "taxamod_ar1" = par_ar1_txtrg, "taxamod_ar2"=par_ar2_txtrg,
                     "taxamod_ma1"=par_ma1_txtrg,"taxamod_ma2"=par_ma2_txtrg, "taxamod_arma11"=par_arma11_txtrg)

saveRDS(params_txtrg,file = "C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/04_params_taxa_trg.rds")

### 05 - ZONA DA MATA ##########################################################

dbmat <- base$`05-Mata de Minas Gerais`

## Definindo variáveis adicionais
# Para o arquivo gabarito o cálculo de K é feito conforme: (ncol(dbbh)-1)/2
# Foi necessário ajustar conforme a base
# Por mais que o arqv baserot0324 seja semelhante ao baseMG_k, ele contém os erros padrão para cada grupo de rotação

colnames(dbmat)
t = c(1:nrow(dbmat))
lags = 24
T = nrow(dbmat)
K = (ncol(dbmat)-1)/6   # Aqui é o número de grupos

# Valor médio dos painéis

dbmat$media_txdesoc = dbmat %>%  select(starts_with("txdesoc")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)

View(dbmat) #Verificação

# Matriz de pseudo erros

dbmat$pseudo1_txdesoc = dbmat$txdesoc_1 - dbmat$media_txdesoc
dbmat$pseudo2_txdesoc = dbmat$txdesoc_2 - dbmat$media_txdesoc
dbmat$pseudo3_txdesoc = dbmat$txdesoc_3 - dbmat$media_txdesoc
dbmat$pseudo4_txdesoc = dbmat$txdesoc_4 - dbmat$media_txdesoc
dbmat$pseudo5_txdesoc = dbmat$txdesoc_5 - dbmat$media_txdesoc

View(dbmat)

# Taxa da desocupação: Autocov; FAC e FACP

lag = c(0:24)
clc_tx_mat=as.data.frame(lag)
head(clc_tx_mat)

## Calculo autocov dos pseudoerros (Ch)

clc_tx_mat$Ch1 = Pcov2(dbmat$pseudo1_txdesoc, lag = lags + 1)
clc_tx_mat$Ch2 = Pcov2(dbmat$pseudo2_txdesoc, lag = lags + 1)
clc_tx_mat$Ch3 = Pcov2(dbmat$pseudo3_txdesoc, lag = lags + 1)
clc_tx_mat$Ch4 = Pcov2(dbmat$pseudo4_txdesoc, lag = lags + 1)
clc_tx_mat$Ch5 = Pcov2(dbmat$pseudo5_txdesoc, lag = lags + 1)

View(clc_tx_mat)

# Soma das autocovs dos pseudo-erros:

clc_tx_mat$SomaChk = clc_tx_mat$Ch1 + clc_tx_mat$Ch2 +clc_tx_mat$Ch3 +clc_tx_mat$Ch4 +clc_tx_mat$Ch5
clc_tx_mat$autocov = clc_tx_mat$SomaChk/(K^2-K)

# FAC
clc_tx_mat$fac = clc_tx_mat$SomaChk/clc_tx_mat$SomaChk[1]
View(clc_tx_mat)

# FACP
clc_tx_mat$facp = 0 
clc_tx_mat$facp[2:25] = facp_acf(clc_tx_mat$fac,lags) # Função retirada de source("data/funcoes/01_funcoes_pseudtx_erro.R")
View(clc_tx_mat)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_tx_mat$esttest = nrow(dbmat)*clc_tx_mat$facp ^ 2 
clc_tx_mat$pvalor = teste(clc_tx_mat$facp,nrow(dbmat)) # ver função teste
View(clc_tx_mat)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_mat$lag, clc_tx_mat$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_mat$lag[2:25], clc_tx_mat$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}

### Parâmetros para os modelos

rho1_tx_mat <- clc_tx_mat$fac[2]
rho2_tx_mat <- clc_tx_mat$fac[3]

#AR(1)

phi1_ar1_txmat = clc_tx_mat$fac[lag==1]

par_ar1_txmat<- data.frame(phi1_ar1_txmat)

### MA(1):

theta1_ma1_txmat <- (1 - sqrt(1 - 4 * rho1_tx_mat^2)) / (2 * rho1_tx_mat)

par_ma1_txmat<-data.frame(theta1_ma1_txmat)

### AR(2)

phi1_ar2_txmat <- (rho1_tx_mat - rho1_tx_mat * rho2_tx_mat) / (1 - rho1_tx_mat^2)
phi2_ar2_txmat <- (rho2_tx_mat - rho1_tx_mat^2) / (1 - rho1_tx_mat^2)

par_ar2_txmat<-data.frame(phi1_ar2_txmat,phi2_ar2_txmat)

### MA(2)

rho1 <- rho1_tx_mat
rho2 <- rho2_tx_mat
sistema_eq <- function(theta) {
  theta1 <- theta[1]
  theta2 <- theta[2]
  eq1 <- (-theta1 * (1 - theta2)) / (1 + theta1^2 + theta2^2) - rho1
  eq2 <- (-theta2) / (1 + theta1^2 + theta2^2) - rho2
  return(c(eq1, eq2))
}
theta_ini <- c(0, 0)
solucao <- nleqslv(theta_ini, sistema_eq)
solucao$x

par_ma2_txmat<-data.frame("theta1_ma2_txmat"=solucao$x[1],"theta2_ma2_txmat"=solucao$x[2])

### ARMA (1,1)

phi1_arma11_txmat <- rho2_tx_mat/rho1_tx_mat

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  remattado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- remattado_theta1$root
  return(theta1)
}

rho1 <- rho1_tx_mat
phi1 <- phi1_arma11_txmat

theta1_arma11_txmat <- arma11_theta1(rho1, phi1)

par_arma11_txmat <-data.frame(phi1_arma11_txmat,theta1_arma11_txmat)

params_txmat <- list("dbmat"=dbmat,"calculos_taxa_mat"=  clc_tx_mat, 
                     "taxamod_ar1" = par_ar1_txmat, "taxamod_ar2"=par_ar2_txmat,
                     "taxamod_ma1"=par_ma1_txmat,"taxamod_ma2"=par_ma2_txmat, "taxamod_arma11"=par_arma11_txmat)

saveRDS(params_txmat,file = "C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/05_params_taxa_mat.rds")

### 06 - NORTE DE MINAS ########################################################

dbnrt <- base$`06-Norte de Minas`

## Definindo variáveis adicionais
# Para o arquivo gabarito o cálculo de K é feito conforme: (ncol(dbbh)-1)/2
# Foi necessário ajustar conforme a base
# Por mais que o arqv baserot0324 seja semelhante ao baseMG_k, ele contém os erros padrão para cada grupo de rotação

colnames(dbnrt)
t = c(1:nrow(dbnrt))
lags = 24
T = nrow(dbnrt)
K = (ncol(dbnrt)-1)/6   # Aqui é o número de grupos

# Valor médio dos painéis

dbnrt$media_txdesoc = dbnrt %>%  select(starts_with("txdesoc")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)

View(dbnrt) #Verificação

# nrtriz de pseudo erros

dbnrt$pseudo1_txdesoc = dbnrt$txdesoc_1 - dbnrt$media_txdesoc
dbnrt$pseudo2_txdesoc = dbnrt$txdesoc_2 - dbnrt$media_txdesoc
dbnrt$pseudo3_txdesoc = dbnrt$txdesoc_3 - dbnrt$media_txdesoc
dbnrt$pseudo4_txdesoc = dbnrt$txdesoc_4 - dbnrt$media_txdesoc
dbnrt$pseudo5_txdesoc = dbnrt$txdesoc_5 - dbnrt$media_txdesoc

View(dbnrt)

# Taxa da desocupação: Autocov; FAC e FACP

lag = c(0:24)
clc_tx_nrt=as.data.frame(lag)
head(clc_tx_nrt)

## Calculo autocov dos pseudoerros (Ch)

clc_tx_nrt$Ch1 = Pcov2(dbnrt$pseudo1_txdesoc, lag = lags + 1)
clc_tx_nrt$Ch2 = Pcov2(dbnrt$pseudo2_txdesoc, lag = lags + 1)
clc_tx_nrt$Ch3 = Pcov2(dbnrt$pseudo3_txdesoc, lag = lags + 1)
clc_tx_nrt$Ch4 = Pcov2(dbnrt$pseudo4_txdesoc, lag = lags + 1)
clc_tx_nrt$Ch5 = Pcov2(dbnrt$pseudo5_txdesoc, lag = lags + 1)

View(clc_tx_nrt)

# Soma das autocovs dos pseudo-erros:

clc_tx_nrt$SomaChk = clc_tx_nrt$Ch1 + clc_tx_nrt$Ch2 +clc_tx_nrt$Ch3 +clc_tx_nrt$Ch4 +clc_tx_nrt$Ch5
clc_tx_nrt$autocov = clc_tx_nrt$SomaChk/(K^2-K)

# FAC
clc_tx_nrt$fac = clc_tx_nrt$SomaChk/clc_tx_nrt$SomaChk[1]
View(clc_tx_nrt)

# FACP
clc_tx_nrt$facp = 0 
clc_tx_nrt$facp[2:25] = facp_acf(clc_tx_nrt$fac,lags) # Função retirada de source("data/funcoes/01_funcoes_pseudtx_erro.R")
View(clc_tx_nrt)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_tx_nrt$esttest = nrow(dbnrt)*clc_tx_nrt$facp ^ 2 
clc_tx_nrt$pvalor = teste(clc_tx_nrt$facp,nrow(dbnrt)) # ver função teste
View(clc_tx_nrt)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_nrt$lag, clc_tx_nrt$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_nrt$lag[2:25], clc_tx_nrt$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}

### Parâmetros para os modelos

rho1_tx_nrt <- clc_tx_nrt$fac[2]
rho2_tx_nrt <- clc_tx_nrt$fac[3]

#AR(1)

phi1_ar1_txnrt = clc_tx_nrt$fac[lag==1]

par_ar1_txnrt<- data.frame(phi1_ar1_txnrt)

### MA(1):

theta1_ma1_txnrt <- (1 - sqrt(1 - 4 * rho1_tx_nrt^2)) / (2 * rho1_tx_nrt)

par_ma1_txnrt<-data.frame(theta1_ma1_txnrt)

### AR(2)

phi1_ar2_txnrt <- (rho1_tx_nrt - rho1_tx_nrt * rho2_tx_nrt) / (1 - rho1_tx_nrt^2)
phi2_ar2_txnrt <- (rho2_tx_nrt - rho1_tx_nrt^2) / (1 - rho1_tx_nrt^2)

par_ar2_txnrt<-data.frame(phi1_ar2_txnrt,phi2_ar2_txnrt)

### MA(2)

rho1 <- rho1_tx_nrt
rho2 <- rho2_tx_nrt
sistema_eq <- function(theta) {
  theta1 <- theta[1]
  theta2 <- theta[2]
  eq1 <- (-theta1 * (1 - theta2)) / (1 + theta1^2 + theta2^2) - rho1
  eq2 <- (-theta2) / (1 + theta1^2 + theta2^2) - rho2
  return(c(eq1, eq2))
}
theta_ini <- c(0, 0)
solucao <- nleqslv(theta_ini, sistema_eq)
solucao$x

par_ma2_txnrt<-data.frame("theta1_ma2_txnrt"=solucao$x[1],"theta2_ma2_txnrt"=solucao$x[2])

### ARMA (1,1)

phi1_arma11_txnrt <- rho2_tx_nrt/rho1_tx_nrt

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  renrttado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- renrttado_theta1$root
  return(theta1)
}

rho1 <- rho1_tx_nrt
phi1 <- phi1_arma11_txnrt

theta1_arma11_txnrt <- arma11_theta1(rho1, phi1)

par_arma11_txnrt <-data.frame(phi1_arma11_txnrt,theta1_arma11_txnrt)

params_txnrt <- list("dbnrt"=dbnrt,"calculos_taxa_nrt"=  clc_tx_nrt, 
                     "taxamod_ar1" = par_ar1_txnrt, "taxamod_ar2"=par_ar2_txnrt,
                     "taxamod_ma1"=par_ma1_txnrt,"taxamod_ma2"=par_ma2_txnrt, "taxamod_arma11"=par_arma11_txnrt)

saveRDS(params_txnrt,file = "C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/06_params_taxa_nrt.rds")


### 07 - VALE DO RIO DOCE ######################################################

dbval <- base$`07-Vale do Rio Doce`

## Definindo variáveis adicionais
# Para o arquivo gabarito o cálculo de K é feito conforme: (ncol(dbbh)-1)/2
# Foi necessário ajustar conforme a base
# Por mais que o arqv baserot0324 seja semelhante ao baseMG_k, ele contém os erros padrão para cada grupo de rotação

colnames(dbval)
t = c(1:nrow(dbval))
lags = 24
T = nrow(dbval)
K = (ncol(dbval)-1)/6   # Aqui é o número de grupos

# Valor médio dos painéis

dbval$media_txdesoc = dbval %>%  select(starts_with("txdesoc")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)

View(dbval) #Verificação

# valriz de pseudo erros

dbval$pseudo1_txdesoc = dbval$txdesoc_1 - dbval$media_txdesoc
dbval$pseudo2_txdesoc = dbval$txdesoc_2 - dbval$media_txdesoc
dbval$pseudo3_txdesoc = dbval$txdesoc_3 - dbval$media_txdesoc
dbval$pseudo4_txdesoc = dbval$txdesoc_4 - dbval$media_txdesoc
dbval$pseudo5_txdesoc = dbval$txdesoc_5 - dbval$media_txdesoc

View(dbval)

# Taxa da desocupação: Autocov; FAC e FACP

lag = c(0:24)
clc_tx_val=as.data.frame(lag)
head(clc_tx_val)

## Calculo autocov dos pseudoerros (Ch)

clc_tx_val$Ch1 = Pcov2(dbval$pseudo1_txdesoc, lag = lags + 1)
clc_tx_val$Ch2 = Pcov2(dbval$pseudo2_txdesoc, lag = lags + 1)
clc_tx_val$Ch3 = Pcov2(dbval$pseudo3_txdesoc, lag = lags + 1)
clc_tx_val$Ch4 = Pcov2(dbval$pseudo4_txdesoc, lag = lags + 1)
clc_tx_val$Ch5 = Pcov2(dbval$pseudo5_txdesoc, lag = lags + 1)

View(clc_tx_val)

# Soma das autocovs dos pseudo-erros:

clc_tx_val$SomaChk = clc_tx_val$Ch1 + clc_tx_val$Ch2 +clc_tx_val$Ch3 +clc_tx_val$Ch4 +clc_tx_val$Ch5
clc_tx_val$autocov = clc_tx_val$SomaChk/(K^2-K)

# FAC
clc_tx_val$fac = clc_tx_val$SomaChk/clc_tx_val$SomaChk[1]
View(clc_tx_val)

# FACP
clc_tx_val$facp = 0 
clc_tx_val$facp[2:25] = facp_acf(clc_tx_val$fac,lags) # Função retirada de source("data/funcoes/01_funcoes_pseudtx_erro.R")
View(clc_tx_val)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_tx_val$esttest = nrow(dbval)*clc_tx_val$facp ^ 2 
clc_tx_val$pvalor = teste(clc_tx_val$facp,nrow(dbval)) # ver função teste
View(clc_tx_val)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_val$lag, clc_tx_val$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_val$lag[2:25], clc_tx_val$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}

### Parâmetros para os modelos

rho1_tx_val <- clc_tx_val$fac[2]
rho2_tx_val <- clc_tx_val$fac[3]

#AR(1)

phi1_ar1_txval = clc_tx_val$fac[lag==1]

par_ar1_txval<- data.frame(phi1_ar1_txval)

### MA(1):

theta1_ma1_txval <- (1 - sqrt(1 - 4 * rho1_tx_val^2)) / (2 * rho1_tx_val)

par_ma1_txval<-data.frame(theta1_ma1_txval)

### AR(2)

phi1_ar2_txval <- (rho1_tx_val - rho1_tx_val * rho2_tx_val) / (1 - rho1_tx_val^2)
phi2_ar2_txval <- (rho2_tx_val - rho1_tx_val^2) / (1 - rho1_tx_val^2)

par_ar2_txval<-data.frame(phi1_ar2_txval,phi2_ar2_txval)

### MA(2)

rho1 <- rho1_tx_val
rho2 <- rho2_tx_val
sistema_eq <- function(theta) {
  theta1 <- theta[1]
  theta2 <- theta[2]
  eq1 <- (-theta1 * (1 - theta2)) / (1 + theta1^2 + theta2^2) - rho1
  eq2 <- (-theta2) / (1 + theta1^2 + theta2^2) - rho2
  return(c(eq1, eq2))
}
theta_ini <- c(0, 0)
solucao <- nleqslv(theta_ini, sistema_eq)
solucao$x

par_ma2_txval<-data.frame("theta1_ma2_txval"=solucao$x[1],"theta2_ma2_txval"=solucao$x[2])

### ARMA (1,1)

phi1_arma11_txval <- rho2_tx_val/rho1_tx_val

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  revaltado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- revaltado_theta1$root
  return(theta1)
}

rho1 <- rho1_tx_val
phi1 <- phi1_arma11_txval

theta1_arma11_txval <- arma11_theta1(rho1, phi1)

par_arma11_txval <-data.frame(phi1_arma11_txval,theta1_arma11_txval)

params_txval <- list("dbval"=dbval,"calculos_taxa_val"=  clc_tx_val, 
                     "taxamod_ar1" = par_ar1_txval, "taxamod_ar2"=par_ar2_txval,
                     "taxamod_ma1"=par_ma1_txval,"taxamod_ma2"=par_ma2_txval, "taxamod_arma11"=par_arma11_txval)

saveRDS(params_txval,file = "C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/07_params_taxa_val.rds")

### 08 - CENTRAL ###############################################################

dbcen <- base$`08-Central`

## Definindo variáveis adicionais
# Para o arquivo gabarito o cálculo de K é feito conforme: (ncol(dbbh)-1)/2
# Foi necessário ajustar conforme a base
# Por mais que o arqv baserot0324 seja semelhante ao baseMG_k, ele contém os erros padrão para cada grupo de rotação

colnames(dbcen)
t = c(1:nrow(dbcen))
lags = 24
T = nrow(dbcen)
K = (ncol(dbcen)-1)/6   # Aqui é o número de grupos

# cenor médio dos painéis

dbcen$media_txdesoc = dbcen %>%  select(starts_with("txdesoc")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)

View(dbcen) #Verificação

# cenriz de pseudo erros

dbcen$pseudo1_txdesoc = dbcen$txdesoc_1 - dbcen$media_txdesoc
dbcen$pseudo2_txdesoc = dbcen$txdesoc_2 - dbcen$media_txdesoc
dbcen$pseudo3_txdesoc = dbcen$txdesoc_3 - dbcen$media_txdesoc
dbcen$pseudo4_txdesoc = dbcen$txdesoc_4 - dbcen$media_txdesoc
dbcen$pseudo5_txdesoc = dbcen$txdesoc_5 - dbcen$media_txdesoc

View(dbcen)

# Taxa da desocupação: Autocov; FAC e FACP

lag = c(0:24)
clc_tx_cen=as.data.frame(lag)
head(clc_tx_cen)

## Calculo autocov dos pseudoerros (Ch)

clc_tx_cen$Ch1 = Pcov2(dbcen$pseudo1_txdesoc, lag = lags + 1)
clc_tx_cen$Ch2 = Pcov2(dbcen$pseudo2_txdesoc, lag = lags + 1)
clc_tx_cen$Ch3 = Pcov2(dbcen$pseudo3_txdesoc, lag = lags + 1)
clc_tx_cen$Ch4 = Pcov2(dbcen$pseudo4_txdesoc, lag = lags + 1)
clc_tx_cen$Ch5 = Pcov2(dbcen$pseudo5_txdesoc, lag = lags + 1)

View(clc_tx_cen)

# Soma das autocovs dos pseudo-erros:

clc_tx_cen$SomaChk = clc_tx_cen$Ch1 + clc_tx_cen$Ch2 +clc_tx_cen$Ch3 +clc_tx_cen$Ch4 +clc_tx_cen$Ch5
clc_tx_cen$autocov = clc_tx_cen$SomaChk/(K^2-K)

# FAC
clc_tx_cen$fac = clc_tx_cen$SomaChk/clc_tx_cen$SomaChk[1]
View(clc_tx_cen)

# FACP
clc_tx_cen$facp = 0 
clc_tx_cen$facp[2:25] = facp_acf(clc_tx_cen$fac,lags) # Função retirada de source("data/funcoes/01_funcoes_pseudtx_erro.R")
View(clc_tx_cen)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_tx_cen$esttest = nrow(dbcen)*clc_tx_cen$facp ^ 2 
clc_tx_cen$pcenor = teste(clc_tx_cen$facp,nrow(dbcen)) # ver função teste
View(clc_tx_cen)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_cen$lag, clc_tx_cen$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_cen$lag[2:25], clc_tx_cen$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}

### Parâmetros para os modelos

rho1_tx_cen <- clc_tx_cen$fac[2]
rho2_tx_cen <- clc_tx_cen$fac[3]

#AR(1)

phi1_ar1_txcen = clc_tx_cen$fac[lag==1]

par_ar1_txcen<- data.frame(phi1_ar1_txcen)

### MA(1):

theta1_ma1_txcen <- (1 - sqrt(1 - 4 * rho1_tx_cen^2)) / (2 * rho1_tx_cen)

par_ma1_txcen<-data.frame(theta1_ma1_txcen)

### AR(2)

phi1_ar2_txcen <- (rho1_tx_cen - rho1_tx_cen * rho2_tx_cen) / (1 - rho1_tx_cen^2)
phi2_ar2_txcen <- (rho2_tx_cen - rho1_tx_cen^2) / (1 - rho1_tx_cen^2)

par_ar2_txcen<-data.frame(phi1_ar2_txcen,phi2_ar2_txcen)

### MA(2)

rho1 <- rho1_tx_cen
rho2 <- rho2_tx_cen
sistema_eq <- function(theta) {
  theta1 <- theta[1]
  theta2 <- theta[2]
  eq1 <- (-theta1 * (1 - theta2)) / (1 + theta1^2 + theta2^2) - rho1
  eq2 <- (-theta2) / (1 + theta1^2 + theta2^2) - rho2
  return(c(eq1, eq2))
}
theta_ini <- c(0, 0)
solucao <- nleqslv(theta_ini, sistema_eq)
solucao$x

par_ma2_txcen<-data.frame("theta1_ma2_txcen"=solucao$x[1],"theta2_ma2_txcen"=solucao$x[2])

### ARMA (1,1)

#phi1_arma11_txcen <- rho2_tx_cen/rho1_tx_cen

#arma11_theta1 <- function(rho1, phi1) {
#  eq_theta1 <- function(theta1, rho1, phi1) {
#    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
#  }
#  recentado_theta1 <- uniroot(eq_theta1, intercen = c(-1, 1), rho1 = rho1, phi1 = phi1)
#  theta1 <- recentado_theta1$root
#  return(theta1)
#}

#rho1 <- rho1_tx_cen
#phi1 <- phi1_arma11_txcen

#theta1_arma11_txcen <- arma11_theta1(rho1, phi1)

#par_arma11_txcen <-data.frame(phi1_arma11_txcen,theta1_arma11_txcen)

params_txcen <- list("dbcen"=dbcen,"calculos_taxa_cen"=  clc_tx_cen, 
                     "taxamod_ar1" = par_ar1_txcen, "taxamod_ar2"=par_ar2_txcen,
                     "taxamod_ma1"=par_ma1_txcen,"taxamod_ma2"=par_ma2_txcen)#, "taxamod_arma11"=par_arma11_txcen)

saveRDS(params_txcen,file = "C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/08_params_taxa_cen.rds")


### 09 - MINAS GERAIS ##########################################################

dbmg <- base$`09 - Minas Gerais`

## Definindo variáveis adicionais
# Para o arquivo gabarito o cálculo de K é feito conforme: (ncol(dbbh)-1)/2
# Foi necessário ajustar conforme a base
# Por mais que o arqv baserot0324 seja semelhante ao baseMG_k, ele contém os erros padrão para cada grupo de rotação

colnames(dbmg)
t = c(1:nrow(dbmg))
lags = 24
T = nrow(dbmg)
K = (ncol(dbmg)-1)/6   # Aqui é o número de grupos

# mgor médio dos painéis

dbmg$media_txdesoc = dbmg %>%  select(starts_with("txdesoc")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)

View(dbmg) #Verificação

# mgriz de pseudo erros

dbmg$pseudo1_txdesoc = dbmg$txdesoc_1 - dbmg$media_txdesoc
dbmg$pseudo2_txdesoc = dbmg$txdesoc_2 - dbmg$media_txdesoc
dbmg$pseudo3_txdesoc = dbmg$txdesoc_3 - dbmg$media_txdesoc
dbmg$pseudo4_txdesoc = dbmg$txdesoc_4 - dbmg$media_txdesoc
dbmg$pseudo5_txdesoc = dbmg$txdesoc_5 - dbmg$media_txdesoc

View(dbmg)

# Taxa da desocupação: Autocov; FAC e FACP

lag = c(0:24)
clc_tx_mg=as.data.frame(lag)
head(clc_tx_mg)

## Calculo autocov dos pseudoerros (Ch)

clc_tx_mg$Ch1 = Pcov2(dbmg$pseudo1_txdesoc, lag = lags + 1)
clc_tx_mg$Ch2 = Pcov2(dbmg$pseudo2_txdesoc, lag = lags + 1)
clc_tx_mg$Ch3 = Pcov2(dbmg$pseudo3_txdesoc, lag = lags + 1)
clc_tx_mg$Ch4 = Pcov2(dbmg$pseudo4_txdesoc, lag = lags + 1)
clc_tx_mg$Ch5 = Pcov2(dbmg$pseudo5_txdesoc, lag = lags + 1)

View(clc_tx_mg)

# Soma das autocovs dos pseudo-erros:

clc_tx_mg$SomaChk = clc_tx_mg$Ch1 + clc_tx_mg$Ch2 +clc_tx_mg$Ch3 +clc_tx_mg$Ch4 +clc_tx_mg$Ch5
clc_tx_mg$autocov = clc_tx_mg$SomaChk/(K^2-K)

# FAC
clc_tx_mg$fac = clc_tx_mg$SomaChk/clc_tx_mg$SomaChk[1]
View(clc_tx_mg)

# FACP
clc_tx_mg$facp = 0 
clc_tx_mg$facp[2:25] = facp_acf(clc_tx_mg$fac,lags) # Função retirada de source("data/funcoes/01_funcoes_pseudtx_erro.R")
View(clc_tx_mg)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_tx_mg$esttest = nrow(dbmg)*clc_tx_mg$facp ^ 2 
clc_tx_mg$pmgor = teste(clc_tx_mg$facp,nrow(dbmg)) # ver função teste
View(clc_tx_mg)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_mg$lag, clc_tx_mg$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_tx_mg$lag[2:25], clc_tx_mg$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}

### Parâmetros para os modelos

rho1_tx_mg <- clc_tx_mg$fac[2]
rho2_tx_mg <- clc_tx_mg$fac[3]

#AR(1)

phi1_ar1_txmg = clc_tx_mg$fac[lag==1]

par_ar1_txmg<- data.frame(phi1_ar1_txmg)

### MA(1):

theta1_ma1_txmg <- (1 - sqrt(1 - 4 * rho1_tx_mg^2)) / (2 * rho1_tx_mg)

par_ma1_txmg<-data.frame(theta1_ma1_txmg)

### AR(2)

phi1_ar2_txmg <- (rho1_tx_mg - rho1_tx_mg * rho2_tx_mg) / (1 - rho1_tx_mg^2)
phi2_ar2_txmg <- (rho2_tx_mg - rho1_tx_mg^2) / (1 - rho1_tx_mg^2)

par_ar2_txmg<-data.frame(phi1_ar2_txmg,phi2_ar2_txmg)

### MA(2)

rho1 <- rho1_tx_mg
rho2 <- rho2_tx_mg
sistema_eq <- function(theta) {
  theta1 <- theta[1]
  theta2 <- theta[2]
  eq1 <- (-theta1 * (1 - theta2)) / (1 + theta1^2 + theta2^2) - rho1
  eq2 <- (-theta2) / (1 + theta1^2 + theta2^2) - rho2
  return(c(eq1, eq2))
}
theta_ini <- c(0, 0)
solucao <- nleqslv(theta_ini, sistema_eq)
solucao$x

par_ma2_txmg<-data.frame("theta1_ma2_txmg"=solucao$x[1],"theta2_ma2_txmg"=solucao$x[2])

### ARMA (1,1)

#phi1_arma11_txmg <- rho2_tx_mg/rho1_tx_mg

#arma11_theta1 <- function(rho1, phi1) {
#  eq_theta1 <- function(theta1, rho1, phi1) {
#    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
#  }
#  remgtado_theta1 <- uniroot(eq_theta1, intermg = c(-1, 1), rho1 = rho1, phi1 = phi1)
#  theta1 <- remgtado_theta1$root
#  return(theta1)
#}

##rho1 <- rho1_tx_mg
#phi1 <- phi1_arma11_txmg

#theta1_arma11_txmg <- arma11_theta1(rho1, phi1)

#par_arma11_txmg <-data.frame(phi1_arma11_txmg,theta1_arma11_txmg)

params_txmg <- list("dbmg"=dbmg,"calculos_taxa_mg"=  clc_tx_mg, 
                     "taxamod_ar1" = par_ar1_txmg, "taxamod_ar2"=par_ar2_txmg,
                     "taxamod_ma1"=par_ma1_txmg,"taxamod_ma2"=par_ma2_txmg)#, "taxamod_arma11"=par_arma11_txmg)

saveRDS(params_txmg,file = "C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/09_params_taxa_mg.rds")