################################################################################
##                          MODELO ERRO AMOSTRAL                              ##
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

base <- readRDS("D:/FJP2425/Programacao/data/basealinhada0424.RDS")

## Utilizando conjunto de funções já prontas para o script

source("data/funcoes/01_funcoes_pseudo_erro.R")

### 01-BELO HORIZONTE###########################################################

dbbh<-base[["01-Belo Horizonte"]]

## Definindo variáveis adicionais
  # Para o arquivo gabarito o cálculo de K é feito conforme: (ncol(dbbh)-1)/2
    # Foi necessário ajustar conforme a base
      # Por mais que o arqv baserot0324 seja semelhante ao baseMG_k, ele contém os erros padrão para cada
      # grupo de rotação

colnames(dbbh)
t = c(1:nrow(dbbh))
lags = 24
T = nrow(dbbh)
K = (ncol(dbbh)-11)/2 

# Valor médio dos painéis

dbbh$media_ocupada = dbbh %>%  select(starts_with("ocupada")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)
dbbh$media_desocupada = dbbh %>%  select(starts_with("desocupada")) %>%  
  replace(.==0, NA) %>%  rowMeans(.,na.rm = TRUE)

View(dbbh) #Verificação

# Matriz de pseudo erros

dbbh$pseudo1_ocupada = dbbh$ocupada_1 - dbbh$media_ocupada
dbbh$pseudo2_ocupada = dbbh$ocupada_2 - dbbh$media_ocupada
dbbh$pseudo3_ocupada = dbbh$ocupada_3 - dbbh$media_ocupada
dbbh$pseudo4_ocupada = dbbh$ocupada_4 - dbbh$media_ocupada
dbbh$pseudo5_ocupada = dbbh$ocupada_5 - dbbh$media_ocupada

dbbh$pseudo1_desocupada = dbbh$desocupada_1 - dbbh$media_desocupada
dbbh$pseudo2_desocupada = dbbh$desocupada_2 - dbbh$media_desocupada
dbbh$pseudo3_desocupada = dbbh$desocupada_3 - dbbh$media_desocupada
dbbh$pseudo4_desocupada = dbbh$desocupada_4 - dbbh$media_desocupada
dbbh$pseudo5_desocupada = dbbh$desocupada_5 - dbbh$media_desocupada

View(dbbh)

# Ocupada: Autocov; FAC e FACP

lag = c(0:24)
clc_o_bh=as.data.frame(lag)
head(clc_o_bh)

  ## Calculo autocov dos pseudoerros (Ch)

clc_o_bh$Ch1 = Pcov2(dbbh$pseudo1_ocupada, lag = lags + 1)
clc_o_bh$Ch2 = Pcov2(dbbh$pseudo2_ocupada, lag = lags + 1)
clc_o_bh$Ch3 = Pcov2(dbbh$pseudo3_ocupada, lag = lags + 1)
clc_o_bh$Ch4 = Pcov2(dbbh$pseudo4_ocupada, lag = lags + 1)
clc_o_bh$Ch5 = Pcov2(dbbh$pseudo5_ocupada, lag = lags + 1)

View(clc_o_bh)

    # Soma das autocovs dos pseudo-erros:

clc_o_bh$SomaChk = clc_o_bh$Ch1 + clc_o_bh$Ch2 +clc_o_bh$Ch3 +clc_o_bh$Ch4 +clc_o_bh$Ch5
clc_o_bh$autocov = clc_o_bh$SomaChk/(K^2-K)

    # FAC
clc_o_bh$fac = clc_o_bh$SomaChk/clc_o_bh$SomaChk[1]
View(clc_o_bh)

    # FACP
clc_o_bh$facp = 0 
clc_o_bh$facp[2:25] = facp_acf(clc_o_bh$fac,lags) # Função retirada de source("data/funcoes/01_funcoes_pseudo_erro.R")
View(clc_o_bh)

    # Estatísticas de teste:
      ## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_o_bh$esttest = nrow(dbbh)*clc_o_bh$facp ^ 2 
clc_o_bh$pvalor = teste(clc_o_bh$facp,nrow(dbbh)) # ver função teste
View(clc_o_bh)

    # Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
par(bty = "l",cex.lab = 2)
plot(x = clc_o_bh$lag, clc_o_bh$fac, xlab = "", ylab = "",
     type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
lines(x = c(-3,lags), y = c(0,0),lwd = 2)
lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
box()
axis(1, cex.axis = 1)
axis(2, cex.axis = 1)
mtext("lag", side = 1, line = 3)


par(bty = "l",cex.lab = 2)
plot(x = clc_o_bh$lag[2:25], clc_o_bh$facp[2:25], xlab = "", ylab = "",
     type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
lines(x = c(-3,lags), y = c(0,0),lwd = 2)
lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
box()
axis(1, cex.axis = 1)
axis(2, cex.axis = 1)
mtext("lag", side = 1, line = 3)}

# Desocupada: autocov, fac e facp

lag = c(0:24)
clc_d_bh=as.data.frame(lag)
head(clc_d_bh)

  ## Calculo autocov dos pseudoerros (Ch)

clc_d_bh$Ch1 = Pcov2(dbbh$pseudo1_desocupada, lag = lags + 1)
clc_d_bh$Ch2 = Pcov2(dbbh$pseudo2_desocupada, lag = lags + 1)
clc_d_bh$Ch3 = Pcov2(dbbh$pseudo3_desocupada, lag = lags + 1)
clc_d_bh$Ch4 = Pcov2(dbbh$pseudo4_desocupada, lag = lags + 1)
clc_d_bh$Ch5 = Pcov2(dbbh$pseudo5_desocupada, lag = lags + 1)

View(clc_d_bh)

    # Soma das autocovs dos pseudo-erros:

clc_d_bh$SomaChk = clc_d_bh$Ch1 + clc_d_bh$Ch2 +clc_d_bh$Ch3 +clc_d_bh$Ch4 +clc_d_bh$Ch5
clc_d_bh$autocov = clc_d_bh$SomaChk/(K^2-K)

    # FAC
clc_d_bh$fac = clc_d_bh$SomaChk/clc_d_bh$SomaChk[1]
View(clc_d_bh)

    # FACP
clc_d_bh$facp = 0 
clc_d_bh$facp[2:25] = facp_acf(clc_d_bh$fac,lags)
View(clc_d_bh)

    # Estatísticas de teste:
      ## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_d_bh$esttest = nrow(dbbh)*clc_d_bh$facp ^ 2 
clc_d_bh$pvalor = teste(clc_d_bh$facp,nrow(dbbh))
View(clc_d_bh)

    # Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_bh$lag, clc_d_bh$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_bh$lag[2:25], clc_d_bh$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}

### Parâmetros para os modelos

rho1_d_bh <- clc_d_bh$fac[2]
rho2_d_bh <- clc_d_bh$fac[3]

rho1_o_bh <- clc_o_bh$fac[2]
rho2_o_bh <- clc_o_bh$fac[3]

#AR(1)
phi1_ar1_dbh = clc_d_bh$fac[lag==1]
phi1_ar1_obh = clc_o_bh$fac[lag==1]

par_ar1_bh<- data.frame(phi1_ar1_dbh, phi1_ar1_obh)

#MA(1):
theta1_ma1_dbh <- (1 - sqrt(1 - 4 * rho1_d_bh^2)) / (2 * rho1_d_bh)

theta1_ma1_obh <- (1 - sqrt(1 - 4 * rho1_o_bh^2)) / (2 * rho1_o_bh)

par_ma1_bh<-data.frame(theta1_ma1_dbh,theta1_ma1_obh)

#AR(2)

phi1_ar2_dbh <- (rho1_d_bh - rho1_d_bh * rho2_d_bh) / (1 - rho1_d_bh^2)
phi2_ar2_dbh <- (rho2_d_bh - rho1_d_bh^2) / (1 - rho1_d_bh^2)

phi1_ar2_obh <- (rho1_o_bh - rho1_o_bh * rho2_o_bh) / (1 - rho1_o_bh^2)
phi2_ar2_obh <- (rho2_o_bh - rho1_o_bh^2) / (1 - rho1_o_bh^2)

par_ar2_bh<-data.frame(phi1_ar2_dbh,phi2_ar2_dbh,phi1_ar2_obh,phi2_ar2_obh)

#MA(2) - apenas desocupados

rho1 <- rho1_d_bh
rho2 <- rho2_d_bh
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

par_ma2_bh<-data.frame("theta1_ma2_dbh"=solucao$x[1],"theta2_ma2_dbh"=solucao$x[2])

#ARMA (1,1) - apenas desocupados

phi1_arma11_dbh <- rho2_d_bh/rho1_d_bh

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  resultado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- resultado_theta1$root
  return(theta1)
}

rho1 <- rho1_d_bh
phi1 <- phi1_arma11_dbh

theta1_arma11_dbh <- arma11_theta1(rho1, phi1)

par_arma11_bh <-data.frame(phi1_arma11_dbh,theta1_arma11_dbh)

params_bh <- list("dbbh"=dbbh,"calculos_desocupada_bh"=clc_d_bh,
                "calculos_ocupada_bh"=  clc_o_bh, 
                "mod_ar1" = par_ar1_bh, "mod_ar2"=par_ar2_bh,
                "mod_ma1"=par_ma1_bh,"mod_ma2"=par_ma2_bh, "mod_arma11"=par_arma11_bh)

saveRDS(params_bh,file = "D:/FJP2425/Programacao/data/pseudoerros/01_params_bh.rds")

### 02-ENTORNO METROPOLITANO DE BELO HORIZONTE##################################

dbent<-base$`02-Entorno metropolitano de BH`

## Definindo variáveis adicionais

colnames(dbent)
t = c(1:nrow(dbent))
lags = 24
T = nrow(dbent)
K = (ncol(dbent)-11)/2 

# Valor médio dos painéis

dbent$media_ocupada = dbent %>%  select(starts_with("ocupada")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)
dbent$media_desocupada = dbent %>%  select(starts_with("desocupada")) %>%  
  replace(.==0, NA) %>%  rowMeans(.,na.rm = TRUE)

#View(dbent) #Verificação

# Matriz de pseudo erros

dbent$pseudo1_ocupada = dbent$ocupada_1 - dbent$media_ocupada
dbent$pseudo2_ocupada = dbent$ocupada_2 - dbent$media_ocupada
dbent$pseudo3_ocupada = dbent$ocupada_3 - dbent$media_ocupada
dbent$pseudo4_ocupada = dbent$ocupada_4 - dbent$media_ocupada
dbent$pseudo5_ocupada = dbent$ocupada_5 - dbent$media_ocupada

dbent$pseudo1_desocupada = dbent$desocupada_1 - dbent$media_desocupada
dbent$pseudo2_desocupada = dbent$desocupada_2 - dbent$media_desocupada
dbent$pseudo3_desocupada = dbent$desocupada_3 - dbent$media_desocupada
dbent$pseudo4_desocupada = dbent$desocupada_4 - dbent$media_desocupada
dbent$pseudo5_desocupada = dbent$desocupada_5 - dbent$media_desocupada

#View(dbent)

# Ocupada: Autocov; FAC e FACP

lag = c(0:24)
clc_o_ent=as.data.frame(lag)
head(clc_o_ent)

    ## Calculo autocov dos pseudoerros (Ch)

clc_o_ent$Ch1 = Pcov2(dbent$pseudo1_ocupada, lag = lags + 1)
clc_o_ent$Ch2 = Pcov2(dbent$pseudo2_ocupada, lag = lags + 1)
clc_o_ent$Ch3 = Pcov2(dbent$pseudo3_ocupada, lag = lags + 1)
clc_o_ent$Ch4 = Pcov2(dbent$pseudo4_ocupada, lag = lags + 1)
clc_o_ent$Ch5 = Pcov2(dbent$pseudo5_ocupada, lag = lags + 1)

#View(clc_o_ent)

      # Soma das autocovs dos pseudo-erros:

clc_o_ent$SomaChk = clc_o_ent$Ch1 + clc_o_ent$Ch2 +clc_o_ent$Ch3 +clc_o_ent$Ch4 +clc_o_ent$Ch5
clc_o_ent$autocov = clc_o_ent$SomaChk/(K^2-K)

      # FAC
clc_o_ent$fac = clc_o_ent$SomaChk/clc_o_ent$SomaChk[1]

      # FACP
clc_o_ent$facp = 0 
clc_o_ent$facp[2:25] = facp_acf(clc_o_ent$fac,lags)

      # Estatísticas de teste:
        ## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)

clc_o_ent$esttest = nrow(dbent)*clc_o_ent$facp ^ 2 
clc_o_ent$pvalor = teste(clc_o_ent$facp,nrow(dbent))
#View(clc_o_ent)

          # Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_ent$lag, clc_o_ent$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_ent$lag[2:25], clc_o_ent$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


# Desocupada: autocov, fac e facp

lag = c(0:24)
clc_d_ent=as.data.frame(lag)
head(clc_d_ent)

    ## Calculo autocov dos pseudoerros (Ch)

clc_d_ent$Ch1 = Pcov2(dbent$pseudo1_desocupada, lag = lags + 1)
clc_d_ent$Ch2 = Pcov2(dbent$pseudo2_desocupada, lag = lags + 1)
clc_d_ent$Ch3 = Pcov2(dbent$pseudo3_desocupada, lag = lags + 1)
clc_d_ent$Ch4 = Pcov2(dbent$pseudo4_desocupada, lag = lags + 1)
clc_d_ent$Ch5 = Pcov2(dbent$pseudo5_desocupada, lag = lags + 1)

View(clc_d_ent)

    # Soma das autocovs dos pseudo-erros:

clc_d_ent$SomaChk = clc_d_ent$Ch1 + clc_d_ent$Ch2 +clc_d_ent$Ch3 +clc_d_ent$Ch4 +clc_d_ent$Ch5
clc_d_ent$autocov = clc_d_ent$SomaChk/(K^2-K)

    # FAC
clc_d_ent$fac = clc_d_ent$SomaChk/clc_d_ent$SomaChk[1]
View(clc_d_ent)

    # FACP
clc_d_ent$facp = 0 
clc_d_ent$facp[2:25] = facp_acf(clc_d_ent$fac,lags)
View(clc_d_ent)

    # Estatísticas de teste:
      ## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_d_ent$esttest = nrow(dbent)*clc_d_ent$facp ^ 2 
clc_d_ent$pvalor = teste(clc_d_ent$facp,nrow(dbent))
View(clc_d_ent)

    # Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_ent$lag, clc_d_ent$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_ent$lag[2:25], clc_d_ent$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


### Parâmetros para os modelos

rho1_d_ent <- clc_d_ent$fac[2]
rho2_d_ent <- clc_d_ent$fac[3]

rho1_o_ent <- clc_o_ent$fac[2]
rho2_o_ent <- clc_o_ent$fac[3]

#AR(1)
phi1_ar1_dent = clc_d_ent$fac[lag==1]
phi1_ar1_oent = clc_o_ent$fac[lag==1]

par_ar1_ent<- data.frame(phi1_ar1_dent, phi1_ar1_oent)

#MA(1):
theta1_ma1_dent <- (1 - sqrt(1 - 4 * rho1_d_ent^2)) / (2 * rho1_d_ent)

theta1_ma1_oent <- (1 - sqrt(1 - 4 * rho1_o_ent^2)) / (2 * rho1_o_ent)

par_ma1_ent<-data.frame(theta1_ma1_dent,theta1_ma1_oent)

#AR(2)

phi1_ar2_dent <- (rho1_d_ent - rho1_d_ent * rho2_d_ent) / (1 - rho1_d_ent^2)
phi2_ar2_dent <- (rho2_d_ent - rho1_d_ent^2) / (1 - rho1_d_ent^2)

phi1_ar2_oent <- (rho1_o_ent - rho1_o_ent * rho2_o_ent) / (1 - rho1_o_ent^2)
phi2_ar2_oent <- (rho2_o_ent - rho1_o_ent^2) / (1 - rho1_o_ent^2)

par_ar2_ent<-data.frame(phi1_ar2_dent,phi2_ar2_dent,phi1_ar2_oent,phi2_ar2_oent)

#MA(2) - apenas desocupados

rho1 <- rho1_d_ent
rho2 <- rho2_d_ent
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

par_ma2_ent<-data.frame("theta1_ma2_dent"=solucao$x[1],"theta2_ma2_dent"=solucao$x[2])

#ARMA (1,1) - apenas desocupados

phi1_arma11_dent <- rho2_d_ent/rho1_d_ent

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  resultado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- resultado_theta1$root
  return(theta1)
}

rho1 <- rho1_d_ent
phi1 <- phi1_arma11_dent

theta1_arma11_dent <- arma11_theta1(rho1, phi1)

par_arma11_ent <-data.frame(phi1_arma11_dent,theta1_arma11_dent)

params_ent <- list("dbent"=dbent,"calculos_desocupada_ent"=clc_d_ent,
                   "calculos_ocupada_ent"=  clc_o_ent, 
                   "mod_ar1" = par_ar1_ent, "mod_ar2"=par_ar2_ent,
                   "mod_ma1"=par_ma1_ent,"mod_ma2"=par_ma2_ent, "mod_arma11"=par_arma11_ent)

saveRDS(params_ent,file = "D:/FJP2425/Programacao/data/pseudoerros/02_params_ent.rds")

### 03-COLAR METROPOLITANO DE BELO HORIZONTE####################################

dbcol<-base$`03-Colar metropolitano de BH`

## Definindo variáveis adicionais

colnames(dbcol)
t = c(1:nrow(dbcol))
lags = 24
T = nrow(dbcol)
K = (ncol(dbcol)-11)/2 

# Valor médio dos painéis

dbcol$media_ocupada = dbcol %>%  select(starts_with("ocupada")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)
dbcol$media_desocupada = dbcol %>%  select(starts_with("desocupada")) %>%  
  replace(.==0, NA) %>%  rowMeans(.,na.rm = TRUE)

View(dbcol) #Verificação

# Matriz de pseudo erros

dbcol$pseudo1_ocupada = dbcol$ocupada_1 - dbcol$media_ocupada
dbcol$pseudo2_ocupada = dbcol$ocupada_2 - dbcol$media_ocupada
dbcol$pseudo3_ocupada = dbcol$ocupada_3 - dbcol$media_ocupada
dbcol$pseudo4_ocupada = dbcol$ocupada_4 - dbcol$media_ocupada
dbcol$pseudo5_ocupada = dbcol$ocupada_5 - dbcol$media_ocupada

dbcol$pseudo1_desocupada = dbcol$desocupada_1 - dbcol$media_desocupada
dbcol$pseudo2_desocupada = dbcol$desocupada_2 - dbcol$media_desocupada
dbcol$pseudo3_desocupada = dbcol$desocupada_3 - dbcol$media_desocupada
dbcol$pseudo4_desocupada = dbcol$desocupada_4 - dbcol$media_desocupada
dbcol$pseudo5_desocupada = dbcol$desocupada_5 - dbcol$media_desocupada

View(dbcol)

#### Ocupada: Autocov; FAC e FACP

lag = c(0:24)
clc_o_col=as.data.frame(lag)
head(clc_o_col)

## Calculo autocov dos pseudoerros (Ch)

clc_o_col$Ch1 = Pcov2(dbcol$pseudo1_ocupada, lag = lags + 1)
clc_o_col$Ch2 = Pcov2(dbcol$pseudo2_ocupada, lag = lags + 1)
clc_o_col$Ch3 = Pcov2(dbcol$pseudo3_ocupada, lag = lags + 1)
clc_o_col$Ch4 = Pcov2(dbcol$pseudo4_ocupada, lag = lags + 1)
clc_o_col$Ch5 = Pcov2(dbcol$pseudo5_ocupada, lag = lags + 1)

View(clc_o_col)

# Soma das autocovs dos pseudo-erros:

clc_o_col$SomaChk = clc_o_col$Ch1 + clc_o_col$Ch2 +clc_o_col$Ch3 +clc_o_col$Ch4 +clc_o_col$Ch5
clc_o_col$autocov = clc_o_col$SomaChk/(K^2-K)

# FAC
clc_o_col$fac = clc_o_col$SomaChk/clc_o_col$SomaChk[1]

# FACP
clc_o_col$facp = 0 
clc_o_col$facp[2:25] = facp_acf(clc_o_col$fac,lags)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)

clc_o_col$esttest = nrow(dbcol)*clc_o_col$facp ^ 2 
clc_o_col$pvalor = teste(clc_o_col$facp,nrow(dbcol))
View(clc_o_col)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_col$lag, clc_o_col$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_col$lag[2:25], clc_o_col$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


#### Desocupada: autocov, fac e facp

lag = c(0:24)
clc_d_col=as.data.frame(lag)
head(clc_d_col)

## Calculo autocov dos pseudoerros (Ch)

clc_d_col$Ch1 = Pcov2(dbcol$pseudo1_desocupada, lag = lags + 1)
clc_d_col$Ch2 = Pcov2(dbcol$pseudo2_desocupada, lag = lags + 1)
clc_d_col$Ch3 = Pcov2(dbcol$pseudo3_desocupada, lag = lags + 1)
clc_d_col$Ch4 = Pcov2(dbcol$pseudo4_desocupada, lag = lags + 1)
clc_d_col$Ch5 = Pcov2(dbcol$pseudo5_desocupada, lag = lags + 1)

View(clc_d_col)

# Soma das autocovs dos pseudo-erros:

clc_d_col$SomaChk = clc_d_col$Ch1 + clc_d_col$Ch2 +clc_d_col$Ch3 +clc_d_col$Ch4 +clc_d_col$Ch5
clc_d_col$autocov = clc_d_col$SomaChk/(K^2-K)

# FAC
clc_d_col$fac = clc_d_col$SomaChk/clc_d_col$SomaChk[1]
View(clc_d_col)

# FACP
clc_d_col$facp = 0 
clc_d_col$facp[2:25] = facp_acf(clc_d_col$fac,lags)
View(clc_d_col)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_d_col$esttest = nrow(dbcol)*clc_d_col$facp ^ 2 
clc_d_col$pvalor = teste(clc_d_col$facp,nrow(dbcol))
View(clc_d_col)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_col$lag, clc_d_col$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_col$lag[2:25], clc_d_col$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


### Parâmetros para os modelos

rho1_d_col <- clc_d_col$fac[2]
rho2_d_col <- clc_d_col$fac[3]

rho1_o_col <- clc_o_col$fac[2]
rho2_o_col <- clc_o_col$fac[3]

#AR(1)
phi1_ar1_dcol = clc_d_col$fac[lag==1]
phi1_ar1_ocol = clc_o_col$fac[lag==1]

par_ar1_col<- data.frame(phi1_ar1_dcol, phi1_ar1_ocol)

#MA(1):
theta1_ma1_dcol <- (1 - sqrt(1 - 4 * rho1_d_col^2)) / (2 * rho1_d_col)

theta1_ma1_ocol <- (1 - sqrt(1 - 4 * rho1_o_col^2)) / (2 * rho1_o_col)

par_ma1_col<-data.frame(theta1_ma1_dcol,theta1_ma1_ocol)

#AR(2)

phi1_ar2_dcol <- (rho1_d_col - rho1_d_col * rho2_d_col) / (1 - rho1_d_col^2)
phi2_ar2_dcol <- (rho2_d_col - rho1_d_col^2) / (1 - rho1_d_col^2)

phi1_ar2_ocol <- (rho1_o_col - rho1_o_col * rho2_o_col) / (1 - rho1_o_col^2)
phi2_ar2_ocol <- (rho2_o_col - rho1_o_col^2) / (1 - rho1_o_col^2)

par_ar2_col<-data.frame(phi1_ar2_dcol,phi2_ar2_dcol,phi1_ar2_ocol,phi2_ar2_ocol)

#MA(2) - apenas desocupados

rho1 <- rho1_d_col
rho2 <- rho2_d_col
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

par_ma2_col<-data.frame("theta1_ma2_dcol"=solucao$x[1],"theta2_ma2_dcol"=solucao$x[2])

#ARMA (1,1) - apenas desocupados

phi1_arma11_dcol <- rho2_d_col/rho1_d_col

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  resultado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- resultado_theta1$root
  return(theta1)
}

rho1 <- rho1_d_col
phi1 <- phi1_arma11_dcol

theta1_arma11_dcol <- arma11_theta1(rho1, phi1)

par_arma11_col <-data.frame(phi1_arma11_dcol,theta1_arma11_dcol)

params_col <- list("dbcol"=dbcol,"calculos_desocupada_col"=clc_d_col,
                    "calculos_ocupada_col"=  clc_o_col, 
                   "mod_ar1" = par_ar1_col, "mod_ar2"=par_ar2_col,
                   "mod_ma1"=par_ma1_col,"mod_ma2"=par_ma2_col, "mod_arma11"=par_arma11_col)

saveRDS(params_col,file = "D:/FJP2425/Programacao/data/pseudoerros/03_params_col.rds")

### 04-RIDE ####################################################################

dbrid<-base$`04-RIDE de Brasília em Minas`

## Definindo variáveis adicionais

colnames(dbrid)
t = c(1:nrow(dbrid))
lags = 24
T = nrow(dbrid)
K = (ncol(dbrid)-11)/2 

# Valor médio dos painéis

dbrid$media_ocupada = dbrid %>%  select(starts_with("ocupada")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)
dbrid$media_desocupada = dbrid %>%  select(starts_with("desocupada")) %>%  
  replace(.==0, NA) %>%  rowMeans(.,na.rm = TRUE)

View(dbrid) #Verificação

# Matriz de pseudo erros

dbrid$pseudo1_ocupada = dbrid$ocupada_1 - dbrid$media_ocupada
dbrid$pseudo2_ocupada = dbrid$ocupada_2 - dbrid$media_ocupada
dbrid$pseudo3_ocupada = dbrid$ocupada_3 - dbrid$media_ocupada
dbrid$pseudo4_ocupada = dbrid$ocupada_4 - dbrid$media_ocupada
dbrid$pseudo5_ocupada = dbrid$ocupada_5 - dbrid$media_ocupada

dbrid$pseudo1_desocupada = dbrid$desocupada_1 - dbrid$media_desocupada
dbrid$pseudo2_desocupada = dbrid$desocupada_2 - dbrid$media_desocupada
dbrid$pseudo3_desocupada = dbrid$desocupada_3 - dbrid$media_desocupada
dbrid$pseudo4_desocupada = dbrid$desocupada_4 - dbrid$media_desocupada
dbrid$pseudo5_desocupada = dbrid$desocupada_5 - dbrid$media_desocupada

View(dbrid)

#### Ocupada: Autocov; FAC e FACP

lag = c(0:24)
clc_o_rid=as.data.frame(lag)
head(clc_o_rid)

## Calculo autocov dos pseudoerros (Ch)

clc_o_rid$Ch1 = Pcov2(dbrid$pseudo1_ocupada, lag = lags + 1)
clc_o_rid$Ch2 = Pcov2(dbrid$pseudo2_ocupada, lag = lags + 1)
clc_o_rid$Ch3 = Pcov2(dbrid$pseudo3_ocupada, lag = lags + 1)
clc_o_rid$Ch4 = Pcov2(dbrid$pseudo4_ocupada, lag = lags + 1)
clc_o_rid$Ch5 = Pcov2(dbrid$pseudo5_ocupada, lag = lags + 1)

View(clc_o_rid)

# Soma das autocovs dos pseudo-erros:

clc_o_rid$SomaChk = clc_o_rid$Ch1 + clc_o_rid$Ch2 +clc_o_rid$Ch3 +clc_o_rid$Ch4 +clc_o_rid$Ch5
clc_o_rid$autocov = clc_o_rid$SomaChk/(K^2-K)

# FAC
clc_o_rid$fac = clc_o_rid$SomaChk/clc_o_rid$SomaChk[1]

# FACP
clc_o_rid$facp = 0 
clc_o_rid$facp[2:25] = facp_acf(clc_o_rid$fac,lags)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)

clc_o_rid$esttest = nrow(dbrid)*clc_o_rid$facp ^ 2 
clc_o_rid$pvalor = teste(clc_o_rid$facp,nrow(dbrid))
View(clc_o_rid)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_rid$lag, clc_o_rid$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_rid$lag[2:25], clc_o_rid$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


#### Desocupada: autocov, fac e facp

lag = c(0:24)
clc_d_rid=as.data.frame(lag)
head(clc_d_rid)

## Calculo autocov dos pseudoerros (Ch)

clc_d_rid$Ch1 = Pcov2(dbrid$pseudo1_desocupada, lag = lags + 1)
clc_d_rid$Ch2 = Pcov2(dbrid$pseudo2_desocupada, lag = lags + 1)
clc_d_rid$Ch3 = Pcov2(dbrid$pseudo3_desocupada, lag = lags + 1)
clc_d_rid$Ch4 = Pcov2(dbrid$pseudo4_desocupada, lag = lags + 1)
clc_d_rid$Ch5 = Pcov2(dbrid$pseudo5_desocupada, lag = lags + 1)

View(clc_d_rid)

# Soma das autocovs dos pseudo-erros:

clc_d_rid$SomaChk = clc_d_rid$Ch1 + clc_d_rid$Ch2 +clc_d_rid$Ch3 +clc_d_rid$Ch4 +clc_d_rid$Ch5
clc_d_rid$autocov = clc_d_rid$SomaChk/(K^2-K)

# FAC
clc_d_rid$fac = clc_d_rid$SomaChk/clc_d_rid$SomaChk[1]
View(clc_d_rid)

# FACP
clc_d_rid$facp = 0 
clc_d_rid$facp[2:25] = facp_acf(clc_d_rid$fac,lags)
View(clc_d_rid)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_d_rid$esttest = nrow(dbrid)*clc_d_rid$facp ^ 2 
clc_d_rid$pvalor = teste(clc_d_rid$facp,nrow(dbrid))
View(clc_d_rid)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_rid$lag, clc_d_rid$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_rid$lag[2:25], clc_d_rid$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


### Parâmetros para os modelos

rho1_d_rid <- clc_d_rid$fac[2]
rho2_d_rid <- clc_d_rid$fac[3]

rho1_o_rid <- clc_o_rid$fac[2]
rho2_o_rid <- clc_o_rid$fac[3]

#AR(1)
phi1_ar1_drid = clc_d_rid$fac[lag==1]
phi1_ar1_orid = clc_o_rid$fac[lag==1]

par_ar1_rid<- data.frame(phi1_ar1_drid, phi1_ar1_orid)

#MA(1):
theta1_ma1_drid <- (1 - sqrt(1 - 4 * rho1_d_rid^2)) / (2 * rho1_d_rid)

theta1_ma1_orid <- (1 - sqrt(1 - 4 * rho1_o_rid^2)) / (2 * rho1_o_rid)

par_ma1_rid<-data.frame(theta1_ma1_drid,theta1_ma1_orid)

#AR(2)

phi1_ar2_drid <- (rho1_d_rid - rho1_d_rid * rho2_d_rid) / (1 - rho1_d_rid^2)
phi2_ar2_drid <- (rho2_d_rid - rho1_d_rid^2) / (1 - rho1_d_rid^2)

phi1_ar2_orid <- (rho1_o_rid - rho1_o_rid * rho2_o_rid) / (1 - rho1_o_rid^2)
phi2_ar2_orid <- (rho2_o_rid - rho1_o_rid^2) / (1 - rho1_o_rid^2)

par_ar2_rid<-data.frame(phi1_ar2_drid,phi2_ar2_drid,phi1_ar2_orid,phi2_ar2_orid)

#MA(2) - apenas desocupados

rho1 <- rho1_d_rid
rho2 <- rho2_d_rid
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

par_ma2_rid<-data.frame("theta1_ma2_drid"=solucao$x[1],"theta2_ma2_drid"=solucao$x[2])

#ARMA (1,1) - apenas desocupados

phi1_arma11_drid <- rho2_d_rid/rho1_d_rid

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  resultado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- resultado_theta1$root
  return(theta1)
}

rho1 <- rho1_d_rid
phi1 <- phi1_arma11_drid

theta1_arma11_drid <- arma11_theta1(rho1, phi1)

par_arma11_rid <-data.frame(phi1_arma11_drid,theta1_arma11_drid)

params_rid <- list("dbrid"=dbrid,"calculos_desocupada_rid"=clc_d_rid,
                    "calculos_ocupada_rid"=  clc_o_rid, 
                   "mod_ar1" = par_ar1_rid, "mod_ar2"=par_ar2_rid,
                   "mod_ma1"=par_ma1_rid,"mod_ma2"=par_ma2_rid, "mod_arma11"=par_arma11_rid)

saveRDS(params_rid,file = "D:/FJP2425/Programacao/data/pseudoerros/04_params_rid.rds")

### 05-SUL DE MINAS ############################################################

dbsul<-base$`05-Sul de Minas`

## Definindo variáveis adicionais

colnames(dbsul)
t = c(1:nrow(dbsul))
lags = 24
T = nrow(dbsul)
K = (ncol(dbsul)-11)/2 

# Valor médio dos painéis

dbsul$media_ocupada = dbsul %>%  select(starts_with("ocupada")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)
dbsul$media_desocupada = dbsul %>%  select(starts_with("desocupada")) %>%  
  replace(.==0, NA) %>%  rowMeans(.,na.rm = TRUE)

View(dbsul) #Verificação

# Matriz de pseudo erros

dbsul$pseudo1_ocupada = dbsul$ocupada_1 - dbsul$media_ocupada
dbsul$pseudo2_ocupada = dbsul$ocupada_2 - dbsul$media_ocupada
dbsul$pseudo3_ocupada = dbsul$ocupada_3 - dbsul$media_ocupada
dbsul$pseudo4_ocupada = dbsul$ocupada_4 - dbsul$media_ocupada
dbsul$pseudo5_ocupada = dbsul$ocupada_5 - dbsul$media_ocupada

dbsul$pseudo1_desocupada = dbsul$desocupada_1 - dbsul$media_desocupada
dbsul$pseudo2_desocupada = dbsul$desocupada_2 - dbsul$media_desocupada
dbsul$pseudo3_desocupada = dbsul$desocupada_3 - dbsul$media_desocupada
dbsul$pseudo4_desocupada = dbsul$desocupada_4 - dbsul$media_desocupada
dbsul$pseudo5_desocupada = dbsul$desocupada_5 - dbsul$media_desocupada

View(dbsul)

#### Ocupada: Autocov; FAC e FACP

lag = c(0:24)
clc_o_sul=as.data.frame(lag)
head(clc_o_sul)

## Calculo autocov dos pseudoerros (Ch)

clc_o_sul$Ch1 = Pcov2(dbsul$pseudo1_ocupada, lag = lags + 1)
clc_o_sul$Ch2 = Pcov2(dbsul$pseudo2_ocupada, lag = lags + 1)
clc_o_sul$Ch3 = Pcov2(dbsul$pseudo3_ocupada, lag = lags + 1)
clc_o_sul$Ch4 = Pcov2(dbsul$pseudo4_ocupada, lag = lags + 1)
clc_o_sul$Ch5 = Pcov2(dbsul$pseudo5_ocupada, lag = lags + 1)

View(clc_o_sul)

# Soma das autocovs dos pseudo-erros:

clc_o_sul$SomaChk = clc_o_sul$Ch1 + clc_o_sul$Ch2 +clc_o_sul$Ch3 +clc_o_sul$Ch4 +clc_o_sul$Ch5
clc_o_sul$autocov = clc_o_sul$SomaChk/(K^2-K)

# FAC
clc_o_sul$fac = clc_o_sul$SomaChk/clc_o_sul$SomaChk[1]

# FACP
clc_o_sul$facp = 0 
clc_o_sul$facp[2:25] = facp_acf(clc_o_sul$fac,lags)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)

clc_o_sul$esttest = nrow(dbsul)*clc_o_sul$facp ^ 2 
clc_o_sul$pvalor = teste(clc_o_sul$facp,nrow(dbsul))
View(clc_o_sul)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_sul$lag, clc_o_sul$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_sul$lag[2:25], clc_o_sul$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


#### Desocupada: autocov, fac e facp

lag = c(0:24)
clc_d_sul=as.data.frame(lag)
head(clc_d_sul)

## Calculo autocov dos pseudoerros (Ch)

clc_d_sul$Ch1 = Pcov2(dbsul$pseudo1_desocupada, lag = lags + 1)
clc_d_sul$Ch2 = Pcov2(dbsul$pseudo2_desocupada, lag = lags + 1)
clc_d_sul$Ch3 = Pcov2(dbsul$pseudo3_desocupada, lag = lags + 1)
clc_d_sul$Ch4 = Pcov2(dbsul$pseudo4_desocupada, lag = lags + 1)
clc_d_sul$Ch5 = Pcov2(dbsul$pseudo5_desocupada, lag = lags + 1)

View(clc_d_sul)

# Soma das autocovs dos pseudo-erros:

clc_d_sul$SomaChk = clc_d_sul$Ch1 + clc_d_sul$Ch2 +clc_d_sul$Ch3 +clc_d_sul$Ch4 +clc_d_sul$Ch5
clc_d_sul$autocov = clc_d_sul$SomaChk/(K^2-K)

# FAC
clc_d_sul$fac = clc_d_sul$SomaChk/clc_d_sul$SomaChk[1]
View(clc_d_sul)

# FACP
clc_d_sul$facp = 0 
clc_d_sul$facp[2:25] = facp_acf(clc_d_sul$fac,lags)
View(clc_d_sul)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_d_sul$esttest = nrow(dbsul)*clc_d_sul$facp ^ 2 
clc_d_sul$pvalor = teste(clc_d_sul$facp,nrow(dbsul))
View(clc_d_sul)

# Plots


{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_sul$lag, clc_d_sul$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_sul$lag[2:25], clc_d_sul$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


### Parâmetros para os modelos

rho1_d_sul <- clc_d_sul$fac[2]
rho2_d_sul <- clc_d_sul$fac[3]

rho1_o_sul <- clc_o_sul$fac[2]
rho2_o_sul <- clc_o_sul$fac[3]

#AR(1)
phi1_ar1_dsul = clc_d_sul$fac[lag==1]
phi1_ar1_osul = clc_o_sul$fac[lag==1]

par_ar1_sul<- data.frame(phi1_ar1_dsul, phi1_ar1_osul)

#MA(1):
theta1_ma1_dsul <- (1 - sqrt(1 - 4 * rho1_d_sul^2)) / (2 * rho1_d_sul)

theta1_ma1_osul <- (1 - sqrt(1 - 4 * rho1_o_sul^2)) / (2 * rho1_o_sul)

par_ma1_sul<-data.frame(theta1_ma1_dsul,theta1_ma1_osul)

#AR(2)

phi1_ar2_dsul <- (rho1_d_sul - rho1_d_sul * rho2_d_sul) / (1 - rho1_d_sul^2)
phi2_ar2_dsul <- (rho2_d_sul - rho1_d_sul^2) / (1 - rho1_d_sul^2)

phi1_ar2_osul <- (rho1_o_sul - rho1_o_sul * rho2_o_sul) / (1 - rho1_o_sul^2)
phi2_ar2_osul <- (rho2_o_sul - rho1_o_sul^2) / (1 - rho1_o_sul^2)

par_ar2_sul<-data.frame(phi1_ar2_dsul,phi2_ar2_dsul,phi1_ar2_osul,phi2_ar2_osul)

#MA(2) - apenas desocupados

rho1 <- rho1_d_sul
rho2 <- rho2_d_sul
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

par_ma2_sul<-data.frame("theta1_ma2_dsul"=solucao$x[1],"theta2_ma2_dsul"=solucao$x[2])

#ARMA (1,1) - apenas desocupados

phi1_arma11_dsul <- rho2_d_sul/rho1_d_sul

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  resultado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- resultado_theta1$root
  return(theta1)
}

rho1 <- rho1_d_sul
phi1 <- phi1_arma11_dsul

theta1_arma11_dsul <- arma11_theta1(rho1, phi1)

par_arma11_sul <-data.frame(phi1_arma11_dsul,theta1_arma11_dsul)

params_sul <- list("dbsul"=dbsul,"calculos_desocupada_sul"=clc_d_sul,
                    "calculos_ocupada_sul"=  clc_o_sul, 
                   "mod_ar1" = par_ar1_sul, "mod_ar2"=par_ar2_sul,
                   "mod_ma1"=par_ma1_sul,"mod_ma2"=par_ma2_sul, "mod_arma11"=par_arma11_sul)

saveRDS(params_sul,file = "D:/FJP2425/Programacao/data/pseudoerros/05_params_sul.rds")

### 06-TRIÂNGULO MINEIRO #######################################################

dbtrg<-base$`06-Triângulo Mineiro`

## Definindo variáveis adicionais

colnames(dbtrg)
t = c(1:nrow(dbtrg))
lags = 24
T = nrow(dbtrg)
K = (ncol(dbtrg)-11)/2 

# Valor médio dos painéis

dbtrg$media_ocupada = dbtrg %>%  select(starts_with("ocupada")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)
dbtrg$media_desocupada = dbtrg %>%  select(starts_with("desocupada")) %>%  
  replace(.==0, NA) %>%  rowMeans(.,na.rm = TRUE)

View(dbtrg) #Verificação

# Matriz de pseudo erros

dbtrg$pseudo1_ocupada = dbtrg$ocupada_1 - dbtrg$media_ocupada
dbtrg$pseudo2_ocupada = dbtrg$ocupada_2 - dbtrg$media_ocupada
dbtrg$pseudo3_ocupada = dbtrg$ocupada_3 - dbtrg$media_ocupada
dbtrg$pseudo4_ocupada = dbtrg$ocupada_4 - dbtrg$media_ocupada
dbtrg$pseudo5_ocupada = dbtrg$ocupada_5 - dbtrg$media_ocupada

dbtrg$pseudo1_desocupada = dbtrg$desocupada_1 - dbtrg$media_desocupada
dbtrg$pseudo2_desocupada = dbtrg$desocupada_2 - dbtrg$media_desocupada
dbtrg$pseudo3_desocupada = dbtrg$desocupada_3 - dbtrg$media_desocupada
dbtrg$pseudo4_desocupada = dbtrg$desocupada_4 - dbtrg$media_desocupada
dbtrg$pseudo5_desocupada = dbtrg$desocupada_5 - dbtrg$media_desocupada

View(dbtrg)

#### Ocupada: Autocov; FAC e FACP

lag = c(0:24)
clc_o_trg=as.data.frame(lag)
head(clc_o_trg)

## Calculo autocov dos pseudoerros (Ch)

clc_o_trg$Ch1 = Pcov2(dbtrg$pseudo1_ocupada, lag = lags + 1)
clc_o_trg$Ch2 = Pcov2(dbtrg$pseudo2_ocupada, lag = lags + 1)
clc_o_trg$Ch3 = Pcov2(dbtrg$pseudo3_ocupada, lag = lags + 1)
clc_o_trg$Ch4 = Pcov2(dbtrg$pseudo4_ocupada, lag = lags + 1)
clc_o_trg$Ch5 = Pcov2(dbtrg$pseudo5_ocupada, lag = lags + 1)

View(clc_o_trg)

# Soma das autocovs dos pseudo-erros:

clc_o_trg$SomaChk = clc_o_trg$Ch1 + clc_o_trg$Ch2 +clc_o_trg$Ch3 +clc_o_trg$Ch4 +clc_o_trg$Ch5
clc_o_trg$autocov = clc_o_trg$SomaChk/(K^2-K)

# FAC
clc_o_trg$fac = clc_o_trg$SomaChk/clc_o_trg$SomaChk[1]

# FACP
clc_o_trg$facp = 0 
clc_o_trg$facp[2:25] = facp_acf(clc_o_trg$fac,lags)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)

clc_o_trg$esttest = nrow(dbtrg)*clc_o_trg$facp ^ 2 
clc_o_trg$pvalor = teste(clc_o_trg$facp,nrow(dbtrg))
View(clc_o_trg)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_trg$lag, clc_o_trg$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_trg$lag[2:25], clc_o_trg$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


#### Desocupada: autocov, fac e facp

lag = c(0:24)
clc_d_trg=as.data.frame(lag)
head(clc_d_trg)

## Calculo autocov dos pseudoerros (Ch)

clc_d_trg$Ch1 = Pcov2(dbtrg$pseudo1_desocupada, lag = lags + 1)
clc_d_trg$Ch2 = Pcov2(dbtrg$pseudo2_desocupada, lag = lags + 1)
clc_d_trg$Ch3 = Pcov2(dbtrg$pseudo3_desocupada, lag = lags + 1)
clc_d_trg$Ch4 = Pcov2(dbtrg$pseudo4_desocupada, lag = lags + 1)
clc_d_trg$Ch5 = Pcov2(dbtrg$pseudo5_desocupada, lag = lags + 1)

View(clc_d_trg)

# Soma das autocovs dos pseudo-erros:

clc_d_trg$SomaChk = clc_d_trg$Ch1 + clc_d_trg$Ch2 +clc_d_trg$Ch3 +clc_d_trg$Ch4 +clc_d_trg$Ch5
clc_d_trg$autocov = clc_d_trg$SomaChk/(K^2-K)

# FAC
clc_d_trg$fac = clc_d_trg$SomaChk/clc_d_trg$SomaChk[1]
View(clc_d_trg)

# FACP
clc_d_trg$facp = 0 
clc_d_trg$facp[2:25] = facp_acf(clc_d_trg$fac,lags)
View(clc_d_trg)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_d_trg$esttest = nrow(dbtrg)*clc_d_trg$facp ^ 2 
clc_d_trg$pvalor = teste(clc_d_trg$facp,nrow(dbtrg))
View(clc_d_trg)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_trg$lag, clc_d_trg$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_trg$lag[2:25], clc_d_trg$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


### Parâmetros para os modelos

rho1_d_trg <- clc_d_trg$fac[2]
rho2_d_trg <- clc_d_trg$fac[3]

rho1_o_trg <- clc_o_trg$fac[2]
rho2_o_trg <- clc_o_trg$fac[3]

#AR(1)
phi1_ar1_dtrg = clc_d_trg$fac[lag==1]
phi1_ar1_otrg = clc_o_trg$fac[lag==1]

par_ar1_trg<- data.frame(phi1_ar1_dtrg, phi1_ar1_otrg)

#MA(1):
theta1_ma1_dtrg <- (1 - sqrt(1 - 4 * rho1_d_trg^2)) / (2 * rho1_d_trg)

theta1_ma1_otrg <- (1 - sqrt(1 - 4 * rho1_o_trg^2)) / (2 * rho1_o_trg)

par_ma1_trg<-data.frame(theta1_ma1_dtrg,theta1_ma1_otrg)

#AR(2)

phi1_ar2_dtrg <- (rho1_d_trg - rho1_d_trg * rho2_d_trg) / (1 - rho1_d_trg^2)
phi2_ar2_dtrg <- (rho2_d_trg - rho1_d_trg^2) / (1 - rho1_d_trg^2)

phi1_ar2_otrg <- (rho1_o_trg - rho1_o_trg * rho2_o_trg) / (1 - rho1_o_trg^2)
phi2_ar2_otrg <- (rho2_o_trg - rho1_o_trg^2) / (1 - rho1_o_trg^2)

par_ar2_trg<-data.frame(phi1_ar2_dtrg,phi2_ar2_dtrg,phi1_ar2_otrg,phi2_ar2_otrg)

#MA(2) - apenas desocupados

rho1 <- rho1_d_trg
rho2 <- rho2_d_trg
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

par_ma2_trg<-data.frame("theta1_ma2_dtrg"=solucao$x[1],"theta2_ma2_dtrg"=solucao$x[2])

#ARMA (1,1) - apenas desocupados

phi1_arma11_dtrg <- rho2_d_trg/rho1_d_trg

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  resultado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- resultado_theta1$root
  return(theta1)
}

rho1 <- rho1_d_trg
phi1 <- phi1_arma11_dtrg

theta1_arma11_dtrg <- arma11_theta1(rho1, phi1)

par_arma11_trg <-data.frame(phi1_arma11_dtrg,theta1_arma11_dtrg)

params_trg <- list("dbtrg"=dbtrg,"calculos_desocupada_trg"=clc_d_trg,
                    "calculos_ocupada_trg"=  clc_o_trg, 
                   "mod_ar1" = par_ar1_trg, "mod_ar2"=par_ar2_trg,
                   "mod_ma1"=par_ma1_trg,"mod_ma2"=par_ma2_trg, "mod_arma11"=par_arma11_trg)

saveRDS(params_trg,file = "D:/FJP2425/Programacao/data/pseudoerros/06_params_trg.rds")

### 07-ZONA DA MATA ############################################################

dbmat<-base$`07-Mata de Minas Gerais`

## Definindo variáveis adicionais

colnames(dbmat)
t = c(1:nrow(dbmat))
lags = 24
T = nrow(dbmat)
K = (ncol(dbmat)-11)/2 

# Valor médio dos painéis

dbmat$media_ocupada = dbmat %>%  select(starts_with("ocupada")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)
dbmat$media_desocupada = dbmat %>%  select(starts_with("desocupada")) %>%  
  replace(.==0, NA) %>%  rowMeans(.,na.rm = TRUE)

View(dbmat) #Verificação

# Matriz de pseudo erros

dbmat$pseudo1_ocupada = dbmat$ocupada_1 - dbmat$media_ocupada
dbmat$pseudo2_ocupada = dbmat$ocupada_2 - dbmat$media_ocupada
dbmat$pseudo3_ocupada = dbmat$ocupada_3 - dbmat$media_ocupada
dbmat$pseudo4_ocupada = dbmat$ocupada_4 - dbmat$media_ocupada
dbmat$pseudo5_ocupada = dbmat$ocupada_5 - dbmat$media_ocupada

dbmat$pseudo1_desocupada = dbmat$desocupada_1 - dbmat$media_desocupada
dbmat$pseudo2_desocupada = dbmat$desocupada_2 - dbmat$media_desocupada
dbmat$pseudo3_desocupada = dbmat$desocupada_3 - dbmat$media_desocupada
dbmat$pseudo4_desocupada = dbmat$desocupada_4 - dbmat$media_desocupada
dbmat$pseudo5_desocupada = dbmat$desocupada_5 - dbmat$media_desocupada

View(dbmat)

#### Ocupada: Autocov; FAC e FACP

lag = c(0:24)
clc_o_mat=as.data.frame(lag)
head(clc_o_mat)

## Calculo autocov dos pseudoerros (Ch)

clc_o_mat$Ch1 = Pcov2(dbmat$pseudo1_ocupada, lag = lags + 1)
clc_o_mat$Ch2 = Pcov2(dbmat$pseudo2_ocupada, lag = lags + 1)
clc_o_mat$Ch3 = Pcov2(dbmat$pseudo3_ocupada, lag = lags + 1)
clc_o_mat$Ch4 = Pcov2(dbmat$pseudo4_ocupada, lag = lags + 1)
clc_o_mat$Ch5 = Pcov2(dbmat$pseudo5_ocupada, lag = lags + 1)

View(clc_o_mat)

# Soma das autocovs dos pseudo-erros:

clc_o_mat$SomaChk = clc_o_mat$Ch1 + clc_o_mat$Ch2 +clc_o_mat$Ch3 +clc_o_mat$Ch4 +clc_o_mat$Ch5
clc_o_mat$autocov = clc_o_mat$SomaChk/(K^2-K)

# FAC
clc_o_mat$fac = clc_o_mat$SomaChk/clc_o_mat$SomaChk[1]

# FACP
clc_o_mat$facp = 0 
clc_o_mat$facp[2:25] = facp_acf(clc_o_mat$fac,lags)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)

clc_o_mat$esttest = nrow(dbmat)*clc_o_mat$facp ^ 2 
clc_o_mat$pvalor = teste(clc_o_mat$facp,nrow(dbmat))
View(clc_o_mat)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_mat$lag, clc_o_mat$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_mat$lag[2:25], clc_o_mat$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


#### Desocupada: autocov, fac e facp

lag = c(0:24)
clc_d_mat=as.data.frame(lag)
head(clc_d_mat)

## Calculo autocov dos pseudoerros (Ch)

clc_d_mat$Ch1 = Pcov2(dbmat$pseudo1_desocupada, lag = lags + 1)
clc_d_mat$Ch2 = Pcov2(dbmat$pseudo2_desocupada, lag = lags + 1)
clc_d_mat$Ch3 = Pcov2(dbmat$pseudo3_desocupada, lag = lags + 1)
clc_d_mat$Ch4 = Pcov2(dbmat$pseudo4_desocupada, lag = lags + 1)
clc_d_mat$Ch5 = Pcov2(dbmat$pseudo5_desocupada, lag = lags + 1)

View(clc_d_mat)

# Soma das autocovs dos pseudo-erros:

clc_d_mat$SomaChk = clc_d_mat$Ch1 + clc_d_mat$Ch2 +clc_d_mat$Ch3 +clc_d_mat$Ch4 +clc_d_mat$Ch5
clc_d_mat$autocov = clc_d_mat$SomaChk/(K^2-K)

# FAC
clc_d_mat$fac = clc_d_mat$SomaChk/clc_d_mat$SomaChk[1]
View(clc_d_mat)

# FACP
clc_d_mat$facp = 0 
clc_d_mat$facp[2:25] = facp_acf(clc_d_mat$fac,lags)
View(clc_d_mat)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_d_mat$esttest = nrow(dbmat)*clc_d_mat$facp ^ 2 
clc_d_mat$pvalor = teste(clc_d_mat$facp,nrow(dbmat))
View(clc_d_mat)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_mat$lag, clc_d_mat$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_mat$lag[2:25], clc_d_mat$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


### Parâmetros para os modelos

rho1_d_mat <- clc_d_mat$fac[2]
rho2_d_mat <- clc_d_mat$fac[3]

rho1_o_mat <- clc_o_mat$fac[2]
rho2_o_mat <- clc_o_mat$fac[3]

#AR(1)
phi1_ar1_dmat = clc_d_mat$fac[lag==1]
phi1_ar1_omat = clc_o_mat$fac[lag==1]

par_ar1_mat<- data.frame(phi1_ar1_dmat, phi1_ar1_omat)

#MA(1):
theta1_ma1_dmat <- (1 - sqrt(1 - 4 * rho1_d_mat^2)) / (2 * rho1_d_mat)

theta1_ma1_omat <- (1 - sqrt(1 - 4 * rho1_o_mat^2)) / (2 * rho1_o_mat)

par_ma1_mat<-data.frame(theta1_ma1_dmat,theta1_ma1_omat)

#AR(2)

phi1_ar2_dmat <- (rho1_d_mat - rho1_d_mat * rho2_d_mat) / (1 - rho1_d_mat^2)
phi2_ar2_dmat <- (rho2_d_mat - rho1_d_mat^2) / (1 - rho1_d_mat^2)

phi1_ar2_omat <- (rho1_o_mat - rho1_o_mat * rho2_o_mat) / (1 - rho1_o_mat^2)
phi2_ar2_omat <- (rho2_o_mat - rho1_o_mat^2) / (1 - rho1_o_mat^2)

par_ar2_mat<-data.frame(phi1_ar2_dmat,phi2_ar2_dmat,phi1_ar2_omat,phi2_ar2_omat)

#MA(2) - apenas desocupados

rho1 <- rho1_d_mat
rho2 <- rho2_d_mat
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

par_ma2_mat<-data.frame("theta1_ma2_dmat"=solucao$x[1],"theta2_ma2_dmat"=solucao$x[2])

#ARMA (1,1) - apenas desocupados

phi1_arma11_dmat <- rho2_d_mat/rho1_d_mat

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  resultado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- resultado_theta1$root
  return(theta1)
}

rho1 <- rho1_d_mat
phi1 <- phi1_arma11_dmat

theta1_arma11_dmat <- arma11_theta1(rho1, phi1)

par_arma11_mat <-data.frame(phi1_arma11_dmat,theta1_arma11_dmat)

params_mat <- list("dbmat"=dbmat,"calculos_desocupada_mat"=clc_d_mat,
                    "calculos_ocupada_mat"=  clc_o_mat, 
                   "mod_ar1" = par_ar1_mat, "mod_ar2"=par_ar2_mat,
                   "mod_ma1"=par_ma1_mat,"mod_ma2"=par_ma2_mat, "mod_arma11"=par_arma11_mat)

saveRDS(params_mat,file = "D:/FJP2425/Programacao/data/pseudoerros/07_params_mat.rds")

### 08-NORTE DE MINAS GERAIS ###################################################

dbnrt<-base$`08-Norte de Minas`

## Definindo variáveis adicionais

colnames(dbnrt)
t = c(1:nrow(dbnrt))
lags = 24
T = nrow(dbnrt)
K = (ncol(dbnrt)-11)/2 

# Valor médio dos painéis

dbnrt$media_ocupada = dbnrt %>%  select(starts_with("ocupada")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)
dbnrt$media_desocupada = dbnrt %>%  select(starts_with("desocupada")) %>%  
  replace(.==0, NA) %>%  rowMeans(.,na.rm = TRUE)

View(dbnrt) #Verificação

# Matriz de pseudo erros

dbnrt$pseudo1_ocupada = dbnrt$ocupada_1 - dbnrt$media_ocupada
dbnrt$pseudo2_ocupada = dbnrt$ocupada_2 - dbnrt$media_ocupada
dbnrt$pseudo3_ocupada = dbnrt$ocupada_3 - dbnrt$media_ocupada
dbnrt$pseudo4_ocupada = dbnrt$ocupada_4 - dbnrt$media_ocupada
dbnrt$pseudo5_ocupada = dbnrt$ocupada_5 - dbnrt$media_ocupada

dbnrt$pseudo1_desocupada = dbnrt$desocupada_1 - dbnrt$media_desocupada
dbnrt$pseudo2_desocupada = dbnrt$desocupada_2 - dbnrt$media_desocupada
dbnrt$pseudo3_desocupada = dbnrt$desocupada_3 - dbnrt$media_desocupada
dbnrt$pseudo4_desocupada = dbnrt$desocupada_4 - dbnrt$media_desocupada
dbnrt$pseudo5_desocupada = dbnrt$desocupada_5 - dbnrt$media_desocupada

View(dbnrt)

#### Ocupada: Autocov; FAC e FACP

lag = c(0:24)
clc_o_nrt=as.data.frame(lag)
head(clc_o_nrt)

## Calculo autocov dos pseudoerros (Ch)

clc_o_nrt$Ch1 = Pcov2(dbnrt$pseudo1_ocupada, lag = lags + 1)
clc_o_nrt$Ch2 = Pcov2(dbnrt$pseudo2_ocupada, lag = lags + 1)
clc_o_nrt$Ch3 = Pcov2(dbnrt$pseudo3_ocupada, lag = lags + 1)
clc_o_nrt$Ch4 = Pcov2(dbnrt$pseudo4_ocupada, lag = lags + 1)
clc_o_nrt$Ch5 = Pcov2(dbnrt$pseudo5_ocupada, lag = lags + 1)

View(clc_o_nrt)

# Soma das autocovs dos pseudo-erros:

clc_o_nrt$SomaChk = clc_o_nrt$Ch1 + clc_o_nrt$Ch2 +clc_o_nrt$Ch3 +clc_o_nrt$Ch4 +clc_o_nrt$Ch5
clc_o_nrt$autocov = clc_o_nrt$SomaChk/(K^2-K)

# FAC
clc_o_nrt$fac = clc_o_nrt$SomaChk/clc_o_nrt$SomaChk[1]

# FACP
clc_o_nrt$facp = 0 
clc_o_nrt$facp[2:25] = facp_acf(clc_o_nrt$fac,lags)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)

clc_o_nrt$esttest = nrow(dbnrt)*clc_o_nrt$facp ^ 2 
clc_o_nrt$pvalor = teste(clc_o_nrt$facp,nrow(dbnrt))
View(clc_o_nrt)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_nrt$lag, clc_o_nrt$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_nrt$lag[2:25], clc_o_nrt$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


#### Desocupada: autocov, fac e facp

lag = c(0:24)
clc_d_nrt=as.data.frame(lag)
head(clc_d_nrt)

## Calculo autocov dos pseudoerros (Ch)

clc_d_nrt$Ch1 = Pcov2(dbnrt$pseudo1_desocupada, lag = lags + 1)
clc_d_nrt$Ch2 = Pcov2(dbnrt$pseudo2_desocupada, lag = lags + 1)
clc_d_nrt$Ch3 = Pcov2(dbnrt$pseudo3_desocupada, lag = lags + 1)
clc_d_nrt$Ch4 = Pcov2(dbnrt$pseudo4_desocupada, lag = lags + 1)
clc_d_nrt$Ch5 = Pcov2(dbnrt$pseudo5_desocupada, lag = lags + 1)

View(clc_d_nrt)

# Soma das autocovs dos pseudo-erros:

clc_d_nrt$SomaChk = clc_d_nrt$Ch1 + clc_d_nrt$Ch2 +clc_d_nrt$Ch3 +clc_d_nrt$Ch4 +clc_d_nrt$Ch5
clc_d_nrt$autocov = clc_d_nrt$SomaChk/(K^2-K)

# FAC
clc_d_nrt$fac = clc_d_nrt$SomaChk/clc_d_nrt$SomaChk[1]
View(clc_d_nrt)

# FACP
clc_d_nrt$facp = 0 
clc_d_nrt$facp[2:25] = facp_acf(clc_d_nrt$fac,lags)
View(clc_d_nrt)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_d_nrt$esttest = nrow(dbnrt)*clc_d_nrt$facp ^ 2 
clc_d_nrt$pvalor = teste(clc_d_nrt$facp,nrow(dbnrt))
View(clc_d_nrt)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_nrt$lag, clc_d_nrt$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_nrt$lag[2:25], clc_d_nrt$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}

### Parâmetros para os modelos

rho1_d_nrt <- clc_d_nrt$fac[2]
rho2_d_nrt <- clc_d_nrt$fac[3]

rho1_o_nrt <- clc_o_nrt$fac[2]
rho2_o_nrt <- clc_o_nrt$fac[3]

#AR(1)
phi1_ar1_dnrt = clc_d_nrt$fac[lag==1]
phi1_ar1_onrt = clc_o_nrt$fac[lag==1]

par_ar1_nrt<- data.frame(phi1_ar1_dnrt, phi1_ar1_onrt)

#MA(1):
theta1_ma1_dnrt <- (1 - sqrt(1 - 4 * rho1_d_nrt^2)) / (2 * rho1_d_nrt)

theta1_ma1_onrt <- (1 - sqrt(1 - 4 * rho1_o_nrt^2)) / (2 * rho1_o_nrt)

par_ma1_nrt<-data.frame(theta1_ma1_dnrt,theta1_ma1_onrt)

#AR(2)

phi1_ar2_dnrt <- (rho1_d_nrt - rho1_d_nrt * rho2_d_nrt) / (1 - rho1_d_nrt^2)
phi2_ar2_dnrt <- (rho2_d_nrt - rho1_d_nrt^2) / (1 - rho1_d_nrt^2)

phi1_ar2_onrt <- (rho1_o_nrt - rho1_o_nrt * rho2_o_nrt) / (1 - rho1_o_nrt^2)
phi2_ar2_onrt <- (rho2_o_nrt - rho1_o_nrt^2) / (1 - rho1_o_nrt^2)

par_ar2_nrt<-data.frame(phi1_ar2_dnrt,phi2_ar2_dnrt,phi1_ar2_onrt,phi2_ar2_onrt)

#MA(2) - apenas desocupados

rho1 <- rho1_d_nrt
rho2 <- rho2_d_nrt
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

par_ma2_nrt<-data.frame("theta1_ma2_dnrt"=solucao$x[1],"theta2_ma2_dnrt"=solucao$x[2])

#ARMA (1,1) - apenas desocupados

phi1_arma11_dnrt <- rho2_d_nrt/rho1_d_nrt

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  resultado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- resultado_theta1$root
  return(theta1)
}

rho1 <- rho1_d_nrt
phi1 <- phi1_arma11_dnrt

theta1_arma11_dnrt <- arma11_theta1(rho1, phi1)

par_arma11_nrt <-data.frame(phi1_arma11_dnrt,theta1_arma11_dnrt)

params_nrt <- list("dbnrt"=dbnrt,"calculos_desocupada_nrt"=clc_d_nrt,
                    "calculos_ocupada_nrt"=  clc_o_nrt, 
                   "mod_ar1" = par_ar1_nrt, "mod_ar2"=par_ar2_nrt,
                   "mod_ma1"=par_ma1_nrt,"mod_ma2"=par_ma2_nrt, "mod_arma11"=par_arma11_nrt)

saveRDS(params_nrt,file = "D:/FJP2425/Programacao/data/pseudoerros/08_params_nrt.rds")

### 09-VALE DO RIO DOCE ########################################################

dbrio<-base$`09-Vale do Rio Doce`

## Definindo variáveis adicionais

colnames(dbrio)
t = c(1:nrow(dbrio))
lags = 24
T = nrow(dbrio)
K = (ncol(dbrio)-11)/2 

# Valor médio dos painéis

dbrio$media_ocupada = dbrio %>%  select(starts_with("ocupada")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)
dbrio$media_desocupada = dbrio %>%  select(starts_with("desocupada")) %>%  
  replace(.==0, NA) %>%  rowMeans(.,na.rm = TRUE)

View(dbrio) #Verificação

# Matriz de pseudo erros

dbrio$pseudo1_ocupada = dbrio$ocupada_1 - dbrio$media_ocupada
dbrio$pseudo2_ocupada = dbrio$ocupada_2 - dbrio$media_ocupada
dbrio$pseudo3_ocupada = dbrio$ocupada_3 - dbrio$media_ocupada
dbrio$pseudo4_ocupada = dbrio$ocupada_4 - dbrio$media_ocupada
dbrio$pseudo5_ocupada = dbrio$ocupada_5 - dbrio$media_ocupada

dbrio$pseudo1_desocupada = dbrio$desocupada_1 - dbrio$media_desocupada
dbrio$pseudo2_desocupada = dbrio$desocupada_2 - dbrio$media_desocupada
dbrio$pseudo3_desocupada = dbrio$desocupada_3 - dbrio$media_desocupada
dbrio$pseudo4_desocupada = dbrio$desocupada_4 - dbrio$media_desocupada
dbrio$pseudo5_desocupada = dbrio$desocupada_5 - dbrio$media_desocupada

View(dbrio)

#### Ocupada: Autocov; FAC e FACP

lag = c(0:24)
clc_o_rio=as.data.frame(lag)
head(clc_o_rio)

## Calculo autocov dos pseudoerros (Ch)

clc_o_rio$Ch1 = Pcov2(dbrio$pseudo1_ocupada, lag = lags + 1)
clc_o_rio$Ch2 = Pcov2(dbrio$pseudo2_ocupada, lag = lags + 1)
clc_o_rio$Ch3 = Pcov2(dbrio$pseudo3_ocupada, lag = lags + 1)
clc_o_rio$Ch4 = Pcov2(dbrio$pseudo4_ocupada, lag = lags + 1)
clc_o_rio$Ch5 = Pcov2(dbrio$pseudo5_ocupada, lag = lags + 1)

View(clc_o_rio)

# Soma das autocovs dos pseudo-erros:

clc_o_rio$SomaChk = clc_o_rio$Ch1 + clc_o_rio$Ch2 +clc_o_rio$Ch3 +clc_o_rio$Ch4 +clc_o_rio$Ch5
clc_o_rio$autocov = clc_o_rio$SomaChk/(K^2-K)

# FAC
clc_o_rio$fac = clc_o_rio$SomaChk/clc_o_rio$SomaChk[1]

# FACP
clc_o_rio$facp = 0 
clc_o_rio$facp[2:25] = facp_acf(clc_o_rio$fac,lags)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)

clc_o_rio$esttest = nrow(dbrio)*clc_o_rio$facp ^ 2 
clc_o_rio$pvalor = teste(clc_o_rio$facp,nrow(dbrio))
View(clc_o_rio)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_rio$lag, clc_o_rio$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_rio$lag[2:25], clc_o_rio$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


#### Desocupada: autocov, fac e facp

lag = c(0:24)
clc_d_rio=as.data.frame(lag)
head(clc_d_rio)

## Calculo autocov dos pseudoerros (Ch)

clc_d_rio$Ch1 = Pcov2(dbrio$pseudo1_desocupada, lag = lags + 1)
clc_d_rio$Ch2 = Pcov2(dbrio$pseudo2_desocupada, lag = lags + 1)
clc_d_rio$Ch3 = Pcov2(dbrio$pseudo3_desocupada, lag = lags + 1)
clc_d_rio$Ch4 = Pcov2(dbrio$pseudo4_desocupada, lag = lags + 1)
clc_d_rio$Ch5 = Pcov2(dbrio$pseudo5_desocupada, lag = lags + 1)

View(clc_d_rio)

# Soma das autocovs dos pseudo-erros:

clc_d_rio$SomaChk = clc_d_rio$Ch1 + clc_d_rio$Ch2 +clc_d_rio$Ch3 +clc_d_rio$Ch4 +clc_d_rio$Ch5
clc_d_rio$autocov = clc_d_rio$SomaChk/(K^2-K)

# FAC
clc_d_rio$fac = clc_d_rio$SomaChk/clc_d_rio$SomaChk[1]
View(clc_d_rio)

# FACP
clc_d_rio$facp = 0 
clc_d_rio$facp[2:25] = facp_acf(clc_d_rio$fac,lags)
View(clc_d_rio)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_d_rio$esttest = nrow(dbrio)*clc_d_rio$facp ^ 2 
clc_d_rio$pvalor = teste(clc_d_rio$facp,nrow(dbrio))
View(clc_d_rio)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_rio$lag, clc_d_rio$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_rio$lag[2:25], clc_d_rio$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}

### Parâmetros para os modelos

rho1_d_rio <- clc_d_rio$fac[2]
rho2_d_rio <- clc_d_rio$fac[3]

rho1_o_rio <- clc_o_rio$fac[2]
rho2_o_rio <- clc_o_rio$fac[3]

#AR(1)
phi1_ar1_drio = clc_d_rio$fac[lag==1]
phi1_ar1_orio = clc_o_rio$fac[lag==1]

par_ar1_rio<- data.frame(phi1_ar1_drio, phi1_ar1_orio)

#MA(1):
theta1_ma1_drio <- (1 - sqrt(1 - 4 * rho1_d_rio^2)) / (2 * rho1_d_rio)

theta1_ma1_orio <- (1 - sqrt(1 - 4 * rho1_o_rio^2)) / (2 * rho1_o_rio)

par_ma1_rio<-data.frame(theta1_ma1_drio,theta1_ma1_orio)

#AR(2)

phi1_ar2_drio <- (rho1_d_rio - rho1_d_rio * rho2_d_rio) / (1 - rho1_d_rio^2)
phi2_ar2_drio <- (rho2_d_rio - rho1_d_rio^2) / (1 - rho1_d_rio^2)

phi1_ar2_orio <- (rho1_o_rio - rho1_o_rio * rho2_o_rio) / (1 - rho1_o_rio^2)
phi2_ar2_orio <- (rho2_o_rio - rho1_o_rio^2) / (1 - rho1_o_rio^2)

par_ar2_rio<-data.frame(phi1_ar2_drio,phi2_ar2_drio,phi1_ar2_orio,phi2_ar2_orio)

#MA(2) - apenas desocupados

rho1 <- rho1_d_rio
rho2 <- rho2_d_rio
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

par_ma2_rio<-data.frame("theta1_ma2_drio"=solucao$x[1],"theta2_ma2_drio"=solucao$x[2])

#ARMA (1,1) - apenas desocupados

phi1_arma11_drio <- rho2_d_rio/rho1_d_rio

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  resultado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- resultado_theta1$root
  return(theta1)
}

rho1 <- rho1_d_rio
phi1 <- phi1_arma11_drio

theta1_arma11_drio <- arma11_theta1(rho1, phi1)

par_arma11_rio <-data.frame(phi1_arma11_drio,theta1_arma11_drio)

params_rio <- list("dbrio"=dbrio,"calculos_desocupada_rio"=clc_d_rio,
                    "calculos_ocupada_rio"=  clc_o_rio, 
                   "mod_ar1" = par_ar1_rio, "mod_ar2"=par_ar2_rio,
                   "mod_ma1"=par_ma1_rio,"mod_ma2"=par_ma2_rio, "mod_arma11"=par_arma11_rio)

saveRDS(params_rio,file = "D:/FJP2425/Programacao/data/pseudoerros/09_params_rio.rds")

### 10-CENTRAL #################################################################

dbcen<-base$`10-Central`

## Definindo variáveis adicionais

colnames(dbcen)
t = c(1:nrow(dbcen))
lags = 24
T = nrow(dbcen)
K = (ncol(dbcen)-11)/2 

# Valor médio dos painéis

dbcen$media_ocupada = dbcen %>%  select(starts_with("ocupada")) %>%  
  replace(.==0, NA) %>% rowMeans(.,na.rm = TRUE)
dbcen$media_desocupada = dbcen %>%  select(starts_with("desocupada")) %>%  
  replace(.==0, NA) %>%  rowMeans(.,na.rm = TRUE)

View(dbcen) #Verificação

# Matriz de pseudo erros

dbcen$pseudo1_ocupada = dbcen$ocupada_1 - dbcen$media_ocupada
dbcen$pseudo2_ocupada = dbcen$ocupada_2 - dbcen$media_ocupada
dbcen$pseudo3_ocupada = dbcen$ocupada_3 - dbcen$media_ocupada
dbcen$pseudo4_ocupada = dbcen$ocupada_4 - dbcen$media_ocupada
dbcen$pseudo5_ocupada = dbcen$ocupada_5 - dbcen$media_ocupada

dbcen$pseudo1_desocupada = dbcen$desocupada_1 - dbcen$media_desocupada
dbcen$pseudo2_desocupada = dbcen$desocupada_2 - dbcen$media_desocupada
dbcen$pseudo3_desocupada = dbcen$desocupada_3 - dbcen$media_desocupada
dbcen$pseudo4_desocupada = dbcen$desocupada_4 - dbcen$media_desocupada
dbcen$pseudo5_desocupada = dbcen$desocupada_5 - dbcen$media_desocupada

View(dbcen)

#### Ocupada: Autocov; FAC e FACP

lag = c(0:24)
clc_o_cen=as.data.frame(lag)
head(clc_o_cen)

## Calculo autocov dos pseudoerros (Ch)

clc_o_cen$Ch1 = Pcov2(dbcen$pseudo1_ocupada, lag = lags + 1)
clc_o_cen$Ch2 = Pcov2(dbcen$pseudo2_ocupada, lag = lags + 1)
clc_o_cen$Ch3 = Pcov2(dbcen$pseudo3_ocupada, lag = lags + 1)
clc_o_cen$Ch4 = Pcov2(dbcen$pseudo4_ocupada, lag = lags + 1)
clc_o_cen$Ch5 = Pcov2(dbcen$pseudo5_ocupada, lag = lags + 1)

View(clc_o_cen)

# Soma das autocovs dos pseudo-erros:

clc_o_cen$SomaChk = clc_o_cen$Ch1 + clc_o_cen$Ch2 +clc_o_cen$Ch3 +clc_o_cen$Ch4 +clc_o_cen$Ch5
clc_o_cen$autocov = clc_o_cen$SomaChk/(K^2-K)

# FAC
clc_o_cen$fac = clc_o_cen$SomaChk/clc_o_cen$SomaChk[1]

# FACP
clc_o_cen$facp = 0 
clc_o_cen$facp[2:25] = facp_acf(clc_o_cen$fac,lags)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)

clc_o_cen$esttest = nrow(dbcen)*clc_o_cen$facp ^ 2 
clc_o_cen$pvalor = teste(clc_o_cen$facp,nrow(dbcen))
View(clc_o_cen)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_cen$lag, clc_o_cen$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_o_cen$lag[2:25], clc_o_cen$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


#### Desocupada: autocov, fac e facp

lag = c(0:24)
clc_d_cen=as.data.frame(lag)
head(clc_d_cen)

## Calculo autocov dos pseudoerros (Ch)

clc_d_cen$Ch1 = Pcov2(dbcen$pseudo1_desocupada, lag = lags + 1)
clc_d_cen$Ch2 = Pcov2(dbcen$pseudo2_desocupada, lag = lags + 1)
clc_d_cen$Ch3 = Pcov2(dbcen$pseudo3_desocupada, lag = lags + 1)
clc_d_cen$Ch4 = Pcov2(dbcen$pseudo4_desocupada, lag = lags + 1)
clc_d_cen$Ch5 = Pcov2(dbcen$pseudo5_desocupada, lag = lags + 1)

View(clc_d_cen)

# Soma das autocovs dos pseudo-erros:

clc_d_cen$SomaChk = clc_d_cen$Ch1 + clc_d_cen$Ch2 +clc_d_cen$Ch3 +clc_d_cen$Ch4 +clc_d_cen$Ch5
clc_d_cen$autocov = clc_d_cen$SomaChk/(K^2-K)

# FAC
clc_d_cen$fac = clc_d_cen$SomaChk/clc_d_cen$SomaChk[1]
View(clc_d_cen)

# FACP
clc_d_cen$facp = 0 
clc_d_cen$facp[2:25] = facp_acf(clc_d_cen$fac,lags)
View(clc_d_cen)

# Estatísticas de teste:
## Referência: pg.86 de Silva e Cruz(2002) (anotação retirada do arqv original do Caio)
clc_d_cen$esttest = nrow(dbcen)*clc_d_cen$facp ^ 2 
clc_d_cen$pvalor = teste(clc_d_cen$facp,nrow(dbcen))
View(clc_d_cen)

# Plots

{par(mfrow=c(1,2),mar=c(5,5,2,5),cex=0.8)
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_cen$lag, clc_d_cen$fac, xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FAC")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T-2/sqrt(T), -1/T-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(-1/T+2/sqrt(T), -1/T+2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)
  
  
  par(bty = "l",cex.lab = 2)
  plot(x = clc_d_cen$lag[2:25], clc_d_cen$facp[2:25], xlab = "", ylab = "",
       type = "h", axes = FALSE, ylim = c(-2/sqrt(T) - 0.1, 1), lwd = 2, main = "FACP")
  lines(x = c(-3,lags), y = c(0,0),lwd = 2)
  lines(x = c(-3,lags), y = c(-2/sqrt(T),-2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  lines(x = c(-3,lags), y = c(2/sqrt(T), 2/sqrt(T)), col = "red", lty = 2, lwd = 2)
  box()
  axis(1, cex.axis = 1)
  axis(2, cex.axis = 1)
  mtext("lag", side = 1, line = 3)}


### Parâmetros para os modelos

rho1_d_cen <- clc_d_cen$fac[2]
rho2_d_cen <- clc_d_cen$fac[3]

rho1_o_cen <- clc_o_cen$fac[2]
rho2_o_cen <- clc_o_cen$fac[3]

#AR(1)
phi1_ar1_dcen = clc_d_cen$fac[lag==1]
phi1_ar1_ocen = clc_o_cen$fac[lag==1]

par_ar1_cen <- data.frame(phi1_ar1_dcen, phi1_ar1_ocen)

#MA(1):
theta1_ma1_dcen <- (1 - sqrt(1 - 4 * rho1_d_cen^2)) / (2 * rho1_d_cen)

theta1_ma1_ocen <- (1 - sqrt(1 - 4 * rho1_o_cen^2)) / (2 * rho1_o_cen)

par_ma1_cen <-data.frame(theta1_ma1_dcen,theta1_ma1_ocen)

#AR(2)

phi1_ar2_dcen <- (rho1_d_cen - rho1_d_cen * rho2_d_cen) / (1 - rho1_d_cen^2)
phi2_ar2_dcen <- (rho2_d_cen - rho1_d_cen^2) / (1 - rho1_d_cen^2)

phi1_ar2_ocen <- (rho1_o_cen - rho1_o_cen * rho2_o_cen) / (1 - rho1_o_cen^2)
phi2_ar2_ocen <- (rho2_o_cen - rho1_o_cen^2) / (1 - rho1_o_cen^2)

par_ar2_cen <-data.frame(phi1_ar2_dcen,phi2_ar2_dcen,phi1_ar2_ocen,phi2_ar2_ocen)

#MA(2) - apenas desocupados

rho1 <- rho1_d_cen
rho2 <- rho2_d_cen
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

par_ma2_cen<-data.frame("theta1_ma2_dcen"=solucao$x[1],"theta2_ma2_dcen"=solucao$x[2])

#ARMA (1,1) - apenas desocupados

phi1_arma11_dcen <- rho2_d_cen/rho1_d_cen

arma11_theta1 <- function(rho1, phi1) {
  eq_theta1 <- function(theta1, rho1, phi1) {
    (1 - phi1 * theta1) * (phi1 - theta1) / (1 + theta1^2 - 2 * phi1 * theta1) - rho1
  }
  resultado_theta1 <- uniroot(eq_theta1, interval = c(-1, 1), rho1 = rho1, phi1 = phi1)
  theta1 <- resultado_theta1$root
  return(theta1)
}

rho1 <- rho1_d_cen
phi1 <- phi1_arma11_dcen

theta1_arma11_dcen <- arma11_theta1(rho1, phi1)

par_arma11_cen <-data.frame(phi1_arma11_dcen,theta1_arma11_dcen)

params_cen <- list("dbcen"=dbcen,"calculos_desocupada_cen"=clc_d_cen,
                    "calculos_ocupada_cen"=  clc_o_cen, 
                   "mod_ar1" = par_ar1_cen, "mod_ar2"=par_ar2_cen,
                   "mod_ma1"=par_ma1_cen,"mod_ma2"=par_ma2_cen, "mod_arma11"=par_arma11_cen)

saveRDS(params_cen,file = "D:/FJP2425/Programacao/data/pseudoerros/10_params_cen.rds")

## Salvando as correlações em arquivo excel:

#FAC_ocupados <- data.frame(clc_o_bh$lag,clc_o_bh$fac,clc_o_ent$fac,clc_o_col$fac,clc_o_rid$fac,clc_o_sul$fac,
  #                         clc_o_trg$fac,clc_o_mat$fac,clc_o_nrt$fac,clc_o_rio$fac,clc_o_cen$fac)
#colnames(FAC_ocupados) <- c("LAG","Belo Horizonte", "Entorno BH", "Colar BH", "RIDE", "Sul",
  #                          "Triangulo", "Zona da Mata", "Norte", "Vale do Rio Doce", "Centro")

#FAC_desocupados <- data.frame(clc_d_bh$lag,clc_d_bh$fac,clc_d_ent$fac,clc_d_col$fac,clc_d_rid$fac,clc_d_sul$fac,
 #                             clc_d_trg$fac,clc_d_mat$fac,clc_d_nrt$fac,clc_d_rio$fac,clc_d_cen$fac)
#colnames(FAC_desocupados) <- c("LAG","Belo Horizonte", "Entorno BH", "Colar BH", "RIDE", "Sul",
 #                           "Triangulo", "Zona da Mata", "Norte", "Vale do Rio Doce", "Centro")

#FACP_ocupados <- data.frame(clc_o_bh$lag,clc_o_bh$facp,clc_o_ent$facp,clc_o_col$facp,clc_o_rid$facp,clc_o_sul$facp,
#                           clc_o_trg$facp,clc_o_mat$facp,clc_o_nrt$facp,clc_o_rio$facp,clc_o_cen$facp)
#colnames(FACP_ocupados) <- c("LAG","Belo Horizonte", "Entorno BH", "Colar BH", "RIDE", "Sul",
#                            "Triangulo", "Zona da Mata", "Norte", "Vale do Rio Doce", "Centro")

#FACP_desocupados <- data.frame(clc_d_bh$lag,clc_d_bh$facp,clc_d_ent$facp,clc_d_col$facp,clc_d_rid$facp,clc_d_sul$facp,
#                             clc_d_trg$facp,clc_d_mat$facp,clc_d_nrt$facp,clc_d_rio$facp,clc_d_cen$facp)
#colnames(FACP_desocupados) <- c("LAG","Belo Horizonte", "Entorno BH", "Colar BH", "RIDE", "Sul",
#                               "Triangulo", "Zona da Mata", "Norte", "Vale do Rio Doce", "Centro")

#listacorrs<-list("FAC_ocupados"=FAC_ocupados,"FAC_desocupados"=FAC_desocupados,
#                 "FACP_ocupados"=FACP_ocupados,"FACP_desocupados"=FACP_desocupados)

#write_xlsx(listacorrs,"D:/FJP2425/Reuniões/Tabelas/Reunião 1902/correlacoes1902.xlsx")
