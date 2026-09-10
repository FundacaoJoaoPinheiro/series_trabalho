################################################################################
##      MODELO MULTIVARIADO COM CORRELAÇÃO DA TAXA DE DESOCUPAÇÃO             ##
################################################################################

library(dlm)
library(tidyverse)
library(beepr)
library(gdata)

rm(list=ls())
gc()
options(scipen=999)

#### BASE DE DADOS #############################################################

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.rds")
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/dadosalin_txdesoc_8reg.rds")

# Environment para cada região

env1<-new.env()
env2<-new.env()
env3<-new.env()
env4<-new.env()
env5<-new.env()
env6<-new.env()
env7<-new.env()
env8<-new.env()
#env9<-new.env()

# 01 - bh

bh<-baseestr8reg$`01-Belo Horizonte`
dbbh<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/01_params_taxa_bh.RDS")
txdesoc_bh <- bh$Taxa.de.desocupação*100
txse_bh<- bh$sd_txd*100
cvtx_bh <- txse_bh/txdesoc_bh

theta1_ma1_bh <- dbbh[["taxamod_ma1"]][["theta1_ma1_txbh"]]
load("C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/01_mod_txbh.Rdata", envir = env1)
initial_bh <- env1$ma1_bh[["initial"]]
estimated_bh <- env1$ma1_bh[["fit"]][["par"]]

# 02 - ent

ent<-baseestr8reg$`02-Colar e Entorno metropolitano de BH`
dbent<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/02_params_taxa_ent.RDS")
txdesoc_ent<-(ent$Taxa.de.desocupação)*100
txse_ent <- (ent$sd_txd)*100
cvtx_ent <- txse_ent/txdesoc_ent

theta1_ma1_ent <- dbent[["taxamod_ma1"]][["theta1_ma1_txent"]]
load("C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/02_mod_txent.Rdata", envir = env2)
initial_ent <- env2$ma1_ent[["initial"]]
estimated_ent <- env2$ma1_ent[["fit"]][["par"]]

# 03 - sul

sul<-baseestr8reg$`03-Sul de Minas`
dbsul<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/03_params_taxa_sul.RDS")
txdesoc_sul <- (sul$Taxa.de.desocupação)*100
txse_sul <- (sul$sd_txd)*100
cvtx_sul <- txse_sul/txdesoc_sul

phi1_arma11_sul <- dbsul[["taxamod_arma11"]][["phi1_arma11_txsul"]]
theta1_arma11_sul <- dbsul[["taxamod_arma11"]][["theta1_arma11_txsul"]]
load("C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/03_mod_txsul.Rdata", envir = env3)
initial_sul <- env3$arma11_sul[["initial"]]
estimated_sul <- env3$arma11_sul[["fit"]][["par"]]

# 04 - trg

trg <- baseestr8reg$`04-Triângulo Mineiro`
dbtrg<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/04_params_taxa_trg.RDS")
txdesoc_trg<-trg$Taxa.de.desocupação*100
txse_trg <- trg$sd_txd*100
cvtx_trg <- txse_trg/txdesoc_trg

theta1_ma1_trg <- dbtrg[["taxamod_ma1"]][["theta1_ma1_txtrg"]]
load("C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/04_mod_txtrg.Rdata", envir = env4)
initial_trg <- env4$ma1_trg[["initial"]]
estimated_trg <- env4$ma1_trg[["fit"]][["par"]]

# 05 - mat

mat<-baseestr8reg$`05-Mata de Minas Gerais`
dbmat<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/05_params_taxa_mat.RDS")
txdesoc_mat<- mat$Taxa.de.desocupação*100
txse_mat<-mat$sd_txd*100
cvtx_mat<-txse_mat/txdesoc_mat

theta1_ma1_mat <- dbmat[["taxamod_ma1"]][["theta1_ma1_txmat"]]
load("C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/05_mod_txmat.Rdata", envir = env5)
initial_mat <- env5$ma1_mat[["initial"]]
estimated_mat <- env5$ma1_mat[["fit"]][["par"]]

# 06 - nrt

nrt<-baseestr8reg$`06-Norte de Minas`
dbnrt<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/06_params_taxa_nrt.RDS")
txdesoc_nrt<-nrt$Taxa.de.desocupação*100
txse_nrt <- nrt$sd_txd*100
cvtx_nrt <- txse_nrt/txdesoc_nrt

theta1_ma1_nrt <- dbnrt[["taxamod_ma1"]][["theta1_ma1_txnrt"]]
load("C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/06_mod_txnrt.Rdata", envir = env6)
initial_nrt <- env6$ma1_nrt[["initial"]]
estimated_nrt <- env6$ma1_nrt[["fit"]][["par"]]

# 07 - val

val<-baseestr8reg$`07-Vale do Rio Doce`
dbval<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/07_params_taxa_val.RDS")
txdesoc_val<-val$Taxa.de.desocupação*100
txse_val<- val$sd_txd*100
cvtx_val<- txse_val/txdesoc_val

phi1_arma11_val <- dbval[["taxamod_arma11"]][["phi1_arma11_txval"]]
theta1_arma11_val <- dbval[["taxamod_arma11"]][["theta1_arma11_txval"]]
load("C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/07_mod_txval.Rdata", envir = env7)
initial_val <- env7$ar1_val[["initial"]]
estimated_val <- env7$ar1_val[["fit"]][["par"]]

# 08 - cen

cen<-baseestr8reg$`08-Central`
dbcen<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_taxa_8reg/08_params_taxa_cen.RDS")
txdesoc_cen<-cen$Taxa.de.desocupação*100
txse_cen<-cen$sd_txd*100
cvtx_cen<-txse_cen/txdesoc_cen

theta1_ma1_cen <- dbcen[["taxamod_ma1"]][["theta1_ma1_txcen"]]
load("C:/FJP2425/Programacao/data/Rdatas/15_estruturaltaxadesocup_8reg/08_mod_txcen.Rdata", envir = env8)
initial_cen <- env8$ma1_cen[["initial"]]
estimated_cen <- env8$ma1_cen[["fit"]][["par"]]

# 09 - mg

#mg<-baseestr8reg$`09 - Minas Gerais`
#dbmg<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/09_params_mg.RDS")
#desoc_mg<-mg$Total.de.desocupados/1000
#se_mg<-mg$sd_d/1000
#cv_mg<-se_mg/desoc_mg

#theta1_ma1_mg <- dbmg[["mod_ma1"]][["theta1_ma1_dmg"]]
#load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/09_mod_mg.Rdata", envir = env9)
#initial_mg <- env9$ma1_mg[["initial"]]
#estimated_mg <- env9$ma1_mg[["fit"]][["par"]]

keep(txdesoc_bh,txse_bh,cvtx_bh,theta1_ma1_bh,initial_bh,estimated_bh,
     txdesoc_ent,txse_ent,cvtx_ent,theta1_ma1_ent,initial_ent,estimated_ent,
     txdesoc_sul,txse_sul,cvtx_sul,phi1_arma11_sul,theta1_arma11_sul,initial_sul,estimated_sul,
     txdesoc_trg,txse_trg,cvtx_trg,theta1_ma1_trg,initial_trg,estimated_trg,
     txdesoc_mat,txse_mat,cvtx_mat,theta1_ma1_mat,initial_mat,estimated_mat,
     txdesoc_nrt,txse_nrt,cvtx_nrt,theta1_ma1_nrt,initial_nrt,estimated_nrt,
     txdesoc_val,txse_val,cvtx_val,phi1_arma11_val,theta1_arma11_val,initial_val,estimated_val,
     txdesoc_cen,txse_cen,cvtx_cen,theta1_ma1_cen,initial_cen,estimated_cen,
     sure=TRUE)
gc()

## TESTANDO MODELO COM CORRELAÇÃO E SEM BENCHMARKING

modelo_mult<- list("fn"=function(params){
  m = dlmModPoly(2) + dlmModTrig(4) + dlmModReg(txse_bh,addInt = FALSE) # Erro de bh como ref do modelo
  m$FF <- cbind(m$FF, rep(0,1))
  m$GG <- rbind(m$GG, rep(0,6))
  m$GG <- cbind(m$GG, rep(0,7))
  m$GG[6,6] <- 0
  m$GG[6,7] <- 5       
  m$GG[7,6] <- 0
  m$GG[7,7] <- 0 
  
  FF <- m$FF %x% diag(8)
  m$FF <- FF
  
  JFF <- matrix(0,8,56)
  JFF[1,41] <- 1
  JFF[2,42] <- 2
  JFF[3,43] <- 3
  JFF[4,44] <- 4
  JFF[5,45] <- 5
  JFF[6,46] <- 6
  JFF[7,47] <- 7
  JFF[8,48] <- 8
  m$JFF <- JFF
  
  X <- cbind(txse_bh,txse_ent,txse_sul,txse_trg,txse_mat,txse_nrt,txse_val,txse_cen)
  m$X <- X
  
  V <- m$V %x% diag(8)
  V[1,1] <- exp(params[25])
  V[2,2] <- exp(params[26])
  V[3,3] <- exp(params[27])
  V[4,4] <- exp(params[28])
  V[5,5] <- exp(params[29])
  V[6,6] <- exp(params[30])
  V[7,7] <- exp(params[31])
  V[8,8] <- exp(params[32])
  m$V <- V
  
  GG <- m$GG %x% diag(8) # Resolver matriz GG
  #dimnames(GG) <- list(1:nrow(GG), 1:ncol(GG)) # Para se orientar ao dar "view" na matriz
  GG[41,41] <- 0
  GG[42,42] <- 0
  GG[43,43] <- phi1_arma11_sul
  GG[44,44] <- 0
  GG[45,45] <- 0
  GG[46,46] <- 0
  GG[47,47] <- phi1_arma11_val
  GG[48,48] <- 0
  
  GG[41,49] <- theta1_ma1_bh
  GG[42,50] <- theta1_ma1_ent
  GG[43,51] <- theta1_arma11_sul
  GG[44,52] <- theta1_ma1_trg
  GG[45,53] <- theta1_ma1_mat
  GG[46,54] <- theta1_ma1_nrt
  GG[47,55] <- theta1_arma11_val
  GG[48,56] <- theta1_ma1_cen
  m$GG <- GG
  
  W <- matrix(0, 7, 7)
  m$W <- W
  W <- m$W %x% diag(8)
  W[1,1] <- exp(params[1])
  W[2,2] <- exp(params[2])
  W[3,3] <- exp(params[3])
  W[4,4] <- exp(params[4])
  W[5,5] <- exp(params[5])
  W[6,6] <- exp(params[6])
  W[7,7] <- exp(params[7])
  W[8,8] <- exp(params[8])
  
  W[9,9] <- exp(params[9])
  W[10,10] <- exp(params[10])
  W[11,11] <- exp(params[11])
  W[12,12] <- exp(params[12])
  W[13,13] <- exp(params[13])
  W[14,14] <- exp(params[14])
  W[15,15] <- exp(params[15])
  W[16,16] <- exp(params[16])
  
  W[17,17] <- exp(params[17])
  W[18,18] <- exp(params[18])
  W[19,19] <- exp(params[19])
  W[20,20] <- exp(params[20])
  W[21,21] <- exp(params[21])
  W[22,22] <- exp(params[22])
  W[23,23] <- exp(params[23])
  W[24,24] <- exp(params[24])
  
  ## --- Erro amostral: forma de Harvey (issue #20) --------------------------
  ## O bloco (e_t, aux_t) precisa da MESMA inovacao nos dois estados:
  ##   e_t = xi_t + theta*xi_{t-1}   =>   W[e,e] = W[aux,aux] = W[e,aux] = s2
  ## ANTES: so W[41..48] eram preenchidos; as posicoes 49-56 (aux) ficavam com
  ## variancia zero e a linha 49-56 de GG e nula, entao o estado auxiliar era
  ## identicamente zero e o termo theta NUNCA entrava: o erro amostral colapsava
  ## para ruido branco (acf(1) = 0 em vez de theta/(1+theta^2)).
  ## ATENCAO: preencher so a diagonal W[49..56] NAO resolve — com choques
  ## independentes a autocovariancia de defasagem 1 volta a ser zero. A
  ## off-diagonal e a peca essencial. O bloco 2x2 resultante e singular
  ## (posto 1), e isso esta correto: ha um unico choque.
  ## Na taxa, os 8 estratos tem termo MA (o Vale e ARMA(1,1)).
  theta_ea <- c(theta1_ma1_bh, theta1_ma1_ent, theta1_arma11_sul, theta1_ma1_trg,
                theta1_ma1_mat, theta1_ma1_nrt, theta1_arma11_val, theta1_ma1_cen)
  for (i in 1:8) {
    s2 <- exp(params[32 + i])
    W[40 + i, 40 + i] <- s2
    if (theta_ea[i] != 0) {
      W[48 + i, 48 + i] <- s2
      W[40 + i, 48 + i] <- s2
      W[48 + i, 40 + i] <- s2
    }
  }

  ## Adicionando a correlacao entre as séries
  
  # tanh -> garante que a correlação fique entre -1 e 1. Tangente hiperbólica
  # Por que eu começo o cálculo dessa covariância apenas a partir do [12,11]? 
  # Qual o motivo dessa posição da matriz?
  
  # Fiz o ajuste das correlações conforme as posições do slope de cada região
  #  posições de 9 a 16
  
  W[10,9] <- W[9,10] <- tanh(params[41]) * prod(exp(0.5 * params[c(9,10)])) # tanh(params[41]) * sqrt(exp(params[9])) * sqrt(exp(params[10])) # BH COM ENT
  W[11,9] <- W[9,11] <- tanh(params[42]) * prod(exp(0.5 * params[c(9,11)])) # BH COM SUL
  W[12,9] <- W[9,12] <- tanh(params[43]) * prod(exp(0.5 * params[c(9,12)])) # BH COM TRG
  W[13,9] <- W[9,13] <- tanh(params[44]) * prod(exp(0.5 * params[c(9,13)])) # BH COM MAT
  W[14,9] <- W[9,14] <- tanh(params[45]) * prod(exp(0.5 * params[c(9,14)])) # BH COM NRT
  W[15,9] <- W[9,15] <- tanh(params[46]) * prod(exp(0.5 * params[c(9,15)])) # BH COM VAL
  W[16,9] <- W[9,16] <- tanh(params[47]) * prod(exp(0.5 * params[c(9,16)])) # BH COM CEN
  
  W[11,10] <- W[10,11] <- tanh(params[48]) * prod(exp(0.5 * params[c(10,11)])) # ENT COM SUL
  W[12,10] <- W[10,12] <- tanh(params[49]) * prod(exp(0.5 * params[c(10,12)])) # ENT COM TRG
  W[13,10] <- W[10,13] <- tanh(params[50]) * prod(exp(0.5 * params[c(10,13)])) # ENT COM MAT
  W[14,10] <- W[10,14] <- tanh(params[51]) * prod(exp(0.5 * params[c(10,14)])) # ENT COM NRT
  W[15,10] <- W[10,15] <- tanh(params[52]) * prod(exp(0.5 * params[c(10,15)])) # ENT COM VAL
  W[16,10] <- W[10,16] <- tanh(params[53]) * prod(exp(0.5 * params[c(10,16)])) # ENT COM CEN
  
  W[12,11] <- W[11,12] <- tanh(params[54]) * prod(exp(0.5 * params[c(11,12)])) # SUL COM TRG
  W[13,11] <- W[11,13] <- tanh(params[55]) * prod(exp(0.5 * params[c(11,13)])) # SUL COM MAT
  W[14,11] <- W[11,14] <- tanh(params[56]) * prod(exp(0.5 * params[c(11,14)])) # SUL COM NRT
  W[15,11] <- W[11,15] <- tanh(params[57]) * prod(exp(0.5 * params[c(11,15)])) # SUL COM VAL
  W[16,11] <- W[11,16] <- tanh(params[58]) * prod(exp(0.5 * params[c(11,16)])) # SUL COM CEN
  
  W[13,12] <- W[12,13] <- tanh(params[59]) * prod(exp(0.5 * params[c(12,13)])) # TRG COM MAT
  W[14,12] <- W[12,14] <- tanh(params[60]) * prod(exp(0.5 * params[c(12,14)])) # TRG COM NRT
  W[15,12] <- W[12,15] <- tanh(params[61]) * prod(exp(0.5 * params[c(12,15)])) # TRG COM VAL
  W[16,12] <- W[12,16] <- tanh(params[62]) * prod(exp(0.5 * params[c(12,16)])) # TRG COM CEN
  
  W[14,13] <- W[13,14] <- tanh(params[63]) * prod(exp(0.5 * params[c(13,14)])) # MAT COM NRT
  W[15,13] <- W[13,15] <- tanh(params[64]) * prod(exp(0.5 * params[c(13,15)])) # MAT COM VAL
  W[16,13] <- W[13,16] <- tanh(params[65]) * prod(exp(0.5 * params[c(13,16)])) # MAT COM CEN
  
  W[15,14] <- W[14,15] <- tanh(params[66]) * prod(exp(0.5 * params[c(14,15)])) # NRT COM VAL
  W[16,14] <- W[14,16] <- tanh(params[67]) * prod(exp(0.5 * params[c(14,16)])) # NRT COM CEN
  
  W[16,15] <- W[15,16] <- tanh(params[68]) * prod(exp(0.5 * params[c(15,16)])) # VAL COM CEN
  
  m$W <- W
  
  ## m0 tem de ser um VETOR de 56 (= nrow(GG)). `rep(0,7) %x% diag(8)` devolve
  ## uma matriz 56x8. Sem efeito numerico (e tudo zero), mas a dimensao estava
  ## errada. C0 abaixo esta certo: diag(7) %x% diag(8) da 56x56.
  m$m0 <- rep(0, 56)
  
  m$C0<- diag(x=10^7,7)
  C0 <- m$C0 %x% diag(8)
  m$C0 <- C0
  
  return(m)
})


iniciais <- c(initial_bh[1], initial_ent[1], initial_sul[1], initial_trg[1], initial_mat[1], initial_nrt[1], initial_val[1], initial_cen[1], 
              initial_bh[2], initial_ent[2], initial_sul[2], initial_trg[2], initial_mat[2], initial_nrt[2], initial_val[2], initial_cen[2],
              initial_bh[3], initial_ent[3], initial_sul[3], initial_trg[3], initial_mat[3], initial_nrt[3], initial_val[3], initial_cen[3],
              initial_bh[4], initial_ent[4], initial_sul[4], initial_trg[4], initial_mat[4], initial_nrt[4], initial_val[4], initial_cen[4],
              initial_bh[5], initial_ent[5], initial_sul[5], initial_trg[5], initial_mat[5], initial_nrt[5], initial_val[5], initial_cen[5])

estimados <- c(estimated_bh[1], estimated_ent[1], estimated_sul[1], estimated_trg[1], estimated_mat[1], estimated_nrt[1], estimated_val[1], estimated_cen[1], 
               estimated_bh[2], estimated_ent[2], estimated_sul[2], estimated_trg[2], estimated_mat[2], estimated_nrt[2], estimated_val[2], estimated_cen[2],
               estimated_bh[3], estimated_ent[3], estimated_sul[3], estimated_trg[3], estimated_mat[3], estimated_nrt[3], estimated_val[3], estimated_cen[3],
               estimated_bh[4], estimated_ent[4], estimated_sul[4], estimated_trg[4], estimated_mat[4], estimated_nrt[4], estimated_val[4], estimated_cen[4],
               estimated_bh[5], estimated_ent[5], estimated_sul[5], estimated_trg[5], estimated_mat[5], estimated_nrt[5], estimated_val[5], estimated_cen[5])

data <- cbind(txdesoc_bh,txdesoc_ent,txdesoc_sul,txdesoc_trg,txdesoc_mat,txdesoc_nrt,txdesoc_val,txdesoc_cen)

modelo_mult$initial<- c(estimados, rep(0,28)) # Iniciais para as correlações
# Essa linha não existia no modelo sem correlação

start_time <- Sys.time()
modelo_mult$fit <- dlmMLE(data, modelo_mult$initial,modelo_mult$fn, hessian=T,
                          control = list(maxit = 10^8))
end_time <- Sys.time()
end_time - start_time
beep(3)


## Separação dos resultados:

# Reproduz o modelo em matrizes:

modelo_mult$mod <- modelo_mult$fn(modelo_mult$fit$par)

# aplica o filtro de Kalman para obter séries filtradas e suavizadas
modelo_mult$filtered <- dlmFilter(data,modelo_mult$mod)
modelo_mult$smoothed <- dlmSmooth(modelo_mult$filtered)
modelo_mult$m <- dropFirst(modelo_mult$filtered$m)
modelo_mult$sm <- dropFirst(modelo_mult$smoothed$s)
modelo_mult$res <- residuals(modelo_mult$filtered,sd=FALSE)

# definição de variável
modelo_mult$d<-length(modelo_mult$mod$m0)/56
modelo_mult$T<-length(data[,1])

# Estatísticas de interesse:

modelo_mult$ts.original_1<- txdesoc_bh
modelo_mult$ts.trend_1 <- modelo_mult[["m"]][,1]
modelo_mult$ts.slope_1 <- modelo_mult[["m"]][,9]
modelo_mult$ts.seasonal_1 <- modelo_mult[["m"]][,17]+modelo_mult[["m"]][,33]
modelo_mult$ts.signal_1 <- modelo_mult$ts.trend_1 +modelo_mult$ts.seasonal_1
modelo_mult$ts.sampling_e_1 <- modelo_mult[["m"]][,41]*txse_bh
modelo_mult$ts.sampling_e_til_1 <- modelo_mult[["m"]][,41]
modelo_mult$ts.irregular_1 <- modelo_mult$ts.original_1-(modelo_mult$ts.signal_1+modelo_mult$ts.sampling_e_1)
modelo_mult$ts.seasonal_adj_1 <- modelo_mult$ts.trend_1+modelo_mult$ts.irregular_1

modelo_mult$ts.original_2<- txdesoc_ent
modelo_mult$ts.trend_2 <- modelo_mult[["m"]][,2]
modelo_mult$ts.slope_2 <- modelo_mult[["m"]][,10]
modelo_mult$ts.seasonal_2 <- modelo_mult[["m"]][,18]+modelo_mult[["m"]][,34]
modelo_mult$ts.signal_2 <- modelo_mult$ts.trend_2 +modelo_mult$ts.seasonal_2
modelo_mult$ts.sampling_e_2 <- modelo_mult[["m"]][,42]*txse_ent
modelo_mult$ts.sampling_e_til_2 <- modelo_mult[["m"]][,42]
modelo_mult$ts.irregular_2 <- modelo_mult$ts.original_2-(modelo_mult$ts.signal_2+modelo_mult$ts.sampling_e_2)
modelo_mult$ts.seasonal_adj_2 <- modelo_mult$ts.trend_2+modelo_mult$ts.irregular_2

modelo_mult$ts.original_3<- txdesoc_sul
modelo_mult$ts.trend_3 <- modelo_mult[["m"]][,3]
modelo_mult$ts.slope_3 <- modelo_mult[["m"]][,11]
modelo_mult$ts.seasonal_3 <- modelo_mult[["m"]][,19]+modelo_mult[["m"]][,35]
modelo_mult$ts.signal_3 <- modelo_mult$ts.trend_3 +modelo_mult$ts.seasonal_3
modelo_mult$ts.sampling_e_3 <- modelo_mult[["m"]][,43]*txse_sul
modelo_mult$ts.sampling_e_til_3 <- modelo_mult[["m"]][,43]
modelo_mult$ts.irregular_3 <- modelo_mult$ts.original_3-(modelo_mult$ts.signal_3+modelo_mult$ts.sampling_e_3)
modelo_mult$ts.seasonal_adj_3 <- modelo_mult$ts.trend_3+modelo_mult$ts.irregular_3

modelo_mult$ts.original_4<- txdesoc_trg
modelo_mult$ts.trend_4 <- modelo_mult[["m"]][,4]
modelo_mult$ts.slope_4 <- modelo_mult[["m"]][,12]
modelo_mult$ts.seasonal_4 <- modelo_mult[["m"]][,20]+modelo_mult[["m"]][,36]
modelo_mult$ts.signal_4 <- modelo_mult$ts.trend_4 +modelo_mult$ts.seasonal_4
modelo_mult$ts.sampling_e_4 <- modelo_mult[["m"]][,44]*txse_trg
modelo_mult$ts.sampling_e_til_4 <- modelo_mult[["m"]][,44]
modelo_mult$ts.irregular_4 <- modelo_mult$ts.original_4-(modelo_mult$ts.signal_4+modelo_mult$ts.sampling_e_4)
modelo_mult$ts.seasonal_adj_4 <- modelo_mult$ts.trend_4+modelo_mult$ts.irregular_4

modelo_mult$ts.original_5<- txdesoc_mat
modelo_mult$ts.trend_5 <- modelo_mult[["m"]][,5]
modelo_mult$ts.slope_5 <- modelo_mult[["m"]][,13]
modelo_mult$ts.seasonal_5 <- modelo_mult[["m"]][,21]+modelo_mult[["m"]][,37]
modelo_mult$ts.signal_5 <- modelo_mult$ts.trend_5 +modelo_mult$ts.seasonal_5
modelo_mult$ts.sampling_e_5 <- modelo_mult[["m"]][,45]*txse_mat
modelo_mult$ts.sampling_e_til_5 <- modelo_mult[["m"]][,45]
modelo_mult$ts.irregular_5 <- modelo_mult$ts.original_5-(modelo_mult$ts.signal_5+modelo_mult$ts.sampling_e_5)
modelo_mult$ts.seasonal_adj_5 <- modelo_mult$ts.trend_5+modelo_mult$ts.irregular_5

modelo_mult$ts.original_6<- txdesoc_nrt
modelo_mult$ts.trend_6 <- modelo_mult[["m"]][,6]
modelo_mult$ts.slope_6 <- modelo_mult[["m"]][,14]
modelo_mult$ts.seasonal_6 <- modelo_mult[["m"]][,22]+modelo_mult[["m"]][,38]
modelo_mult$ts.signal_6 <- modelo_mult$ts.trend_6 +modelo_mult$ts.seasonal_6
modelo_mult$ts.sampling_e_6 <- modelo_mult[["m"]][,46]*txse_nrt
modelo_mult$ts.sampling_e_til_6 <- modelo_mult[["m"]][,46]
modelo_mult$ts.irregular_6 <- modelo_mult$ts.original_6-(modelo_mult$ts.signal_6+modelo_mult$ts.sampling_e_6)
modelo_mult$ts.seasonal_adj_6 <- modelo_mult$ts.trend_6+modelo_mult$ts.irregular_6

modelo_mult$ts.original_7<- txdesoc_val
modelo_mult$ts.trend_7 <- modelo_mult[["m"]][,7]
modelo_mult$ts.slope_7 <- modelo_mult[["m"]][,15]
modelo_mult$ts.seasonal_7 <- modelo_mult[["m"]][,23]+modelo_mult[["m"]][,39]
modelo_mult$ts.signal_7 <- modelo_mult$ts.trend_7 +modelo_mult$ts.seasonal_7
modelo_mult$ts.sampling_e_7 <- modelo_mult[["m"]][,47]*txse_val
modelo_mult$ts.sampling_e_til_7 <- modelo_mult[["m"]][,47]
modelo_mult$ts.irregular_7 <- modelo_mult$ts.original_7-(modelo_mult$ts.signal_7+modelo_mult$ts.sampling_e_7)
modelo_mult$ts.seasonal_adj_7 <- modelo_mult$ts.trend_7+modelo_mult$ts.irregular_7

modelo_mult$ts.original_8<- txdesoc_cen
modelo_mult$ts.trend_8 <- modelo_mult[["m"]][,8]
modelo_mult$ts.slope_8 <- modelo_mult[["m"]][,16]
modelo_mult$ts.seasonal_8 <- modelo_mult[["m"]][,24]+modelo_mult[["m"]][,40]
modelo_mult$ts.signal_8 <- modelo_mult$ts.trend_8 +modelo_mult$ts.seasonal_8
modelo_mult$ts.sampling_e_8 <- modelo_mult[["m"]][,48]*txse_cen
modelo_mult$ts.sampling_e_til_8 <- modelo_mult[["m"]][,48]
modelo_mult$ts.irregular_8 <- modelo_mult$ts.original_8-(modelo_mult$ts.signal_8+modelo_mult$ts.sampling_e_8)
modelo_mult$ts.seasonal_adj_8 <- modelo_mult$ts.trend_8+modelo_mult$ts.irregular_8


# Painel de gráficos

par(mfrow = c(4, 2), mar = c(2, 2, 1, 1), oma = c(0, 0, 3, 0))
ts.plot(modelo_mult$ts.trend_1[8:52], main = "01 - BH")
ts.plot(modelo_mult$ts.trend_2[8:52], main = "02 - ENT")
ts.plot(modelo_mult$ts.trend_3[8:52], main = "03 - SUL")
ts.plot(modelo_mult$ts.trend_4[8:52], main = "04 - TRG")
ts.plot(modelo_mult$ts.trend_5[8:52], main = "05 - MAT")
ts.plot(modelo_mult$ts.trend_6[8:52], main = "06 - NRT")
ts.plot(modelo_mult$ts.trend_7[8:52], main = "07 - VAL")
ts.plot(modelo_mult$ts.trend_8[8:52], main = "08 - CEN")
mtext("Tendência extraída para cada região", outer = TRUE, cex = 1.2, line = -1)

par(mfrow = c(4, 2), mar = c(2, 2, 1, 1), oma = c(0, 0, 3, 0))
ts.plot(modelo_mult$ts.signal_1[8:52], main = "01 - BH")
ts.plot(modelo_mult$ts.signal_2[8:52], main = "02 - ENT")
ts.plot(modelo_mult$ts.signal_3[8:52], main = "03 - SUL")
ts.plot(modelo_mult$ts.signal_4[8:52], main = "04 - TRG")
ts.plot(modelo_mult$ts.signal_5[8:52], main = "05 - MAT")
ts.plot(modelo_mult$ts.signal_6[8:52], main = "06 - NRT")
ts.plot(modelo_mult$ts.signal_7[8:52], main = "07 - VAL")
ts.plot(modelo_mult$ts.signal_8[8:52], main = "08 - CEN")
mtext("Sinal extraído para cada região", outer = TRUE, cex = 1.2, line = -1)

par(mfrow = c(4, 2), mar = c(2, 2, 1, 1), oma = c(0, 0, 3, 0))
ts.plot(modelo_mult$ts.original_1[8:52], main = "01 - BH")
ts.plot(modelo_mult$ts.original_2[8:52], main = "02 - ENT")
ts.plot(modelo_mult$ts.original_3[8:52], main = "03 - SUL")
ts.plot(modelo_mult$ts.original_4[8:52], main = "04 - TRG")
ts.plot(modelo_mult$ts.original_5[8:52], main = "05 - MAT")
ts.plot(modelo_mult$ts.original_6[8:52], main = "06 - NRT")
ts.plot(modelo_mult$ts.original_7[8:52], main = "07 - VAL")
ts.plot(modelo_mult$ts.original_8[8:52], main = "08 - CEN")
mtext("Séries originais do total de desocupados", outer = TRUE, cex = 1.2, line = -1)

par(mfrow = c(4, 2), mar = c(2, 2, 1, 1), oma = c(0, 0, 3, 0))
ts.plot(modelo_mult$ts.sampling_e_1[8:52], main = "01 - BH")
ts.plot(modelo_mult$ts.sampling_e_2[8:52], main = "02 - ENT")
ts.plot(modelo_mult$ts.sampling_e_3[8:52], main = "03 - SUL")
ts.plot(modelo_mult$ts.sampling_e_4[8:52], main = "04 - TRG")
ts.plot(modelo_mult$ts.sampling_e_5[8:52], main = "05 - MAT")
ts.plot(modelo_mult$ts.sampling_e_6[8:52], main = "06 - NRT")
ts.plot(modelo_mult$ts.sampling_e_7[8:52], main = "07 - VAL")
ts.plot(modelo_mult$ts.sampling_e_8[8:52], main = "08 - CEN")
mtext("Erro amostral extraído para cada região", outer = TRUE, cex = 1.2, line = -1)

par(mfrow = c(4, 2), mar = c(2, 2, 1, 1), oma = c(0, 0, 3, 0))
ts.plot(modelo_mult$ts.irregular_1[8:52], main = "01 - BH")
ts.plot(modelo_mult$ts.irregular_2[8:52], main = "02 - ENT")
ts.plot(modelo_mult$ts.irregular_3[8:52], main = "03 - SUL")
ts.plot(modelo_mult$ts.irregular_4[8:52], main = "04 - TRG")
ts.plot(modelo_mult$ts.irregular_5[8:52], main = "05 - MAT")
ts.plot(modelo_mult$ts.irregular_6[8:52], main = "06 - NRT")
ts.plot(modelo_mult$ts.irregular_7[8:52], main = "07 - VAL")
ts.plot(modelo_mult$ts.irregular_8[8:52], main = "08 - CEN")
mtext("Termo irregular extraído para cada região", outer = TRUE, cex = 1.2, line = -1)

par(mfrow = c(4, 2), mar = c(2, 2, 1, 1), oma = c(0, 0, 3, 0))
ts.plot(modelo_mult$ts.seasonal_1[8:52], main = "01 - BH")
ts.plot(modelo_mult$ts.seasonal_2[8:52], main = "02 - ENT")
ts.plot(modelo_mult$ts.seasonal_3[8:52], main = "03 - SUL")
ts.plot(modelo_mult$ts.seasonal_4[8:52], main = "04 - TRG")
ts.plot(modelo_mult$ts.seasonal_5[8:52], main = "05 - MAT")
ts.plot(modelo_mult$ts.seasonal_6[8:52], main = "06 - NRT")
ts.plot(modelo_mult$ts.seasonal_7[8:52], main = "07 - VAL")
ts.plot(modelo_mult$ts.seasonal_8[8:52], main = "08 - CEN")
mtext("Sazonalidade extraída para cada região", outer = TRUE, cex = 1.2, line = -1)

## Separando os parâmetros obtidos para análise

# Iniciais

componentes_ini <- c("level_ini", "slope_ini", "seasonality_ini", "irregular_ini", "sample_error_ini")

# Extração dos valores iniciais para cada região
first_bh <- data.frame(
  componente = componentes_ini,
  valor = round(exp(modelo_mult$initial[c(1, 9, 17, 25, 33)]), 5)
)

first_ent <- data.frame(
  componente = componentes_ini,
  valor = round(exp(modelo_mult$initial[c(2, 10, 18, 26, 34)]), 5)
)

first_sul <- data.frame(
  componente = componentes_ini,
  valor = round(exp(modelo_mult$initial[c(3, 11, 19, 27, 35)]), 5)
)

first_trg <- data.frame(
  componente = componentes_ini,
  valor = round(exp(modelo_mult$initial[c(4, 12, 20, 28, 36)]), 5)
)

first_mat <- data.frame(
  componente = componentes_ini,
  valor = round(exp(modelo_mult$initial[c(5, 13, 21, 29, 37)]), 5)
)

first_nrt <- data.frame(
  componente = componentes_ini,
  valor = round(exp(modelo_mult$initial[c(6, 14, 22, 30, 38)]), 5)
)

first_val <- data.frame(
  componente = componentes_ini,
  valor = round(exp(modelo_mult$initial[c(7, 15, 23, 31, 39)]), 5)
)

first_cen <- data.frame(
  componente = componentes_ini,
  valor = round(exp(modelo_mult$initial[c(8, 16, 24, 32, 40)]), 5)
)

# Hiperparâmetros estimados

componentes <- c("level", "slope", "seasonality", "irregular", "sample_error")

pars_bh <- data.frame(
  componente = componentes,
  valor = round(exp(modelo_mult$fit$par[c(1, 9, 17, 25, 33)]), 5)
)

pars_ent <- data.frame(
  componente = componentes,
  valor = round(exp(modelo_mult$fit$par[c(2, 10, 18, 26, 34)]), 5)
)

pars_sul <- data.frame(
  componente = componentes,
  valor = round(exp(modelo_mult$fit$par[c(3, 11, 19, 27, 35)]), 5)
)

pars_trg <- data.frame(
  componente = componentes,
  valor = round(exp(modelo_mult$fit$par[c(4, 12, 20, 28, 36)]), 5)
)

pars_mat <- data.frame(
  componente = componentes,
  valor = round(exp(modelo_mult$fit$par[c(5, 13, 21, 29, 37)]), 5)
)

pars_nrt <- data.frame(
  componente = componentes,
  valor = round(exp(modelo_mult$fit$par[c(6, 14, 22, 30, 38)]), 5)
)

pars_val <- data.frame(
  componente = componentes,
  valor = round(exp(modelo_mult$fit$par[c(7, 15, 23, 31, 39)]), 5)
)

pars_cen <- data.frame(
  componente = componentes,
  valor = round(exp(modelo_mult$fit$par[c(8, 16, 24, 32, 40)]), 5)
)

# Calculando o erro padrão

mse.list = dlmSvd2var(modelo_mult[["filtered"]][["U.C"]], modelo_mult[["filtered"]][["D.C"]])
se.mat = dropFirst(t(sapply(mse.list, FUN=function(x) sqrt(diag(x)))))

# Cria vertores indicadores para soma de estados

c_sinal1 <- matrix(c(1,0,0,0,0,0,0,0, # Tend
                     0,0,0,0,0,0,0,0, # n somo o slope
                     1,0,0,0,0,0,0,0, # Somo saz 1 trig
                     0,0,0,0,0,0,0,0, # pulo saz 2 trig
                     1,0,0,0,0,0,0,0, # somo saz 3 trig
                     0,0,0,0,0,0,0,0, # EA não entra no sinal
                     0,0,0,0,0,0,0,0),1,56) # esse é por causa do eta

c_sinal2 <- matrix(c(0,1,0,0,0,0,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,1,0,0,0,0,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,1,0,0,0,0,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,0,0,0,0),1,56)

c_sinal3 <- matrix(c(0,0,1,0,0,0,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,1,0,0,0,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,1,0,0,0,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,0,0,0,0),1,56)

c_sinal4 <- matrix(c(0,0,0,1,0,0,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,1,0,0,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,1,0,0,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,0,0,0,0),1,56)

c_sinal5 <- matrix(c(0,0,0,0,1,0,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,1,0,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,1,0,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,0,0,0,0),1,56)

c_sinal6 <- matrix(c(0,0,0,0,0,1,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,0,1,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,0,1,0,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,0,0,0,0),1,56)

c_sinal7 <- matrix(c(0,0,0,0,0,0,1,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,0,0,1,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,0,0,1,0, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,0,0,0,0),1,56)

c_sinal8 <- matrix(c(0,0,0,0,0,0,0,1, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,0,0,0,1, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,0,0,0,1, 
                     0,0,0,0,0,0,0,0, 
                     0,0,0,0,0,0,0,0),1,56)

# Para a sazonalidade

c_seasonal1 <- matrix(c(0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        1,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        1,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0),1,56)

c_seasonal2 <- matrix(c(0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,1,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,1,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0),1,56)

c_seasonal3 <- matrix(c(0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,1,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,1,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0),1,56)

c_seasonal4 <- matrix(c(0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,1,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,1,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0),1,56)

c_seasonal5 <- matrix(c(0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,1,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,1,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0),1,56)

c_seasonal6 <- matrix(c(0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,1,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,1,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0),1,56)

c_seasonal7 <- matrix(c(0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,1,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,1,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0),1,56)

c_seasonal8 <- matrix(c(0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,1, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,1, 
                        0,0,0,0,0,0,0,0, 
                        0,0,0,0,0,0,0,0),1,56)

se.mat1_sinal = dropFirst((sapply(mse.list, function(i) sqrt(c_sinal1%*%i%*%t(c_sinal1)) )))
se.mat1_seasonal = dropFirst((sapply(mse.list, function(i) sqrt(c_seasonal1%*%i%*%t(c_seasonal1)) )))

se.mat2_sinal = dropFirst((sapply(mse.list, function(i) sqrt(c_sinal2%*%i%*%t(c_sinal2)) )))
se.mat2_seasonal = dropFirst((sapply(mse.list, function(i) sqrt(c_seasonal2%*%i%*%t(c_seasonal2)) )))

se.mat3_sinal = dropFirst((sapply(mse.list, function(i) sqrt(c_sinal3%*%i%*%t(c_sinal3)) )))
se.mat3_seasonal = dropFirst((sapply(mse.list, function(i) sqrt(c_seasonal3%*%i%*%t(c_seasonal3)) )))

se.mat4_sinal = dropFirst((sapply(mse.list, function(i) sqrt(c_sinal4%*%i%*%t(c_sinal4)) )))
se.mat4_seasonal = dropFirst((sapply(mse.list, function(i) sqrt(c_seasonal4%*%i%*%t(c_seasonal4)) )))

se.mat5_sinal = dropFirst((sapply(mse.list, function(i) sqrt(c_sinal5%*%i%*%t(c_sinal5)) )))
se.mat5_seasonal = dropFirst((sapply(mse.list, function(i) sqrt(c_seasonal5%*%i%*%t(c_seasonal5)) )))

se.mat6_sinal = dropFirst((sapply(mse.list, function(i) sqrt(c_sinal6%*%i%*%t(c_sinal6)) )))
se.mat6_seasonal = dropFirst((sapply(mse.list, function(i) sqrt(c_seasonal6%*%i%*%t(c_seasonal6)) )))

se.mat7_sinal = dropFirst((sapply(mse.list, function(i) sqrt(c_sinal7%*%i%*%t(c_sinal7)) )))
se.mat7_seasonal = dropFirst((sapply(mse.list, function(i) sqrt(c_seasonal7%*%i%*%t(c_seasonal7)) )))

se.mat8_sinal = dropFirst((sapply(mse.list, function(i) sqrt(c_sinal8%*%i%*%t(c_sinal8)) )))
se.mat8_seasonal = dropFirst((sapply(mse.list, function(i) sqrt(c_seasonal8%*%i%*%t(c_seasonal8)) )))


modelo_mult$se.original_1<- txse_bh
modelo_mult$se.trend_1 <- se.mat[,1]
modelo_mult$se.slope_1 <- se.mat[,9] # Contei errado o erro padrão do slope dentro da matriz? 
# Por que no original foram contadas 20 pos. além do sinal?
# [,21] no script original corresponde exatamente ao início do componente sazonal
# Olhar linha 438 do scrip de ref.
modelo_mult$se.sampling_e_til_1 <- se.mat[,41]
modelo_mult$se.seasonal_1 <- se.mat1_seasonal
modelo_mult$se.signal_1 <- se.mat1_sinal
modelo_mult$cv.original_1<- cvtx_bh*100
modelo_mult$cv.trend_1<- modelo_mult$se.trend_1/modelo_mult$ts.trend_1*100
modelo_mult$cv.slope_1<- modelo_mult$se.slope_1/modelo_mult$ts.slope_1*100
modelo_mult$cv.sampling_e_til_1<- modelo_mult$se.sampling_e_til_1/modelo_mult$ts.sampling_e_til_1*100
modelo_mult$cv.seasonal_1<- modelo_mult$se.seasonal_1/modelo_mult$ts.seasonal_1*100
modelo_mult$cv.signal_1<- modelo_mult$se.signal_1/modelo_mult$ts.signal_1*100

modelo_mult$se.original_2<- txse_ent
modelo_mult$se.trend_2 <- se.mat[,2]
modelo_mult$se.slope_2 <- se.mat[,10]
modelo_mult$se.sampling_e_til_2 <- se.mat[,42]
modelo_mult$se.seasonal_2 <- se.mat2_seasonal
modelo_mult$se.signal_2 <- se.mat2_sinal
modelo_mult$cv.original_2<- cvtx_ent*100
modelo_mult$cv.trend_2<- modelo_mult$se.trend_2/modelo_mult$ts.trend_2*100
modelo_mult$cv.slope_2<- modelo_mult$se.slope_2/modelo_mult$ts.slope_2*100
modelo_mult$cv.sampling_e_til_2<- modelo_mult$se.sampling_e_til_2/modelo_mult$ts.sampling_e_til_2*100
modelo_mult$cv.seasonal_2<- modelo_mult$se.seasonal_2/modelo_mult$ts.seasonal_2*100
modelo_mult$cv.signal_2<- modelo_mult$se.signal_2/modelo_mult$ts.signal_2*100

modelo_mult$se.original_3<- txse_sul
modelo_mult$se.trend_3 <- se.mat[,3]
modelo_mult$se.slope_3 <- se.mat[,11]
modelo_mult$se.sampling_e_til_3 <- se.mat[,43]
modelo_mult$se.seasonal_3 <- se.mat3_seasonal
modelo_mult$se.signal_3 <- se.mat3_sinal
modelo_mult$cv.original_3<- cvtx_sul*100
modelo_mult$cv.trend_3<- modelo_mult$se.trend_3/modelo_mult$ts.trend_3*100
modelo_mult$cv.slope_3<- modelo_mult$se.slope_3/modelo_mult$ts.slope_3*100
modelo_mult$cv.sampling_e_til_3<- modelo_mult$se.sampling_e_til_3/modelo_mult$ts.sampling_e_til_3*100
modelo_mult$cv.seasonal_3<- modelo_mult$se.seasonal_3/modelo_mult$ts.seasonal_3*100
modelo_mult$cv.signal_3<- modelo_mult$se.signal_3/modelo_mult$ts.signal_3*100

modelo_mult$se.original_4<- txse_trg
modelo_mult$se.trend_4 <- se.mat[,4]
modelo_mult$se.slope_4 <- se.mat[,12]
modelo_mult$se.sampling_e_til_4 <- se.mat[,44]
modelo_mult$se.seasonal_4 <- se.mat4_seasonal
modelo_mult$se.signal_4 <- se.mat4_sinal
modelo_mult$cv.original_4<- cvtx_trg*100
modelo_mult$cv.trend_4<- modelo_mult$se.trend_4/modelo_mult$ts.trend_4*100
modelo_mult$cv.slope_4<- modelo_mult$se.slope_4/modelo_mult$ts.slope_4*100
modelo_mult$cv.sampling_e_til_4<- modelo_mult$se.sampling_e_til_4/modelo_mult$ts.sampling_e_til_4*100
modelo_mult$cv.seasonal_4<- modelo_mult$se.seasonal_4/modelo_mult$ts.seasonal_4*100
modelo_mult$cv.signal_4<- modelo_mult$se.signal_4/modelo_mult$ts.signal_4*100

modelo_mult$se.original_5<- txse_mat
modelo_mult$se.trend_5 <- se.mat[,5]
modelo_mult$se.slope_5 <- se.mat[,13]
modelo_mult$se.sampling_e_til_5 <- se.mat[,45]
modelo_mult$se.seasonal_5 <- se.mat5_seasonal
modelo_mult$se.signal_5 <- se.mat5_sinal
modelo_mult$cv.original_5<- cvtx_mat*100
modelo_mult$cv.trend_5<- modelo_mult$se.trend_5/modelo_mult$ts.trend_5*100
modelo_mult$cv.slope_5<- modelo_mult$se.slope_5/modelo_mult$ts.slope_5*100
modelo_mult$cv.sampling_e_til_5<- modelo_mult$se.sampling_e_til_5/modelo_mult$ts.sampling_e_til_5*100
modelo_mult$cv.seasonal_5<- modelo_mult$se.seasonal_5/modelo_mult$ts.seasonal_5*100
modelo_mult$cv.signal_5<- modelo_mult$se.signal_5/modelo_mult$ts.signal_5*100

modelo_mult$se.original_6<- txse_nrt
modelo_mult$se.trend_6 <- se.mat[,6]
modelo_mult$se.slope_6 <- se.mat[,14]
modelo_mult$se.sampling_e_til_6 <- se.mat[,46]
modelo_mult$se.seasonal_6 <- se.mat6_seasonal
modelo_mult$se.signal_6 <- se.mat6_sinal
modelo_mult$cv.original_6<- cvtx_nrt*100
modelo_mult$cv.trend_6<- modelo_mult$se.trend_6/modelo_mult$ts.trend_6*100
modelo_mult$cv.slope_6<- modelo_mult$se.slope_6/modelo_mult$ts.slope_6*100
modelo_mult$cv.sampling_e_til_6<- modelo_mult$se.sampling_e_til_6/modelo_mult$ts.sampling_e_til_6*100
modelo_mult$cv.seasonal_6<- modelo_mult$se.seasonal_6/modelo_mult$ts.seasonal_6*100
modelo_mult$cv.signal_6<- modelo_mult$se.signal_6/modelo_mult$ts.signal_6*100

modelo_mult$se.original_7<- txse_val
modelo_mult$se.trend_7 <- se.mat[,7]
modelo_mult$se.slope_7 <- se.mat[,15]
modelo_mult$se.sampling_e_til_7 <- se.mat[,47]
modelo_mult$se.seasonal_7 <- se.mat7_seasonal
modelo_mult$se.signal_7 <- se.mat7_sinal
modelo_mult$cv.original_7<- cvtx_val*100
modelo_mult$cv.trend_7<- modelo_mult$se.trend_7/modelo_mult$ts.trend_7*100
modelo_mult$cv.slope_7<- modelo_mult$se.slope_7/modelo_mult$ts.slope_7*100
modelo_mult$cv.sampling_e_til_7<- modelo_mult$se.sampling_e_til_7/modelo_mult$ts.sampling_e_til_7*100
modelo_mult$cv.seasonal_7<- modelo_mult$se.seasonal_7/modelo_mult$ts.seasonal_7*100
modelo_mult$cv.signal_7<- modelo_mult$se.signal_7/modelo_mult$ts.signal_7*100

modelo_mult$se.original_8<- txse_cen
modelo_mult$se.trend_8 <- se.mat[,8]
modelo_mult$se.slope_8 <- se.mat[,16]
modelo_mult$se.sampling_e_til_8 <- se.mat[,48]
modelo_mult$se.seasonal_8 <- se.mat8_seasonal
modelo_mult$se.signal_8 <- se.mat8_sinal
modelo_mult$cv.original_8<- cvtx_cen*100
modelo_mult$cv.trend_8<- modelo_mult$se.trend_8/modelo_mult$ts.trend_8*100
modelo_mult$cv.slope_8<- modelo_mult$se.slope_8/modelo_mult$ts.slope_8*100
modelo_mult$cv.sampling_e_til_8<- modelo_mult$se.sampling_e_til_8/modelo_mult$ts.sampling_e_til_8*100
modelo_mult$cv.seasonal_8<- modelo_mult$se.seasonal_8/modelo_mult$ts.seasonal_8*100
modelo_mult$cv.signal_8<- modelo_mult$se.signal_8/modelo_mult$ts.signal_8*100


round(exp(modelo_mult$fit$par[1:40]),4)
round(tanh(modelo_mult$fit$par[41:68]),4)


par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_1<- window(ts.union(
  ts(modelo_mult$ts.original_1,start = 2012,frequency=4),
  ts(modelo_mult$ts.signal_1,start = 2012,frequency=4),
  ts(modelo_mult$ts.trend_1,start = 2012,frequency=4)),start=c(2013,4))
plot(fig_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Desocupação",
                            "Sinal da desocupação: model-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Desocupação (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_1<- window(ts.union(
  ts(modelo_mult$cv.original_1,start = 2012,frequency=4),
  ts(modelo_mult$cv.signal_1,start = 2012,frequency=4),
  ts(modelo_mult$cv.trend_1,start = 2012,frequency=4)),start=c(2013,4))
plot(fig.cv_1, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV da desocupação",
                             "CV do sinal da desocupação: model-based",
                             "CV da tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte", side = 3, outer = TRUE, line = 0.5)

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_2<- window(ts.union(
  ts(modelo_mult$ts.original_2,start = 2012,frequency=4),
  ts(modelo_mult$ts.signal_2,start = 2012,frequency=4),
  ts(modelo_mult$ts.trend_2,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_2, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Desocupação",
                            "Sinal da desocupação: model-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_2<- window(ts.union(
  ts(modelo_mult$cv.original_2,start = 2012,frequency=4),
  ts(modelo_mult$cv.signal_2,start = 2012,frequency=4),
  ts(modelo_mult$cv.trend_2,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_2, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV da desocupação",
                             "CV do sinal da desocupação: model-based",
                             "CV da tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("02 - Colar e Entorno de Belo Horizonte", side = 3, outer = TRUE, line = 0.5)

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_3<- window(ts.union(
  ts(modelo_mult$ts.original_3,start = 2012,frequency=4),
  ts(modelo_mult$ts.signal_3,start = 2012,frequency=4),
  ts(modelo_mult$ts.trend_3,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_3, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Desocupação",
                            "Sinal da desocupação: model-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_3<- window(ts.union(
  ts(modelo_mult$cv.original_3,start = 2012,frequency=4),
  ts(modelo_mult$cv.signal_3,start = 2012,frequency=4),
  ts(modelo_mult$cv.trend_3,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_3, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV da desocupação",
                             "CV do sinal da desocupação: model-based",
                             "CV da tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Sul de Minas", side = 3, outer = TRUE, line = 0.5)

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_4<- window(ts.union(
  ts(modelo_mult$ts.original_4,start = 2012,frequency=4),
  ts(modelo_mult$ts.signal_4,start = 2012,frequency=4),
  ts(modelo_mult$ts.trend_4,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_4, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Desocupação",
                            "Sinal da desocupação: model-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_4<- window(ts.union(
  ts(modelo_mult$cv.original_4,start = 2012,frequency=4),
  ts(modelo_mult$cv.signal_4,start = 2012,frequency=4),
  ts(modelo_mult$cv.trend_4,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_4, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV da desocupação",
                             "CV do sinal da desocupação: model-based",
                             "CV da tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("04 - Triângulo Mineiro", side = 3, outer = TRUE, line = 0.5)

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_5<- window(ts.union(
  ts(modelo_mult$ts.original_5,start = 2012,frequency=4),
  ts(modelo_mult$ts.signal_5,start = 2012,frequency=4),
  ts(modelo_mult$ts.trend_5,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_5, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Desocupação",
                            "Sinal da desocupação: model-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_5<- window(ts.union(
  ts(modelo_mult$cv.original_5,start = 2012,frequency=4),
  ts(modelo_mult$cv.signal_5,start = 2012,frequency=4),
  ts(modelo_mult$cv.trend_5,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_5, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2),ylim=c(3,20))
legend("topleft", legend = c("CV da desocupação",
                             "CV do sinal da desocupação: model-based",
                             "CV da tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05 - Zona da Mata", side = 3, outer = TRUE, line = 0.5)

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_6<- window(ts.union(
  ts(modelo_mult$ts.original_6,start = 2012,frequency=4),
  ts(modelo_mult$ts.signal_6,start = 2012,frequency=4),
  ts(modelo_mult$ts.trend_6,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_6, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Desocupação",
                            "Sinal da desocupação: model-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_6<- window(ts.union(
  ts(modelo_mult$cv.original_6,start = 2012,frequency=4),
  ts(modelo_mult$cv.signal_6,start = 2012,frequency=4),
  ts(modelo_mult$cv.trend_6,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_6, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV da desocupação",
                             "CV do sinal da desocupação: model-based",
                             "CV da tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5)

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_7<- window(ts.union(
  ts(txdesoc_val,start = 2012,frequency=4),
  ts(modelo_mult$ts.signal_7,start = 2012,frequency=4),
  ts(modelo_mult$ts.trend_7,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_7, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Desocupação",
                            "Sinal da desocupação: model-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_7<- window(ts.union(
  ts(modelo_mult$cv.original_7,start = 2012,frequency=4),
  ts(modelo_mult$cv.signal_7,start = 2012,frequency=4),
  ts(modelo_mult$cv.trend_7,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_7, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV da desocupação",
                             "CV do sinal da desocupação: model-based",
                             "CV da tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07 - Vale do Rio Doce", side = 3, outer = TRUE, line = 0.5)

par(mfrow=c(1,2),mar=c(5,5,1,1),cex=0.8)
fig_8<- window(ts.union(
  ts(modelo_mult$ts.original_8,start = 2012,frequency=4),
  ts(modelo_mult$ts.signal_8,start = 2012,frequency=4),
  ts(modelo_mult$ts.trend_8,start = 2012,frequency=4)),start=c(2013,3))
plot(fig_8, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("bottom", legend = c("Desocupação",
                            "Sinal da desocupação: model-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("Unemployment (thousand persons)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig.cv_8<- window(ts.union(
  ts(modelo_mult$cv.original_8,start = 2012,frequency=4),
  ts(modelo_mult$cv.signal_8,start = 2012,frequency=4),
  ts(modelo_mult$cv.trend_8,start = 2012,frequency=4)),start=c(2013,3))
plot(fig.cv_8, plot.type = "single", col = c(1,2,3,4), ylab="", xlab="",lty = c(1,1,1),lwd=c(2))
legend("topleft", legend = c("CV da desocupação",
                             "CV do sinal da desocupação: model-based",
                             "CV da tendência da desocupação: model-based"),
       lty = c(1,1,1), col = c(1,2,3), bty = 'n',lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Central", side = 3, outer = TRUE, line = 0.5)


### Testes de diagnóstico

source("data/funcoes/05_teste_H.R")

diagmult <- matrix(NA, nrow = 3, ncol = 8)
colnames(diagmult) <- c("BH", "ENT", "SUL", "TRG", "MAT", "NRT", "VAL", "CEN")
rownames(diagmult) <- c("Shapiro", "Box", "Teste H")

# 01 - Belo Horizonte

shap_bh <- round(
  shapiro.test(modelo_mult[["res"]][,1][modelo_mult[["d"]]:modelo_mult[["T"]]])[["p.value"]],
  5
)

box_bh <- round((Box.test(modelo_mult[["res"]][,1][modelo_mult[["d"]]:modelo_mult[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5)

testh_bh <- teste_H(modelo_mult[["res"]][,1][modelo_mult[["d"]]:modelo_mult[["T"]]])

diagmult["Shapiro", "BH"]  <- shap_bh
diagmult["Box",     "BH"]  <- box_bh
diagmult["Teste H", "BH"]  <- testh_bh

# 02 - Colar e Entorno de BH

shap_ent <- round(
  shapiro.test(modelo_mult[["res"]][,2][modelo_mult[["d"]]:modelo_mult[["T"]]])[["p.value"]],
  5
)

box_ent <- round((Box.test(modelo_mult[["res"]][,2][modelo_mult[["d"]]:modelo_mult[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5)

testh_ent <- teste_H(modelo_mult[["res"]][,2][modelo_mult[["d"]]:modelo_mult[["T"]]])

diagmult["Shapiro", "ENT"] <- shap_ent
diagmult["Box",     "ENT"] <- box_ent
diagmult["Teste H", "ENT"] <- testh_ent

# 03 - Sul de Minas

shap_sul <- round(
  shapiro.test(modelo_mult[["res"]][,3][modelo_mult[["d"]]:modelo_mult[["T"]]])[["p.value"]],
  5
)

box_sul <- round((Box.test(modelo_mult[["res"]][,3][modelo_mult[["d"]]:modelo_mult[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5)

testh_sul <- teste_H(modelo_mult[["res"]][,3][modelo_mult[["d"]]:modelo_mult[["T"]]])

diagmult["Shapiro", "SUL"] <- shap_sul
diagmult["Box",     "SUL"] <- box_sul
diagmult["Teste H", "SUL"] <- testh_sul

# 04 - Triângulo Mineiro

shap_trg <- round(
  shapiro.test(modelo_mult[["res"]][,4][modelo_mult[["d"]]:modelo_mult[["T"]]])[["p.value"]],
  5
)

box_trg <- round((Box.test(modelo_mult[["res"]][,4][modelo_mult[["d"]]:modelo_mult[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5)

testh_trg <- teste_H(modelo_mult[["res"]][,4][modelo_mult[["d"]]:modelo_mult[["T"]]])

diagmult["Shapiro", "TRG"] <- shap_trg
diagmult["Box",     "TRG"] <- box_trg
diagmult["Teste H", "TRG"] <- testh_trg

# 05 - Zona da Mata

shap_mat <- round(
  shapiro.test(modelo_mult[["res"]][,5][modelo_mult[["d"]]:modelo_mult[["T"]]])[["p.value"]],
  5
)

box_mat <- round((Box.test(modelo_mult[["res"]][,5][modelo_mult[["d"]]:modelo_mult[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5)

testh_mat <- teste_H(modelo_mult[["res"]][,5][modelo_mult[["d"]]:modelo_mult[["T"]]])

diagmult["Shapiro", "MAT"] <- shap_mat
diagmult["Box",     "MAT"] <- box_mat
diagmult["Teste H", "MAT"] <- testh_mat

# 06 - Norte de Minas

shap_nrt <- round(
  shapiro.test(modelo_mult[["res"]][,6][modelo_mult[["d"]]:modelo_mult[["T"]]])[["p.value"]],
  5
)

box_nrt <- round((Box.test(modelo_mult[["res"]][,6][modelo_mult[["d"]]:modelo_mult[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5)

testh_nrt <- teste_H(modelo_mult[["res"]][,6][modelo_mult[["d"]]:modelo_mult[["T"]]])

diagmult["Shapiro", "NRT"] <- shap_nrt
diagmult["Box",     "NRT"] <- box_nrt
diagmult["Teste H", "NRT"] <- testh_nrt

# 07 - Vale do Rio Doce

shap_val <- round(
  shapiro.test(modelo_mult[["res"]][,7][modelo_mult[["d"]]:modelo_mult[["T"]]])[["p.value"]],
  5
)

box_val <- round((Box.test(modelo_mult[["res"]][,7][modelo_mult[["d"]]:modelo_mult[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5)

testh_val <- teste_H(modelo_mult[["res"]][,7][modelo_mult[["d"]]:modelo_mult[["T"]]])

diagmult["Shapiro", "VAL"] <- shap_val
diagmult["Box",     "VAL"] <- box_val
diagmult["Teste H", "VAL"] <- testh_val

# 08 - Central

shap_cen <- round(
  shapiro.test(modelo_mult[["res"]][,8][modelo_mult[["d"]]:modelo_mult[["T"]]])[["p.value"]],
  5
)

box_cen <- round((Box.test(modelo_mult[["res"]][,8][modelo_mult[["d"]]:modelo_mult[["T"]]], lag = 24, type = "Ljung"))[["p.value"]],5)

testh_cen <- teste_H(modelo_mult[["res"]][,8][modelo_mult[["d"]]:modelo_mult[["T"]]])

diagmult["Shapiro", "CEN"] <- shap_cen
diagmult["Box",     "CEN"] <- box_cen
diagmult["Teste H", "CEN"] <- testh_cen

View(diagmult)

save.image(file = "C:/FJP2425/Programacao/data/Rdatas/16_multivariado_comcorr - taxadesoc_8reg/estimados/01_taxamod_comcorr.Rdata")

#load("D:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/01_mod_comcorr_SULARMA11.Rdata")


#### DADOS PARA ARTIGO #########################################################

rm(list = ls())
gc()

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
bh<-baseestr8reg$`01-Belo Horizonte`
ent<-baseestr8reg$`02-Colar e Entorno metropolitano de BH`
sul<-baseestr8reg$`03-Sul de Minas`
trg<-baseestr8reg$`04-Triângulo Mineiro`
mat<-baseestr8reg$`05-Mata de Minas Gerais`
nrt<-baseestr8reg$`06-Norte de Minas`
vl<-baseestr8reg$`07-Vale do Rio Doce`
cen<-baseestr8reg$`08-Central`

env1<-new.env()
load("C:/FJP2425/Programacao/data/Rdatas/16_multivariado_comcorr - taxadesoc_8reg/estimados/01_taxamod_comcorr.Rdata", envir = env1)
modmult<-env1$modelo_mult

# BH

est_direta_bh <- bh$Taxa.de.desocupação
cv_direta_bh <- baseestr8reg$`01-Belo Horizonte`$CV.taxa
se_dir_bh <- baseestr8reg$`01-Belo Horizonte`$sd_txd
est_direta_bh <- est_direta_bh*100
se_dir_bh <- se_dir_bh*100

ICinf_direta_bh <- est_direta_bh-1.96*se_dir_bh
ICsup_direta_bh <- est_direta_bh+1.96*se_dir_bh

est_direta_bh <-  window(ts.union(ts(est_direta_bh, start = 2012, frequency = 4)), start = c(2014,1))
cv_direta_bh <-  window(ts.union(ts(cv_direta_bh, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_direta_bh <- window(ts.union(ts(ICinf_direta_bh, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_direta_bh <- window(ts.union(ts(ICsup_direta_bh, start = 2012, frequency = 4)), start = c(2014,1))

sig_bh <- modmult$ts.signal_1
cvsig_bh <- modmult$cv.signal_1
sesig_bh <- modmult$se.signal_1
sigICinf_bh <- sig_bh-1.96*sesig_bh
sigICsup_bh <- sig_bh+1.96*sesig_bh

sig_bh <-  window(ts.union(ts(sig_bh, start = 2012, frequency = 4)), start = c(2014,1))
cvsig_bh <-  window(ts.union(ts(cvsig_bh, start = 2012, frequency = 4)), start = c(2014,1))
sigICinf_bh <- window(ts.union(ts(sigICinf_bh, start = 2012, frequency = 4)), start = c(2014,1))
sigICsup_bh <- window(ts.union(ts(sigICsup_bh, start = 2012, frequency = 4)), start = c(2014,1))

est_direta_bh
cv_direta_bh
ICinf_direta_bh
ICsup_direta_bh

sig_bh
cvsig_bh
sigICinf_bh
sigICsup_bh

# ENT
est_direta_ent <- ent$Taxa.de.desocupação
cv_direta_ent <- baseestr8reg$`02-Colar e Entorno metropolitano de BH`$CV.taxa
se_dir_ent <- baseestr8reg$`02-Colar e Entorno metropolitano de BH`$sd_txd
est_direta_ent <- est_direta_ent*100
se_dir_ent <- se_dir_ent*100

ICinf_direta_ent <- est_direta_ent-1.96*se_dir_ent
ICsup_direta_ent <- est_direta_ent+1.96*se_dir_ent

est_direta_ent <-  window(ts.union(ts(est_direta_ent, start = 2012, frequency = 4)), start = c(2014,1))
cv_direta_ent <-  window(ts.union(ts(cv_direta_ent, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_direta_ent <- window(ts.union(ts(ICinf_direta_ent, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_direta_ent <- window(ts.union(ts(ICsup_direta_ent, start = 2012, frequency = 4)), start = c(2014,1))

sig_ent <- modmult$ts.signal_2
cvsig_ent <- modmult$cv.signal_2
sesig_ent <- modmult$se.signal_2
sigICinf_ent <- sig_ent-1.96*sesig_ent
sigICsup_ent <- sig_ent+1.96*sesig_ent

sig_ent <-  window(ts.union(ts(sig_ent, start = 2012, frequency = 4)), start = c(2014,1))
cvsig_ent <-  window(ts.union(ts(cvsig_ent, start = 2012, frequency = 4)), start = c(2014,1))
sigICinf_ent <- window(ts.union(ts(sigICinf_ent, start = 2012, frequency = 4)), start = c(2014,1))
sigICsup_ent <- window(ts.union(ts(sigICsup_ent, start = 2012, frequency = 4)), start = c(2014,1))

est_direta_ent
cv_direta_ent
ICinf_direta_ent
ICsup_direta_ent

sig_ent
cvsig_ent
sigICinf_ent
sigICsup_ent

# SUL
est_direta_sul <- sul$Taxa.de.desocupação
cv_direta_sul <- baseestr8reg$`03-Sul de Minas`$CV.taxa
se_dir_sul <- baseestr8reg$`03-Sul de Minas`$sd_txd
est_direta_sul <- est_direta_sul*100
se_dir_sul <- se_dir_sul*100

ICinf_direta_sul <- est_direta_sul-1.96*se_dir_sul
ICsup_direta_sul <- est_direta_sul+1.96*se_dir_sul

est_direta_sul <-  window(ts.union(ts(est_direta_sul, start = 2012, frequency = 4)), start = c(2014,1))
cv_direta_sul <-  window(ts.union(ts(cv_direta_sul, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_direta_sul <- window(ts.union(ts(ICinf_direta_sul, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_direta_sul <- window(ts.union(ts(ICsup_direta_sul, start = 2012, frequency = 4)), start = c(2014,1))

sig_sul <- modmult$ts.signal_3
cvsig_sul <- modmult$cv.signal_3
sesig_sul <- modmult$se.signal_3
sigICinf_sul <- sig_sul-1.96*sesig_sul
sigICsup_sul <- sig_sul+1.96*sesig_sul

sig_sul <-  window(ts.union(ts(sig_sul, start = 2012, frequency = 4)), start = c(2014,1))
cvsig_sul <-  window(ts.union(ts(cvsig_sul, start = 2012, frequency = 4)), start = c(2014,1))
sigICinf_sul <- window(ts.union(ts(sigICinf_sul, start = 2012, frequency = 4)), start = c(2014,1))
sigICsup_sul <- window(ts.union(ts(sigICsup_sul, start = 2012, frequency = 4)), start = c(2014,1))

est_direta_sul
cv_direta_sul
ICinf_direta_sul
ICsup_direta_sul

sig_sul
cvsig_sul
sigICinf_sul
sigICsup_sul

# TRG
est_direta_trg <- trg$Taxa.de.desocupação
cv_direta_trg <- baseestr8reg$`04-Triângulo Mineiro`$CV.taxa
se_dir_trg <- baseestr8reg$`04-Triângulo Mineiro`$sd_txd
est_direta_trg <- est_direta_trg*100
se_dir_trg <- se_dir_trg*100

ICinf_direta_trg <- est_direta_trg-1.96*se_dir_trg
ICsup_direta_trg <- est_direta_trg+1.96*se_dir_trg

est_direta_trg <-  window(ts.union(ts(est_direta_trg, start = 2012, frequency = 4)), start = c(2014,1))
cv_direta_trg <-  window(ts.union(ts(cv_direta_trg, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_direta_trg <- window(ts.union(ts(ICinf_direta_trg, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_direta_trg <- window(ts.union(ts(ICsup_direta_trg, start = 2012, frequency = 4)), start = c(2014,1))

sig_trg <- modmult$ts.signal_4
cvsig_trg <- modmult$cv.signal_4
sesig_trg <- modmult$se.signal_4
sigICinf_trg <- sig_trg-1.96*sesig_trg
sigICsup_trg <- sig_trg+1.96*sesig_trg

sig_trg <-  window(ts.union(ts(sig_trg, start = 2012, frequency = 4)), start = c(2014,1))
cvsig_trg <-  window(ts.union(ts(cvsig_trg, start = 2012, frequency = 4)), start = c(2014,1))
sigICinf_trg <- window(ts.union(ts(sigICinf_trg, start = 2012, frequency = 4)), start = c(2014,1))
sigICsup_trg <- window(ts.union(ts(sigICsup_trg, start = 2012, frequency = 4)), start = c(2014,1))

est_direta_trg
cv_direta_trg
ICinf_direta_trg
ICsup_direta_trg

sig_trg
cvsig_trg
sigICinf_trg
sigICsup_trg

# MAT
est_direta_mat <- mat$Taxa.de.desocupação
cv_direta_mat <- baseestr8reg$`05-Mata de Minas Gerais`$CV.taxa
se_dir_mat <- baseestr8reg$`05-Mata de Minas Gerais`$sd_txd
est_direta_mat <- est_direta_mat*100
se_dir_mat <- se_dir_mat*100

ICinf_direta_mat <- est_direta_mat-1.96*se_dir_mat
ICsup_direta_mat <- est_direta_mat+1.96*se_dir_mat

est_direta_mat <-  window(ts.union(ts(est_direta_mat, start = 2012, frequency = 4)), start = c(2014,1))
cv_direta_mat <-  window(ts.union(ts(cv_direta_mat, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_direta_mat <- window(ts.union(ts(ICinf_direta_mat, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_direta_mat <- window(ts.union(ts(ICsup_direta_mat, start = 2012, frequency = 4)), start = c(2014,1))

sig_mat <- modmult$ts.signal_5
cvsig_mat <- modmult$cv.signal_5
sesig_mat <- modmult$se.signal_5
sigICinf_mat <- sig_mat-1.96*sesig_mat
sigICsup_mat <- sig_mat+1.96*sesig_mat

sig_mat <-  window(ts.union(ts(sig_mat, start = 2012, frequency = 4)), start = c(2014,1))
cvsig_mat <-  window(ts.union(ts(cvsig_mat, start = 2012, frequency = 4)), start = c(2014,1))
sigICinf_mat <- window(ts.union(ts(sigICinf_mat, start = 2012, frequency = 4)), start = c(2014,1))
sigICsup_mat <- window(ts.union(ts(sigICsup_mat, start = 2012, frequency = 4)), start = c(2014,1))

est_direta_mat
cv_direta_mat
ICinf_direta_mat
ICsup_direta_mat

sig_mat
cvsig_mat
sigICinf_mat
sigICsup_mat

# NRT
est_direta_nrt <- nrt$Taxa.de.desocupação
cv_direta_nrt <- baseestr8reg$`06-Norte de Minas`$CV.taxa
se_dir_nrt <- baseestr8reg$`06-Norte de Minas`$sd_txd
est_direta_nrt <- est_direta_nrt*100
se_dir_nrt <- se_dir_nrt*100

ICinf_direta_nrt <- est_direta_nrt-1.96*se_dir_nrt
ICsup_direta_nrt <- est_direta_nrt+1.96*se_dir_nrt

est_direta_nrt <-  window(ts.union(ts(est_direta_nrt, start = 2012, frequency = 4)), start = c(2014,1))
cv_direta_nrt <-  window(ts.union(ts(cv_direta_nrt, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_direta_nrt <- window(ts.union(ts(ICinf_direta_nrt, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_direta_nrt <- window(ts.union(ts(ICsup_direta_nrt, start = 2012, frequency = 4)), start = c(2014,1))

sig_nrt <- modmult$ts.signal_6
cvsig_nrt <- modmult$cv.signal_6
sesig_nrt <- modmult$se.signal_6
sigICinf_nrt <- sig_nrt-1.96*sesig_nrt
sigICsup_nrt <- sig_nrt+1.96*sesig_nrt

sig_nrt <-  window(ts.union(ts(sig_nrt, start = 2012, frequency = 4)), start = c(2014,1))
cvsig_nrt <-  window(ts.union(ts(cvsig_nrt, start = 2012, frequency = 4)), start = c(2014,1))
sigICinf_nrt <- window(ts.union(ts(sigICinf_nrt, start = 2012, frequency = 4)), start = c(2014,1))
sigICsup_nrt <- window(ts.union(ts(sigICsup_nrt, start = 2012, frequency = 4)), start = c(2014,1))

est_direta_nrt
cv_direta_nrt
ICinf_direta_nrt
ICsup_direta_nrt

sig_nrt
cvsig_nrt
sigICinf_nrt
sigICsup_nrt

# VAL
est_direta_val <- vl$Taxa.de.desocupação
cv_direta_val <- baseestr8reg$`07-Vale do Rio Doce`$CV.taxa
se_dir_val <- baseestr8reg$`07-Vale do Rio Doce`$sd_txd
est_direta_val <- est_direta_val*100
se_dir_val <- se_dir_val*100

ICinf_direta_val <- est_direta_val-1.96*se_dir_val
ICsup_direta_val <- est_direta_val+1.96*se_dir_val

est_direta_val <-  window(ts.union(ts(est_direta_val, start = 2012, frequency = 4)), start = c(2014,1))
cv_direta_val <-  window(ts.union(ts(cv_direta_val, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_direta_val <- window(ts.union(ts(ICinf_direta_val, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_direta_val <- window(ts.union(ts(ICsup_direta_val, start = 2012, frequency = 4)), start = c(2014,1))

sig_val <- modmult$ts.signal_7
cvsig_val <- modmult$cv.signal_7
sesig_val <- modmult$se.signal_7
sigICinf_val <- sig_val-1.96*sesig_val
sigICsup_val <- sig_val+1.96*sesig_val

sig_val <-  window(ts.union(ts(sig_val, start = 2012, frequency = 4)), start = c(2014,1))
cvsig_val <-  window(ts.union(ts(cvsig_val, start = 2012, frequency = 4)), start = c(2014,1))
sigICinf_val <- window(ts.union(ts(sigICinf_val, start = 2012, frequency = 4)), start = c(2014,1))
sigICsup_val <- window(ts.union(ts(sigICsup_val, start = 2012, frequency = 4)), start = c(2014,1))

est_direta_val
cv_direta_val
ICinf_direta_val
ICsup_direta_val

sig_val
cvsig_val
sigICinf_val
sigICsup_val

# CEN
est_direta_cen <- cen$Taxa.de.desocupação
cv_direta_cen <- baseestr8reg$`08-Central`$CV.taxa
se_dir_cen <- baseestr8reg$`08-Central`$sd_txd
est_direta_cen <- est_direta_cen*100
se_dir_cen <- se_dir_cen*100

ICinf_direta_cen <- est_direta_cen-1.96*se_dir_cen
ICsup_direta_cen <- est_direta_cen+1.96*se_dir_cen

est_direta_cen <-  window(ts.union(ts(est_direta_cen, start = 2012, frequency = 4)), start = c(2014,1))
cv_direta_cen <-  window(ts.union(ts(cv_direta_cen, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_direta_cen <- window(ts.union(ts(ICinf_direta_cen, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_direta_cen <- window(ts.union(ts(ICsup_direta_cen, start = 2012, frequency = 4)), start = c(2014,1))

sig_cen <- modmult$ts.signal_8
cvsig_cen <- modmult$cv.signal_8
sesig_cen <- modmult$se.signal_8
sigICinf_cen <- sig_cen-1.96*sesig_cen
sigICsup_cen <- sig_cen+1.96*sesig_cen

sig_cen <-  window(ts.union(ts(sig_cen, start = 2012, frequency = 4)), start = c(2014,1))
cvsig_cen <-  window(ts.union(ts(cvsig_cen, start = 2012, frequency = 4)), start = c(2014,1))
sigICinf_cen <- window(ts.union(ts(sigICinf_cen, start = 2012, frequency = 4)), start = c(2014,1))
sigICsup_cen <- window(ts.union(ts(sigICsup_cen, start = 2012, frequency = 4)), start = c(2014,1))

est_direta_cen
cv_direta_cen
ICinf_direta_cen
ICsup_direta_cen

sig_cen
cvsig_cen
sigICinf_cen
sigICsup_cen



