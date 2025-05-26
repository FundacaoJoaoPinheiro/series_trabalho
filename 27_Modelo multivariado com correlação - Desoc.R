################################################################################
##                  MODELO MULTIVARIADO SEM CORRELAÇÃO                        ##
################################################################################

library(dlm)
library(tidyverse)
library(beepr)
library(gdata)

rm(list=ls())
gc()
options(scipen=999)

#### BASE DE DADOS #############################################################

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")

# Environment para cada região

env1<-new.env()
env2<-new.env()
env3<-new.env()
env4<-new.env()
env5<-new.env()
env6<-new.env()
env7<-new.env()
env8<-new.env()
env9<-new.env()

# 01 - bh

bh<-baseestr8reg$`01-Belo Horizonte`
dbbh<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/01_params_bh.RDS")
desoc_bh <- bh$Total.de.desocupados/1000
se_bh<- bh$sd_d/1000
cv_bh <- se_bh/desoc_bh

theta1_ma1_bh <- dbbh[["mod_ma1"]][["theta1_ma1_dbh"]]
load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/01_mod_bh.Rdata", envir = env1)
initial_bh <- env1$ma1_bh[["initial"]]
estimated_bh <- env1$ma1_bh[["fit"]][["par"]]

# 02 - ent

ent<-baseestr8reg$`02-Colar e Entorno metropolitano de BH`
dbent<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/02_params_ent.RDS")
desoc_ent<-(ent$Total.de.desocupados)/1000
se_ent <- (ent$sd_d)/1000
cv_ent <- se_ent/desoc_ent

theta1_ma1_ent <- dbent[["mod_ma1"]][["theta1_ma1_dent"]]
load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/02_mod_ent.Rdata", envir = env2)
initial_ent <- env2$ma1_ent[["initial"]]
estimated_ent <- env2$ma1_ent[["fit"]][["par"]]

# 03 - sul (teste com resultado ma1)

sul<-baseestr8reg$`03-Sul de Minas`
dbsul<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/03_params_sul.RDS")
desoc_sul <- sul$Total.de.desocupados/1000
se_sul <- sul$sd_d/1000
cv_sul <- se_sul/desoc_sul

phi1_ar1_sul <- dbsul[["mod_ar1"]][["phi1_ar1_dsul"]]
theta1_ma1_sul <- dbsul[["mod_ma1"]][["theta1_ma1_dsul"]]
load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/03_mod_sul.Rdata", envir = env3)
initial_sul <- env3$ma1_sul[["initial"]]
estimated_sul <- env3$ma1_sul[["fit"]][["par"]]

# 04 - trg

trg <- baseestr8reg$`04-Triângulo Mineiro`
dbtrg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/04_params_trg.RDS")
desoc_trg<-trg$Total.de.desocupados/1000
se_trg <- trg$sd_d/1000
cv_trg <- se_trg/desoc_trg

theta1_ma1_trg <- dbtrg[["mod_ma1"]][["theta1_ma1_dtrg"]]
load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/04_mod_trg.Rdata", envir = env4)
initial_trg <- env4$ma1_trg[["initial"]]
estimated_trg <- env4$ma1_trg[["fit"]][["par"]]

# 05 - mat

mat<-baseestr8reg$`05-Mata de Minas Gerais`
dbmat<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/05_params_mat.RDS")
desoc_mat<- mat$Total.de.desocupados/1000
se_mat<-mat$sd_d/1000
cv_mat<-se_mat/desoc_mat

theta1_ma1_mat <- dbmat[["mod_ma1"]][["theta1_ma1_dmat"]]
load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/05_mod_mat.Rdata", envir = env5)
initial_mat <- env5$ma1_mat[["initial"]]
estimated_mat <- env5$ma1_mat[["fit"]][["par"]]

# 06 - nrt

nrt<-baseestr8reg$`06-Norte de Minas`
dbnrt<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/06_params_nrt.RDS")
desoc_nrt<-nrt$Total.de.desocupados/1000
se_nrt <- nrt$sd_d/1000
cv_nrt <- se_nrt/desoc_nrt

theta1_ma1_nrt <- dbnrt[["mod_ma1"]][["theta1_ma1_dnrt"]]
load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/06_mod_nrt.Rdata", envir = env6)
initial_nrt <- env6$ma1_nrt[["initial"]]
estimated_nrt <- env6$ma1_nrt[["fit"]][["par"]]

# 07 - val (ar1)

val<-baseestr8reg$`07-Vale do Rio Doce`
dbval<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/07_params_rio.RDS")
desoc_val<-val$Total.de.desocupados/1000
se_val<- val$sd_d/1000
cv_val<- se_val/desoc_val

phi1_ar1_val <- dbval[["mod_ar1"]][["phi1_ar1_drio"]]
load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/07_mod_val.Rdata", envir = env7)
initial_val <- env7$ar1_val[["initial"]]
estimated_val <- env7$ar1_val[["fit"]][["par"]]

# 08 - cen

cen<-baseestr8reg$`08-Central`
dbcen<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/08_params_cen.RDS")
desoc_cen<-cen$Total.de.desocupados/1000
se_cen<-cen$sd_d/1000
cv_cen<-se_cen/desoc_cen

theta1_ma1_cen <- dbcen[["mod_ma1"]][["theta1_ma1_dcen"]]
load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/08_mod_cen.Rdata", envir = env8)
initial_cen <- env8$ma1_cen[["initial"]]
estimated_cen <- env8$ma1_cen[["fit"]][["par"]]

# 09 - mg

#mg<-baseestr8reg$`09 - Minas Gerais`
#dbmg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/09_params_mg.RDS")
#desoc_mg<-mg$Total.de.desocupados/1000
#se_mg<-mg$sd_d/1000
#cv_mg<-se_mg/desoc_mg

#theta1_ma1_mg <- dbmg[["mod_ma1"]][["theta1_ma1_dmg"]]
#load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/09_mod_mg.Rdata", envir = env9)
#initial_mg <- env9$ma1_mg[["initial"]]
#estimated_mg <- env9$ma1_mg[["fit"]][["par"]]

keep(desoc_bh,se_bh,cv_bh,theta1_ma1_bh,initial_bh,estimated_bh,
     desoc_ent,se_ent,cv_ent,theta1_ma1_ent,initial_ent,estimated_ent,
     desoc_sul,se_sul,cv_sul,phi1_ar1_sul,theta1_ma1_sul,initial_sul,estimated_sul,
     desoc_trg,se_trg,cv_trg,theta1_ma1_trg,initial_trg,estimated_trg,
     desoc_mat,se_mat,cv_mat,theta1_ma1_mat,initial_mat,estimated_mat,
     desoc_nrt,se_nrt,cv_nrt,theta1_ma1_nrt,initial_nrt,estimated_nrt,
     desoc_val,se_val,cv_val,phi1_ar1_val,initial_val,estimated_val,
     desoc_cen,se_cen,cv_cen,theta1_ma1_cen,initial_cen,estimated_cen,
     sure=TRUE)
gc()


## TESTANDO MODELO COM CORRELAÇÃO E SEM BENCHMARKING

modelo_mult<- list("fn"=function(params){
  m = dlmModPoly(2) + dlmModTrig(4) + dlmModReg(se_bh,addInt = FALSE) # Erro de bh como ref do modelo
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
  
  X <- cbind(se_bh,se_ent,se_sul,se_trg,se_mat,se_nrt,se_val,se_cen)
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
  GG[43,43] <- phi1_ar1_sul
  GG[44,44] <- 0
  GG[45,45] <- 0
  GG[46,46] <- 0
  GG[47,47] <- phi1_ar1_val
  GG[48,48] <- 0
  
  GG[41,49] <- theta1_ma1_bh
  GG[42,50] <- theta1_ma1_ent
  GG[43,51] <- theta1_ma1_sul
  GG[44,52] <- theta1_ma1_trg
  GG[45,53] <- theta1_ma1_mat
  GG[46,54] <- theta1_ma1_nrt
  GG[47,55] <- 0
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
  
  W[41,41] <- exp(params[33])
  W[42,42] <- exp(params[34])
  W[43,43] <- exp(params[35])
  W[44,44] <- exp(params[36])
  W[45,45] <- exp(params[37])
  W[46,46] <- exp(params[38])
  W[47,47] <- exp(params[39])
  W[48,48] <- exp(params[40])
  
  ## Adicionando a correlacao entre as séries
  
  # tanh -> garante que a correlação fique entre -1 e 1. Tangente hiperbólica
    # Por que eu começo o cálculo dessa covariância apenas a partir do [12,11]? 
      # Qual o motivo dessa posição da matriz?
  
  # Fiz o ajuste das correlações conforme as posições do slope de cada região
  #  posições de 9 a 16
  
  W[10,9] <- W[9,10] <- tanh(params[41]) * prod(exp(0.5 * params[c(9,10)])) # BH COM ENT
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
  
  W[16,15] <- W[16,16] <- tanh(params[68]) * prod(exp(0.5 * params[c(17,18)])) # VAL COM CEN
  
  m$W <- W
  
  m$m0<- rep(0,7)
  m0 <- m$m0 %x% diag(8)
  m$m0 <- m0
  
  m$C0<- diag(x=10^7,7)
  C0 <- m$C0 %x% diag(8)
  m$C0 <- C0
  
  return(m)
})


