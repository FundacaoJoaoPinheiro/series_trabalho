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

data <- cbind(desoc_bh,desoc_ent,desoc_sul,desoc_trg,desoc_mat,desoc_nrt,desoc_val,desoc_cen)

modelo_mult$initial<- c(iniciais, rep(0,28)) # Iniciais para as correlações
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
modelo_mult$d<-length(modelo_mult$mod$m0)/56+1
modelo_mult$T<-length(data[,1])

# Estatísticas de interesse:

modelo_mult$ts.original_1<- desoc_bh
modelo_mult$ts.trend_1 <- modelo_mult[["m"]][,1]
modelo_mult$ts.slope_1 <- modelo_mult[["m"]][,9]
modelo_mult$ts.seasonal_1 <- modelo_mult[["m"]][,17]+modelo_mult[["m"]][,33]
modelo_mult$ts.signal_1 <- modelo_mult$ts.trend_1 +modelo_mult$ts.seasonal_1
modelo_mult$ts.sampling_e_1 <- modelo_mult[["m"]][,41]*se_bh
modelo_mult$ts.sampling_e_til_1 <- modelo_mult[["m"]][,41]
modelo_mult$ts.irregular_1 <- modelo_mult$ts.original_1-(modelo_mult$ts.signal_1+modelo_mult$ts.sampling_e_1)
modelo_mult$ts.seasonal_adj_1 <- modelo_mult$ts.trend_1+modelo_mult$ts.irregular_1

modelo_mult$ts.original_2<- desoc_ent
modelo_mult$ts.trend_2 <- modelo_mult[["m"]][,2]
modelo_mult$ts.slope_2 <- modelo_mult[["m"]][,10]
modelo_mult$ts.seasonal_2 <- modelo_mult[["m"]][,18]+modelo_mult[["m"]][,34]
modelo_mult$ts.signal_2 <- modelo_mult$ts.trend_2 +modelo_mult$ts.seasonal_2
modelo_mult$ts.sampling_e_2 <- modelo_mult[["m"]][,42]*se_ent
modelo_mult$ts.sampling_e_til_2 <- modelo_mult[["m"]][,42]
modelo_mult$ts.irregular_2 <- modelo_mult$ts.original_2-(modelo_mult$ts.signal_2+modelo_mult$ts.sampling_e_2)
modelo_mult$ts.seasonal_adj_2 <- modelo_mult$ts.trend_2+modelo_mult$ts.irregular_2

modelo_mult$ts.original_3<- desoc_sul
modelo_mult$ts.trend_3 <- modelo_mult[["m"]][,3]
modelo_mult$ts.slope_3 <- modelo_mult[["m"]][,11]
modelo_mult$ts.seasonal_3 <- modelo_mult[["m"]][,19]+modelo_mult[["m"]][,35]
modelo_mult$ts.signal_3 <- modelo_mult$ts.trend_3 +modelo_mult$ts.seasonal_3
modelo_mult$ts.sampling_e_3 <- modelo_mult[["m"]][,43]*se_sul
modelo_mult$ts.sampling_e_til_3 <- modelo_mult[["m"]][,43]
modelo_mult$ts.irregular_3 <- modelo_mult$ts.original_3-(modelo_mult$ts.signal_3+modelo_mult$ts.sampling_e_3)
modelo_mult$ts.seasonal_adj_3 <- modelo_mult$ts.trend_3+modelo_mult$ts.irregular_3

modelo_mult$ts.original_4<- desoc_trg
modelo_mult$ts.trend_4 <- modelo_mult[["m"]][,4]
modelo_mult$ts.slope_4 <- modelo_mult[["m"]][,12]
modelo_mult$ts.seasonal_4 <- modelo_mult[["m"]][,20]+modelo_mult[["m"]][,36]
modelo_mult$ts.signal_4 <- modelo_mult$ts.trend_4 +modelo_mult$ts.seasonal_4
modelo_mult$ts.sampling_e_4 <- modelo_mult[["m"]][,44]*se_trg
modelo_mult$ts.sampling_e_til_4 <- modelo_mult[["m"]][,44]
modelo_mult$ts.irregular_4 <- modelo_mult$ts.original_4-(modelo_mult$ts.signal_4+modelo_mult$ts.sampling_e_4)
modelo_mult$ts.seasonal_adj_4 <- modelo_mult$ts.trend_4+modelo_mult$ts.irregular_4

modelo_mult$ts.original_5<- desoc_mat
modelo_mult$ts.trend_5 <- modelo_mult[["m"]][,5]
modelo_mult$ts.slope_5 <- modelo_mult[["m"]][,13]
modelo_mult$ts.seasonal_5 <- modelo_mult[["m"]][,21]+modelo_mult[["m"]][,37]
modelo_mult$ts.signal_5 <- modelo_mult$ts.trend_5 +modelo_mult$ts.seasonal_5
modelo_mult$ts.sampling_e_5 <- modelo_mult[["m"]][,45]*se_mat
modelo_mult$ts.sampling_e_til_5 <- modelo_mult[["m"]][,45]
modelo_mult$ts.irregular_5 <- modelo_mult$ts.original_5-(modelo_mult$ts.signal_5+modelo_mult$ts.sampling_e_5)
modelo_mult$ts.seasonal_adj_5 <- modelo_mult$ts.trend_5+modelo_mult$ts.irregular_5

modelo_mult$ts.original_6<- desoc_nrt
modelo_mult$ts.trend_6 <- modelo_mult[["m"]][,6]
modelo_mult$ts.slope_6 <- modelo_mult[["m"]][,14]
modelo_mult$ts.seasonal_6 <- modelo_mult[["m"]][,22]+modelo_mult[["m"]][,38]
modelo_mult$ts.signal_6 <- modelo_mult$ts.trend_6 +modelo_mult$ts.seasonal_6
modelo_mult$ts.sampling_e_6 <- modelo_mult[["m"]][,46]*se_nrt
modelo_mult$ts.sampling_e_til_6 <- modelo_mult[["m"]][,46]
modelo_mult$ts.irregular_6 <- modelo_mult$ts.original_6-(modelo_mult$ts.signal_6+modelo_mult$ts.sampling_e_6)
modelo_mult$ts.seasonal_adj_6 <- modelo_mult$ts.trend_6+modelo_mult$ts.irregular_6

modelo_mult$ts.original_7<- desoc_val
modelo_mult$ts.trend_7 <- modelo_mult[["m"]][,7]
modelo_mult$ts.slope_7 <- modelo_mult[["m"]][,15]
modelo_mult$ts.seasonal_7 <- modelo_mult[["m"]][,23]+modelo_mult[["m"]][,39]
modelo_mult$ts.signal_7 <- modelo_mult$ts.trend_7 +modelo_mult$ts.seasonal_7
modelo_mult$ts.sampling_e_7 <- modelo_mult[["m"]][,47]*se_val
modelo_mult$ts.sampling_e_til_7 <- modelo_mult[["m"]][,47]
modelo_mult$ts.irregular_7 <- modelo_mult$ts.original_7-(modelo_mult$ts.signal_7+modelo_mult$ts.sampling_e_7)
modelo_mult$ts.seasonal_adj_7 <- modelo_mult$ts.trend_7+modelo_mult$ts.irregular_7

modelo_mult$ts.original_8<- desoc_cen
modelo_mult$ts.trend_8 <- modelo_mult[["m"]][,8]
modelo_mult$ts.slope_8 <- modelo_mult[["m"]][,16]
modelo_mult$ts.seasonal_8 <- modelo_mult[["m"]][,24]+modelo_mult[["m"]][,40]
modelo_mult$ts.signal_8 <- modelo_mult$ts.trend_8 +modelo_mult$ts.seasonal_8
modelo_mult$ts.sampling_e_8 <- modelo_mult[["m"]][,48]*se_cen
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


modelo_mult$se.original_1<- se_bh
modelo_mult$se.trend_1 <- se.mat[,1]
modelo_mult$se.slope_1 <- se.mat[,9] # Contei errado o erro padrão do slope dentro da matriz? 
                                        # Por que no original foram contadas 20 pos. além do sinal?
                                          # [,21] no script original corresponde exatamente ao início do componente sazonal
                                            # Olhar linha 438 do scrip de ref.
modelo_mult$se.sampling_e_til_1 <- se.mat[,41]
modelo_mult$se.seasonal_1 <- se.mat1_seasonal
modelo_mult$se.signal_1 <- se.mat1_sinal
modelo_mult$cv.original_1<- cv_bh*100
modelo_mult$cv.trend_1<- modelo_mult$se.trend_1/modelo_mult$ts.trend_1*100
modelo_mult$cv.slope_1<- modelo_mult$se.slope_1/modelo_mult$ts.slope_1*100
modelo_mult$cv.sampling_e_til_1<- modelo_mult$se.sampling_e_til_1/modelo_mult$ts.sampling_e_til_1*100
modelo_mult$cv.seasonal_1<- modelo_mult$se.seasonal_1/modelo_mult$ts.seasonal_1*100
modelo_mult$cv.signal_1<- modelo_mult$se.signal_1/modelo_mult$ts.signal_1*100

modelo_mult$se.original_2<- se_ent
modelo_mult$se.trend_2 <- se.mat[,2]
modelo_mult$se.slope_2 <- se.mat[,10]
modelo_mult$se.sampling_e_til_2 <- se.mat[,42]
modelo_mult$se.seasonal_2 <- se.mat2_seasonal
modelo_mult$se.signal_2 <- se.mat2_sinal
modelo_mult$cv.original_2<- cv_ent*100
modelo_mult$cv.trend_2<- modelo_mult$se.trend_2/modelo_mult$ts.trend_2*100
modelo_mult$cv.slope_2<- modelo_mult$se.slope_2/modelo_mult$ts.slope_2*100
modelo_mult$cv.sampling_e_til_2<- modelo_mult$se.sampling_e_til_2/modelo_mult$ts.sampling_e_til_2*100
modelo_mult$cv.seasonal_2<- modelo_mult$se.seasonal_2/modelo_mult$ts.seasonal_2*100
modelo_mult$cv.signal_2<- modelo_mult$se.signal_2/modelo_mult$ts.signal_2*100

modelo_mult$se.original_3<- se_sul
modelo_mult$se.trend_3 <- se.mat[,3]
modelo_mult$se.slope_3 <- se.mat[,11]
modelo_mult$se.sampling_e_til_3 <- se.mat[,43]
modelo_mult$se.seasonal_3 <- se.mat3_seasonal
modelo_mult$se.signal_3 <- se.mat3_sinal
modelo_mult$cv.original_3<- cv_sul*100
modelo_mult$cv.trend_3<- modelo_mult$se.trend_3/modelo_mult$ts.trend_3*100
modelo_mult$cv.slope_3<- modelo_mult$se.slope_3/modelo_mult$ts.slope_3*100
modelo_mult$cv.sampling_e_til_3<- modelo_mult$se.sampling_e_til_3/modelo_mult$ts.sampling_e_til_3*100
modelo_mult$cv.seasonal_3<- modelo_mult$se.seasonal_3/modelo_mult$ts.seasonal_3*100
modelo_mult$cv.signal_3<- modelo_mult$se.signal_3/modelo_mult$ts.signal_3*100

modelo_mult$se.original_4<- se_trg
modelo_mult$se.trend_4 <- se.mat[,4]
modelo_mult$se.slope_4 <- se.mat[,12]
modelo_mult$se.sampling_e_til_4 <- se.mat[,44]
modelo_mult$se.seasonal_4 <- se.mat4_seasonal
modelo_mult$se.signal_4 <- se.mat4_sinal
modelo_mult$cv.original_4<- cv_trg*100
modelo_mult$cv.trend_4<- modelo_mult$se.trend_4/modelo_mult$ts.trend_4*100
modelo_mult$cv.slope_4<- modelo_mult$se.slope_4/modelo_mult$ts.slope_4*100
modelo_mult$cv.sampling_e_til_4<- modelo_mult$se.sampling_e_til_4/modelo_mult$ts.sampling_e_til_4*100
modelo_mult$cv.seasonal_4<- modelo_mult$se.seasonal_4/modelo_mult$ts.seasonal_4*100
modelo_mult$cv.signal_4<- modelo_mult$se.signal_4/modelo_mult$ts.signal_4*100

modelo_mult$se.original_5<- se_mat
modelo_mult$se.trend_5 <- se.mat[,5]
modelo_mult$se.slope_5 <- se.mat[,13]
modelo_mult$se.sampling_e_til_5 <- se.mat[,45]
modelo_mult$se.seasonal_5 <- se.mat5_seasonal
modelo_mult$se.signal_5 <- se.mat5_sinal
modelo_mult$cv.original_5<- cv_mat*100
modelo_mult$cv.trend_5<- modelo_mult$se.trend_5/modelo_mult$ts.trend_5*100
modelo_mult$cv.slope_5<- modelo_mult$se.slope_5/modelo_mult$ts.slope_5*100
modelo_mult$cv.sampling_e_til_5<- modelo_mult$se.sampling_e_til_5/modelo_mult$ts.sampling_e_til_5*100
modelo_mult$cv.seasonal_5<- modelo_mult$se.seasonal_5/modelo_mult$ts.seasonal_5*100
modelo_mult$cv.signal_5<- modelo_mult$se.signal_5/modelo_mult$ts.signal_5*100

modelo_mult$se.original_6<- se_nrt
modelo_mult$se.trend_6 <- se.mat[,6]
modelo_mult$se.slope_6 <- se.mat[,14]
modelo_mult$se.sampling_e_til_6 <- se.mat[,46]
modelo_mult$se.seasonal_6 <- se.mat6_seasonal
modelo_mult$se.signal_6 <- se.mat6_sinal
modelo_mult$cv.original_6<- cv_nrt*100
modelo_mult$cv.trend_6<- modelo_mult$se.trend_6/modelo_mult$ts.trend_6*100
modelo_mult$cv.slope_6<- modelo_mult$se.slope_6/modelo_mult$ts.slope_6*100
modelo_mult$cv.sampling_e_til_6<- modelo_mult$se.sampling_e_til_6/modelo_mult$ts.sampling_e_til_6*100
modelo_mult$cv.seasonal_6<- modelo_mult$se.seasonal_6/modelo_mult$ts.seasonal_6*100
modelo_mult$cv.signal_6<- modelo_mult$se.signal_6/modelo_mult$ts.signal_6*100

modelo_mult$se.original_7<- se_val
modelo_mult$se.trend_7 <- se.mat[,7]
modelo_mult$se.slope_7 <- se.mat[,15]
modelo_mult$se.sampling_e_til_7 <- se.mat[,47]
modelo_mult$se.seasonal_7 <- se.mat7_seasonal
modelo_mult$se.signal_7 <- se.mat7_sinal
modelo_mult$cv.original_7<- cv_val*100
modelo_mult$cv.trend_7<- modelo_mult$se.trend_7/modelo_mult$ts.trend_7*100
modelo_mult$cv.slope_7<- modelo_mult$se.slope_7/modelo_mult$ts.slope_7*100
modelo_mult$cv.sampling_e_til_7<- modelo_mult$se.sampling_e_til_7/modelo_mult$ts.sampling_e_til_7*100
modelo_mult$cv.seasonal_7<- modelo_mult$se.seasonal_7/modelo_mult$ts.seasonal_7*100
modelo_mult$cv.signal_7<- modelo_mult$se.signal_7/modelo_mult$ts.signal_7*100

modelo_mult$se.original_8<- se_cen
modelo_mult$se.trend_8 <- se.mat[,8]
modelo_mult$se.slope_8 <- se.mat[,16]
modelo_mult$se.sampling_e_til_8 <- se.mat[,48]
modelo_mult$se.seasonal_8 <- se.mat8_seasonal
modelo_mult$se.signal_8 <- se.mat8_sinal
modelo_mult$cv.original_8<- cv_cen*100
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
  ts(desoc_val,start = 2012,frequency=4),
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


save.image(file = "D:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/iniciais/01_mod_comcorr_SULARMA11.Rdata")

load("D:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/01_mod_comcorr_SULARMA11.Rdata")










