################################################################################
##                  MODELO MULTIVARIADO COM CORRELAÇÃO                        ##
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

mg<-baseestr8reg$`09 - Minas Gerais`
dbmg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/09_params_mg.RDS")
desoc_mg<-mg$Total.de.desocupados/1000
se_mg<-mg$sd_d/1000
cv_mg<-se_mg/desoc_mg

theta1_ma1_mg <- dbmg[["mod_ma1"]][["theta1_ma1_dmg"]]
load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/09_mod_mg.Rdata", envir = env9)
initial_mg <- env9$ma1_mg[["initial"]]
estimated_mg <- env9$ma1_mg[["fit"]][["par"]]

keep(desoc_bh,se_bh,cv_bh,theta1_ma1_bh,initial_bh,estimated_bh,
     desoc_ent,se_ent,cv_ent,theta1_ma1_ent,initial_ent,estimated_ent,
     desoc_sul,se_sul,cv_sul,theta1_ma1_sul,initial_sul,estimated_sul,
     desoc_trg,se_trg,cv_trg,theta1_ma1_trg,initial_trg,estimated_trg,
     desoc_mat,se_mat,cv_mat,theta1_ma1_mat,initial_mat,estimated_mat,
     desoc_nrt,se_nrt,cv_nrt,theta1_ma1_nrt,initial_nrt,estimated_nrt,
     desoc_val,se_val,cv_val,phi1_ar1_val,initial_val,estimated_val,
     desoc_cen,se_cen,cv_cen,theta1_ma1_cen,initial_cen,estimated_cen,
     sure=TRUE)
gc()


#### TESTANDO MODELO MULTIVARIADO SEM CORRELAÇÃO E SEM BENCHMARKING #####################

modelo_mult_sem_corr<- list("fn"=function(params){
  m = dlmModPoly(2) + dlmModTrig(4) + dlmModReg(se_bh,addInt = FALSE) # Por que o erro de bh?
  
  FF <- m$FF %x% diag(8)
  m$FF <- FF
  
  JFF <- matrix(0,8,48)
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
  GG[41,41] <- theta1_ma1_bh
  GG[42,42] <- theta1_ma1_ent
  GG[43,43] <- theta1_ma1_sul
  GG[44,44] <- theta1_ma1_trg
  GG[45,45] <- theta1_ma1_mat
  GG[46,46] <- theta1_ma1_nrt
  GG[47,47] <- phi1_ar1_val
  GG[48,48] <- theta1_ma1_cen
  m$GG <- GG
  
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
  W[24,24] <- exp(params[24]) # Por que o salto? Não deveria continuar no 40 (pos. na matriz)?
  
  W[41,41] <- exp(params[33])
  W[42,42] <- exp(params[34])
  W[43,43] <- exp(params[35])
  W[44,44] <- exp(params[36])
  W[45,45] <- exp(params[37])
  W[46,46] <- exp(params[38])
  W[47,47] <- exp(params[39])
  W[48,48] <- exp(params[40])

  m$W <- W
  
  m0 <- m$m0 %x% diag(8)
  m$m0 <- m0
  
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
modelo_mult_sem_corr$initial<- c(estimados)



start_time <- Sys.time()
modelo_mult_sem_corr$fit <- dlmMLE(data, modelo_mult_sem_corr$initial,modelo_mult_sem_corr$fn, hessian=T,
                                   control = list(maxit = 10^8))
end_time <- Sys.time()
end_time - start_time
beep(3)


## Separação dos resultados:

modelo_mult_sem_corr$mod <- modelo_mult_sem_corr$fn(modelo_mult_sem_corr$fit$par)

# aplica o filtro de Kalman para obter séries filtradas e suavizadas
modelo_mult_sem_corr$filtered <- dlmFilter(data,modelo_mult_sem_corr$mod)
modelo_mult_sem_corr$smoothed <- dlmSmooth(modelo_mult_sem_corr$filtered)
modelo_mult_sem_corr$m <- dropFirst(modelo_mult_sem_corr$filtered$m)
modelo_mult_sem_corr$sm <- dropFirst(modelo_mult_sem_corr$smoothed$s)
modelo_mult_sem_corr$res <- residuals(modelo_mult_sem_corr$filtered,sd=FALSE)

# definição de variável
modelo_mult_sem_corr$d<-length(modelo_mult_sem_corr$mod$m0)/100+1 # Por que 100+1?
modelo_mult_sem_corr$T<-length(data[,1])

# estatísticas de interesse
modelo_mult_sem_corr$ts.original_1<- desoc_bh
modelo_mult_sem_corr$ts.trend_1 <- modelo_mult_sem_corr[["m"]][,1]
modelo_mult_sem_corr$ts.slope_1 <- modelo_mult_sem_corr[["m"]][,9]
modelo_mult_sem_corr$ts.seasonal_1 <- modelo_mult_sem_corr[["m"]][,21]+modelo_mult_sem_corr[["m"]][,41] # Por que esse "salto"?
modelo_mult_sem_corr$ts.signal_1 <- modelo_mult_sem_corr$ts.trend_1 +modelo_mult_sem_corr$ts.seasonal_1
modelo_mult_sem_corr$ts.sampling_e_1 <- modelo_mult_sem_corr[["m"]][,51]*se_db_1
modelo_mult_sem_corr$ts.sampling_e_til_1 <- modelo_mult_sem_corr[["m"]][,51]
modelo_mult_sem_corr$ts.irregular_1 <- modelo_mult_sem_corr$ts.original_1-(modelo_mult_sem_corr$ts.signal_1+modelo_mult_sem_corr$ts.sampling_error_1)
modelo_mult_sem_corr$ts.seasonal_adj_1 <- modelo_mult_sem_corr$ts.trend_1+modelo_mult_sem_corr$ts.irregular_1

modelo_mult_sem_corr$ts.original_2<- desoc_ent
modelo_mult_sem_corr$ts.trend_2 <- modelo_mult_sem_corr[["m"]][,2]
modelo_mult_sem_corr$ts.slope_2 <- modelo_mult_sem_corr[["m"]][,10]
modelo_mult_sem_corr$ts.seasonal_2 <- modelo_mult_sem_corr[["m"]][,22]+modelo_mult_sem_corr[["m"]][,42]
modelo_mult_sem_corr$ts.signal_2 <- modelo_mult_sem_corr$ts.trend_2 +modelo_mult_sem_corr$ts.seasonal_2
modelo_mult_sem_corr$ts.sampling_e_2 <- modelo_mult_sem_corr[["m"]][,52]*se_db_2
modelo_mult_sem_corr$ts.sampling_e_til_2 <- modelo_mult_sem_corr[["m"]][,52]
modelo_mult_sem_corr$ts.irregular_2 <- modelo_mult_sem_corr$ts.original_2-(modelo_mult_sem_corr$ts.signal_2+modelo_mult_sem_corr$ts.sampling_e_2)
modelo_mult_sem_corr$ts.seasonal_adj_2 <- modelo_mult_sem_corr$ts.trend_2+modelo_mult_sem_corr$ts.irregular_2

modelo_mult_sem_corr$ts.original_3<- desoc_sul
modelo_mult_sem_corr$ts.trend_3 <- modelo_mult_sem_corr[["m"]][,3]
modelo_mult_sem_corr$ts.slope_3 <- modelo_mult_sem_corr[["m"]][,11]
modelo_mult_sem_corr$ts.seasonal_3 <- modelo_mult_sem_corr[["m"]][,23]+modelo_mult_sem_corr[["m"]][,43]
modelo_mult_sem_corr$ts.signal_3 <- modelo_mult_sem_corr$ts.trend_3 +modelo_mult_sem_corr$ts.seasonal_3
modelo_mult_sem_corr$ts.sampling_e_3 <- modelo_mult_sem_corr[["m"]][,53]*se_db_3
modelo_mult_sem_corr$ts.sampling_e_til_3 <- modelo_mult_sem_corr[["m"]][,53]
modelo_mult_sem_corr$ts.irregular_3 <- modelo_mult_sem_corr$ts.original_3-(modelo_mult_sem_corr$ts.signal_3+modelo_mult_sem_corr$ts.sampling_e_3)
modelo_mult_sem_corr$ts.seasonal_adj_3 <- modelo_mult_sem_corr$ts.trend_3+modelo_mult_sem_corr$ts.irregular_3

modelo_mult_sem_corr$ts.original_4<- desoc_trg
modelo_mult_sem_corr$ts.trend_4 <- modelo_mult_sem_corr[["m"]][,4]
modelo_mult_sem_corr$ts.slope_4 <- modelo_mult_sem_corr[["m"]][,12]
modelo_mult_sem_corr$ts.seasonal_4 <- modelo_mult_sem_corr[["m"]][,24]+modelo_mult_sem_corr[["m"]][,44]
modelo_mult_sem_corr$ts.signal_4 <- modelo_mult_sem_corr$ts.trend_4 +modelo_mult_sem_corr$ts.seasonal_4
modelo_mult_sem_corr$ts.sampling_e_4 <- modelo_mult_sem_corr[["m"]][,54]*se_db_4
modelo_mult_sem_corr$ts.sampling_e_til_4 <- modelo_mult_sem_corr[["m"]][,54]
modelo_mult_sem_corr$ts.irregular_4 <- modelo_mult_sem_corr$ts.original_4-(modelo_mult_sem_corr$ts.signal_4+modelo_mult_sem_corr$ts.sampling_e_4)
modelo_mult_sem_corr$ts.seasonal_adj_4 <- modelo_mult_sem_corr$ts.trend_4+modelo_mult_sem_corr$ts.irregular_4

modelo_mult_sem_corr$ts.original_5<- desoc_mat
modelo_mult_sem_corr$ts.trend_5 <- modelo_mult_sem_corr[["m"]][,5]
modelo_mult_sem_corr$ts.slope_5 <- modelo_mult_sem_corr[["m"]][,13]
modelo_mult_sem_corr$ts.seasonal_5 <- modelo_mult_sem_corr[["m"]][,25]+modelo_mult_sem_corr[["m"]][,45]
modelo_mult_sem_corr$ts.signal_5 <- modelo_mult_sem_corr$ts.trend_5 +modelo_mult_sem_corr$ts.seasonal_5
modelo_mult_sem_corr$ts.sampling_e_5 <- modelo_mult_sem_corr[["m"]][,55]*se_db_5
modelo_mult_sem_corr$ts.sampling_e_til_5 <- modelo_mult_sem_corr[["m"]][,55]
modelo_mult_sem_corr$ts.irregular_5 <- modelo_mult_sem_corr$ts.original_5-(modelo_mult_sem_corr$ts.signal_5+modelo_mult_sem_corr$ts.sampling_e_5)
modelo_mult_sem_corr$ts.seasonal_adj_5 <- modelo_mult_sem_corr$ts.trend_5+modelo_mult_sem_corr$ts.irregular_5

modelo_mult_sem_corr$ts.original_6<- desoc_nrt
modelo_mult_sem_corr$ts.trend_6 <- modelo_mult_sem_corr[["m"]][,6]
modelo_mult_sem_corr$ts.slope_6 <- modelo_mult_sem_corr[["m"]][,14]
modelo_mult_sem_corr$ts.seasonal_6 <- modelo_mult_sem_corr[["m"]][,26]+modelo_mult_sem_corr[["m"]][,46]
modelo_mult_sem_corr$ts.signal_6 <- modelo_mult_sem_corr$ts.trend_6 +modelo_mult_sem_corr$ts.seasonal_6
modelo_mult_sem_corr$ts.sampling_e_6 <- modelo_mult_sem_corr[["m"]][,56]*se_db_6
modelo_mult_sem_corr$ts.sampling_e_til_6 <- modelo_mult_sem_corr[["m"]][,56]
modelo_mult_sem_corr$ts.irregular_6 <- modelo_mult_sem_corr$ts.original_6-(modelo_mult_sem_corr$ts.signal_6+modelo_mult_sem_corr$ts.sampling_e_6)
modelo_mult_sem_corr$ts.seasonal_adj_6 <- modelo_mult_sem_corr$ts.trend_6+modelo_mult_sem_corr$ts.irregular_6

modelo_mult_sem_corr$ts.original_7<- desoc_val
modelo_mult_sem_corr$ts.trend_7 <- modelo_mult_sem_corr[["m"]][,7]
modelo_mult_sem_corr$ts.slope_7 <- modelo_mult_sem_corr[["m"]][,15]
modelo_mult_sem_corr$ts.seasonal_7 <- modelo_mult_sem_corr[["m"]][,27]+modelo_mult_sem_corr[["m"]][,47]
modelo_mult_sem_corr$ts.signal_7 <- modelo_mult_sem_corr$ts.trend_7 +modelo_mult_sem_corr$ts.seasonal_7
modelo_mult_sem_corr$ts.sampling_e_7 <- modelo_mult_sem_corr[["m"]][,57]*se_db_7
modelo_mult_sem_corr$ts.sampling_e_til_7 <- modelo_mult_sem_corr[["m"]][,57]
modelo_mult_sem_corr$ts.irregular_7 <- modelo_mult_sem_corr$ts.original_7-(modelo_mult_sem_corr$ts.signal_7+modelo_mult_sem_corr$ts.sampling_e_7)
modelo_mult_sem_corr$ts.seasonal_adj_7 <- modelo_mult_sem_corr$ts.trend_7+modelo_mult_sem_corr$ts.irregular_7

modelo_mult_sem_corr$ts.original_8<- desoc_cen
modelo_mult_sem_corr$ts.trend_8 <- modelo_mult_sem_corr[["m"]][,8]
modelo_mult_sem_corr$ts.slope_8 <- modelo_mult_sem_corr[["m"]][,16] # Com 10 reg estava 18
modelo_mult_sem_corr$ts.seasonal_8 <- modelo_mult_sem_corr[["m"]][,28]+modelo_mult_sem_corr[["m"]][,48]
modelo_mult_sem_corr$ts.signal_8 <- modelo_mult_sem_corr$ts.trend_8 +modelo_mult_sem_corr$ts.seasonal_8
modelo_mult_sem_corr$ts.sampling_e_8 <- modelo_mult_sem_corr[["m"]][,58]*se_db_8
modelo_mult_sem_corr$ts.sampling_e_til_8 <- modelo_mult_sem_corr[["m"]][,58]
modelo_mult_sem_corr$ts.irregular_8 <- modelo_mult_sem_corr$ts.original_8-(modelo_mult_sem_corr$ts.signal_8+modelo_mult_sem_corr$ts.sampling_e_8)
modelo_mult_sem_corr$ts.seasonal_adj_8 <- modelo_mult_sem_corr$ts.trend_8+modelo_mult_sem_corr$ts.irregular_8






