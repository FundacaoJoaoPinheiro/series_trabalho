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








