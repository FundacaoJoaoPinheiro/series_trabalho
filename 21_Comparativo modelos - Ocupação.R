################################################################################
##          COMPARATIVO MODELOS SMOOTH E ESTRUTURAL - OCUPADOS                ##
################################################################################

## Para visualizar os gráficos em segunda tela:

dev.new()
dev.new()


### 01 - BELO HORIZONTE ########################################################
rm(list = ls())

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
bh<-baseestr8reg$`01-Belo Horizonte`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtbh<-baseal8reg$`01-Belo Horizonte` 
dbbh<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/01_params_bh.RDS")

ocup_bh <- bh$Total.de.ocupados/1000
se_db<- bh$sd_o/1000
cv_bh <- se_db/ocup_bh
ICinf_bh<-ocup_bh-1.96*se_db
ICsup_bh<-ocup_bh+1.96*se_db

sm_ar1_bh<-mods$`01-Belo Horizonte`$sinal_smooth_ar1bh
cv_sm_ar1_bh<-mods$`01-Belo Horizonte`$cv_sinal_smooth_ar1bh
est_ar1_bh<-mods$`01-Belo Horizonte`$sinal_estrutural_ar1bh
cv_est_ar1_bh<-mods$`01-Belo Horizonte`$cv_sinal_estrutural_ar1bh

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_bh <- window(ts.union(
  ts(ocup_bh, start = 2012, frequency = 4),
  ts(sm_ar1_bh, start = 2012, frequency = 4),
  ts(est_ar1_bh, start = 2012, frequency = 4),
  ts(ICinf_bh,start = 2012, frequency = 4),
  ts(ICsup_bh,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_bh, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("topleft", legend = c("Ocupação: design-based",
                  "Sinal da Ocupação - Smooth AR(1)",
                  "Sinal da Ocupação - Estrutural AR(1)"),lty = c(1, 1, 1),col = c(1, 4, 2),bty = 'n',lwd = c(2, 2, 2))
mtext("Total de ocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_bh.cv <- window(ts.union(
  ts((cv_bh*100), start = 2012, frequency = 4),
  ts(cv_sm_ar1_bh, start = 2012, frequency = 4),
  ts(cv_est_ar1_bh, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_bh.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV ocupados: design-based",
                             "Sinal CV ocupados - Smooth AR(1)",
                             "Sinal CV ocupados - Estrutural AR(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 02 - COLAR E ENTORNO METROPOLITANO DE BELO HORIZONTE #######################
rm(list = ls())

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
ent<-baseestr8reg$`02-Colar e Entorno metropolitano de BH`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtent<-baseal8reg$`02-Colar e Entorno Metropolitano de BH`
dbent<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/02_params_ent.RDS")

ocup_ent <- (ent$Total.de.ocupados)/1000
se_db <- (ent$sd_o)/1000
cv_ent <- se_db/ocup_ent
ICinf_ent<-ocup_ent-1.96*se_db
ICsup_ent<-ocup_ent+1.96*se_db

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")

sm_ar1_ent<-mods$`02-Colar e Entorno Metropolitano de BH`$sinal_smooth_ar1ent
cv_sm_ar1_ent<-mods$`02-Colar e Entorno Metropolitano de BH`$cv_sinal_smooth_ar1ent
est_ar1_ent<-mods$`02-Colar e Entorno Metropolitano de BH`$sinal_estrutural_ar1ent
cv_est_ar1_ent<-mods$`02-Colar e Entorno Metropolitano de BH`$cv_sinal_estrutural_ar1ent

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_ent <- window(ts.union(
  ts(ocup_ent, start = 2012, frequency = 4),
  ts(sm_ar1_ent, start = 2012, frequency = 4),
  ts(est_ar1_ent, start = 2012, frequency = 4),
  ts(ICinf_ent,start = 2012, frequency = 4),
  ts(ICsup_ent,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_ent, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("topleft", legend = c("Ocupação: design-based",
                            "Sinal da Ocupação - Smooth AR(1)",
                            "Sinal da Ocupação - Estrutural AR(1)"),lty = c(1, 1, 1),col = c(1, 4, 2),bty = 'n',lwd = c(2, 2, 2))
mtext("Total de ocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ent.cv <- window(ts.union(
  ts((cv_ent) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ar1_ent, start = 2012, frequency = 4),
  ts(cv_est_ar1_ent, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_ent.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV ocupados: design-based",
                             "Sinal CV ocupados - Smooth AR(1)",
                             "Sinal CV ocupados - Estrutural AR(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 03 - SUL DE MINAS ##########################################################

rm(list = ls())

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")
  
baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
sul<-baseestr8reg$`03-Sul de Minas`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtsul<-baseal8reg$`03-Sul de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbsul<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/03_params_sul.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

ocup_sul <- sul$Total.de.ocupados/1000
se_db <- sul$sd_o/1000
cv_sul <- se_db/ocup_sul
ICinf_sul<-ocup_sul-1.96*se_db
ICsup_sul<-ocup_sul+1.96*se_db

sm_ar1_sul<-mods$`03-Sul de Minas`$sinal_smooth_ar1sul
cv_sm_ar1_sul<-mods$`03-Sul de Minas`$cv_sinal_smooth_ar1sul
est_ar1_sul<-mods$`03-Sul de Minas`$sinal_estrutural_ar1sul
cv_est_ar1_sul<-mods$`03-Sul de Minas`$cv_sinal_estrutural_ar1sul

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_sul <- window(ts.union(
  ts(ocup_sul, start = 2012, frequency = 4),
  ts(sm_ar1_sul, start = 2012, frequency = 4),
  ts(est_ar1_sul, start = 2012, frequency = 4),
  ts(ICinf_sul,start = 2012, frequency = 4),
  ts(ICsup_sul,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_sul, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("topleft", legend = c("Ocupação: design-based",
                            "Sinal da Ocupação - Smooth AR(1)",
                            "Sinal da Ocupação - Estrutural AR(1)"),lty = c(1, 1, 1),col = c(1, 4, 2),bty = 'n',lwd = c(2, 2, 2))
mtext("Total de ocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_sul.cv <- window(ts.union(
  ts((cv_sul) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ar1_sul, start = 2012, frequency = 4),
  ts(cv_est_ar1_sul, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_sul.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV ocupados: design-based",
                             "Sinal CV ocupados - Smooth AR(1)",
                             "Sinal CV ocupados - Estrutural AR(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 04 - TRIÂNGULO MINEIRO #####################################################
rm(list = ls())

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
trg<-baseestr8reg$`04-Triângulo Mineiro`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dttrg<-baseal8reg$`04-Triângulo Mineiro` 
dbtrg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/04_params_trg.RDS") 

ocup_trg <- trg$Total.de.ocupados/1000
se_db <- trg$sd_o/1000
cv_trg <- se_db/ocup_trg
ICinf_trg<-ocup_trg-1.96*se_db
ICsup_trg<-ocup_trg+1.96*se_db

sm_ar1_trg<-mods$`04-Triângulo Mineiro`$sinal_smooth_ar1trg
cv_sm_ar1_trg<-mods$`04-Triângulo Mineiro`$cv_sinal_smooth_ar1trg
est_ar1_trg<-mods$`04-Triângulo Mineiro`$sinal_estrutural_ar1trg
cv_est_ar1_trg<-mods$`04-Triângulo Mineiro`$cv_sinal_estrutural_ar1trg

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_trg <- window(ts.union(
  ts(ocup_trg, start = 2012, frequency = 4),
  ts(sm_ar1_trg, start = 2012, frequency = 4),
  ts(est_ar1_trg, start = 2012, frequency = 4),
  ts(ICinf_trg,start = 2012, frequency = 4),
  ts(ICsup_trg,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_trg, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("topleft", legend = c("Ocupação: design-based",
                            "Sinal da Ocupação - Smooth AR(1)",
                            "Sinal da Ocupação - Estrutural AR(1)"),lty = c(1, 1, 1),col = c(1, 4, 2),bty = 'n',lwd = c(2, 2, 2))
mtext("Total de ocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_trg.cv <- window(ts.union(
  ts((cv_trg) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ar1_trg, start = 2012, frequency = 4),
  ts(cv_est_ar1_trg, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_trg.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV ocupados: design-based",
                             "Sinal CV ocupados - Smooth AR(1)",
                             "Sinal CV ocupados - Estrutural AR(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("04 - Triângulo Mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 05 - ZONA DA MATA ##########################################################
rm(list = ls())

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
mat<-baseestr8reg$`05-Mata de Minas Gerais`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtmat<-baseal8reg$`05-Mata de Minas Gerais`
dbmat<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/05_params_mat.RDS")

ocup_mat<- mat$Total.de.ocupados/1000
se_db <- mat$sd_o/1000
cv_mat <- se_db/ocup_mat
ICinf_mat<-ocup_mat-1.96*se_db
ICsup_mat<-ocup_mat+1.96*se_db

sm_ar1_mat<-mods$`05-Zona da Mata`$sinal_smooth_ar1mat
cv_sm_ar1_mat<-mods$`05-Zona da Mata`$cv_sinal_smooth_ar1mat
est_ar1_mat<-mods$`05-Zona da Mata`$sinal_estrutural_ar1mat
cv_est_ar1_mat<-mods$`05-Zona da Mata`$cv_sinal_estrutural_ar1mat

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_mat <- window(ts.union(
  ts(ocup_mat, start = 2012, frequency = 4),
  ts(sm_ar1_mat, start = 2012, frequency = 4),
  ts(est_ar1_mat, start = 2012, frequency = 4),
  ts(ICinf_mat,start = 2012, frequency = 4),
  ts(ICsup_mat,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_mat, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("topright", legend = c("Ocupação: design-based",
                            "Sinal da Ocupação - Smooth AR(1)",
                            "Sinal da Ocupação - Estrutural AR(1)"),lty = c(1, 1, 1),col = c(1, 4, 2),bty = 'n',lwd = c(2, 2, 2))
mtext("Total de ocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_mat.cv <- window(ts.union(
  ts((cv_mat) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ar1_mat, start = 2012, frequency = 4),
  ts(cv_est_ar1_mat, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_mat.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topright", legend = c("CV ocupados: design-based",
                             "Sinal CV ocupados - Smooth AR(1)",
                             "Sinal CV ocupados - Estrutural AR(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05 - Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### 06 - NORTE DE MINAS ########################################################
rm(list = ls())

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
nrt<-baseestr8reg$`06-Norte de Minas`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtnrt<-baseal8reg$`06-Norte de Minas`
dbnrt<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/06_params_nrt.RDS") 

ocup_nrt <- nrt$Total.de.ocupados/1000
se_db <- nrt$sd_o/1000
cv_nrt <- se_db/ocup_nrt
ICinf_nrt<-ocup_nrt-1.96*se_db
ICsup_nrt<-ocup_nrt+1.96*se_db

sm_ar1_nrt<-mods$`06-Norte de Minas`$sinal_smooth_ar1nrt
cv_sm_ar1_nrt<-mods$`06-Norte de Minas`$cv_sinal_smooth_ar1nrt
est_ar1_nrt<-mods$`06-Norte de Minas`$sinal_estrutural_ar1nrt
cv_est_ar1_nrt<-mods$`06-Norte de Minas`$cv_sinal_estrutural_ar1nrt

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_nrt <- window(ts.union(
  ts(ocup_nrt, start = 2012, frequency = 4),
  ts(sm_ar1_nrt, start = 2012, frequency = 4),
  ts(est_ar1_nrt, start = 2012, frequency = 4),
  ts(ICinf_nrt,start = 2012, frequency = 4),
  ts(ICsup_nrt,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_nrt, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("topleft", legend = c("Ocupação: design-based",
                            "Sinal da Ocupação - Smooth AR(1)",
                            "Sinal da Ocupação - Estrutural AR(1)"),lty = c(1, 1, 1),col = c(1, 4, 2),bty = 'n',lwd = c(2, 2, 2))
mtext("Total de ocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_nrt.cv <- window(ts.union(
  ts((cv_nrt) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ar1_nrt, start = 2012, frequency = 4),
  ts(cv_est_ar1_nrt, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_nrt.cv, plot.type = "single", col = c(1, 4, 2),lty = c(1, 1, 1),lwd = c(2, 2, 2),ylab = "", xlab = "")
legend("topleft", legend = c("CV ocupados: design-based",
                             "Sinal CV ocupados - Smooth AR(1)",
                             "Sinal CV ocupados - Estrutural AR(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### 07 - VALE DO RIO DOCE ######################################################
rm(list = ls())

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
vl<-baseestr8reg$`07-Vale do Rio Doce`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtvl<-baseal8reg$`07-Vale do Rio Doce`
dbvl<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/07_params_rio.RDS") 

ocup_val <- vl$Total.de.ocupados/1000
se_db <- vl$sd_o/1000
cv_val <- se_db/ocup_val
ICinf_val<-ocup_val-1.96*se_db
ICsup_val<-ocup_val+1.96*se_db

sm_ar1_val<-mods$`07-Vale do Rio Doce`$sinal_smooth_ar1val
cv_sm_ar1_val<-mods$`07-Vale do Rio Doce`$cv_sinal_smooth_ar1val
est_ar1_val<-mods$`07-Vale do Rio Doce`$sinal_estrutural_ar1val
cv_est_ar1_val<-mods$`07-Vale do Rio Doce`$cv_sinal_estrutural_ar1val

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(ocup_val, start = 2012, frequency = 4),
  ts(sm_ar1_val, start = 2012, frequency = 4),
  ts(est_ar1_val, start = 2012, frequency = 4),
  ts(ICinf_val,start = 2012, frequency = 4),
  ts(ICsup_val,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_ar1, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("topleft", legend = c("Ocupação: design-based",
                            "Sinal da Ocupação - Smooth AR(1)",
                            "Sinal da Ocupação - Estrutural AR(1)"),lty = c(1, 1, 1),col = c(1, 4, 2),bty = 'n',lwd = c(2, 2, 2))
mtext("Total de ocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((cv_val) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ar1_val, start = 2012, frequency = 4),
  ts(cv_est_ar1_val, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4,2), ylab = "", xlab = "", lty = c(1, 1,1), lwd = c(2))
legend("topleft", legend = c("CV ocupados: design-based",
                             "Sinal CV ocupados - Smooth AR(1)",
                             "Sinal CV ocupados - Estrutural AR(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07 - Vale do Rio Doce", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 08 - CENTRAL ###############################################################
rm(list = ls())

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
cen<-baseestr8reg$`08-Central`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtcen<-baseal8reg$`08-Central`
dbcen<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/08_params_cen.RDS") 

ocup_cen <- cen$Total.de.ocupados/1000
se_db <- cen$sd_o/1000
cv_cen <- se_db/ocup_cen
ICinf_cen<-ocup_cen-1.96*se_db
ICsup_cen<-ocup_cen+1.96*se_db

sm_ar1_cen<-mods$`08-Central`$sinal_smooth_ar1cen
cv_sm_ar1_cen<-mods$`08-Central`$cv_sinal_smooth_ar1cen
est_ar1_cen<-mods$`08-Central`$sinal_estrutural_ar1cen
cv_est_ar1_cen<-mods$`08-Central`$cv_sinal_estrutural_ar1cen

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_cen <- window(ts.union(
  ts(ocup_cen, start = 2012, frequency = 4),
  ts(sm_ar1_cen, start = 2012, frequency = 4),
  ts(est_ar1_cen, start = 2012, frequency = 4),
  ts(ICinf_cen,start = 2012, frequency = 4),
  ts(ICsup_cen,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_cen, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("topleft", legend = c("Ocupação: design-based",
                            "Sinal da Ocupação - Smooth AR(1)",
                            "Sinal da Ocupação - Estrutural AR(1)"),lty = c(1, 1, 1),col = c(1, 4, 2),bty = 'n',lwd = c(2, 2, 2))
mtext("Total de ocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_cen.cv <- window(ts.union(
  ts((cv_cen) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ar1_cen, start = 2012, frequency = 4),
  ts(cv_est_ar1_cen, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_cen.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV ocupados: design-based",
                             "Sinal CV ocupados - Smooth AR(1)",
                             "Sinal CV ocupados - Estrutural AR(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Central", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### 09 - MINAS GERAIS ##########################################################
rm(list = ls())

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_ocup.rds")

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
mg<-baseestr8reg$`09 - Minas Gerais`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtmg<-baseal8reg$`09 - Minas Gerais` 
dbmg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/09_params_mg.RDS")

ocup_mg <- mg$Total.de.ocupados/1000
se_db <- mg$sd_o/1000
cv_mg <- se_db/ocup_mg
ICinf_mg<-ocup_mg-1.96*se_db
ICsup_mg<-ocup_mg+1.96*se_db

sm_ar1_mg<-mods$`09 - Minas Gerais`$sinal_smooth_ar1mg
cv_sm_ar1_mg<-mods$`09 - Minas Gerais`$cv_sinal_smooth_ar1mg
est_ar1_mg<-mods$`09 - Minas Gerais`$sinal_estrutural_ar1mg
cv_est_ar1_mg<-mods$`09 - Minas Gerais`$cv_sinal_estrutural_ar1mg

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_mg <- window(ts.union(
  ts(ocup_mg, start = 2012, frequency = 4),
  ts(sm_ar1_mg, start = 2012, frequency = 4),
  ts(est_ar1_mg, start = 2012, frequency = 4),
  ts(ICinf_mg,start = 2012, frequency = 4),
  ts(ICsup_mg,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_mg, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("topleft", legend = c("Ocupação: design-based",
                            "Sinal da Ocupação - Smooth AR(1)",
                            "Sinal da Ocupação - Estrutural AR(1)"),lty = c(1, 1, 1),col = c(1, 4, 2),bty = 'n',lwd = c(2, 2, 2))
mtext("Total de ocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_mg.cv <- window(ts.union(
  ts((cv_mg) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ar1_mg, start = 2012, frequency = 4),
  ts(cv_est_ar1_mg, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_mg.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV ocupados: design-based",
                             "Sinal CV ocupados - Smooth AR(1)",
                             "Sinal CV ocupados - Estrutural AR(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("09 - Minas Gerais", side = 3, outer = TRUE, line = 0.5)


