################################################################################
##          SCRIPT CÁLCULO DA TAXA DE DESOCUPAÇÃO - OUT OF MODEL              ##
################################################################################

rm(list=ls())
gc()
options(scipen=999)


#### UPLOAD DOS DADOS

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")

# Desocupação

env1<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/estimados/01_mod_comcorr.Rdata", envir = env1)

# Ocupção

env2 <- new.env()

load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)


#### 01 - BELO HORIZONTE #######################################################

est_direta_bh <- baseestr8reg$`01-Belo Horizonte`$Taxa.de.desocupação
est_direta_bh <-  window(ts.union(ts(est_direta_bh, start = 2012, frequency = 4)), start = c(2013,4))
est_direta_bh <- est_direta_bh*100

sig_desoc_bh <- env1$modelo_mult$ts.signal_1
sig_ocup_bh <- env2$modelo_mult$ts.signal_1
var_desoc_bh <- (env1$modelo_mult$se.signal_1)^2
var_ocup_bh <- (env2$modelo_mult$se.signal_1)^2
S_bh <- sig_desoc_bh+sig_ocup_bh

txdesoc_bh <- sig_desoc_bh/ (sig_desoc_bh+sig_ocup_bh)
var_txdesoc_bh <- (sig_ocup_bh^2*var_ocup_bh+sig_desoc_bh^2*var_desoc_bh)/(S_bh^4)
se_txdesoc_bh <- sqrt(var_txdesoc_bh)
txdesoc_bh <- txdesoc_bh*100
se_txdesoc_bh <- se_txdesoc_bh*100

ICinf_bh<-txdesoc_bh-1.96*se_txdesoc_bh
ICsup_bh<-txdesoc_bh+1.96*se_txdesoc_bh

txdesoc_bh <- window(ts.union(ts(txdesoc_bh, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_bh <- window(ts.union(ts(ICinf_bh, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_bh <- window(ts.union(ts(ICsup_bh, start = 2012, frequency = 4)), start = c(2013,4))

plot(est_direta_bh, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa de desocupação (%)", ylim=c(0,20))
lines(txdesoc_bh, col = "red", lty = 1, lwd = 2)
lines(ICinf_bh, col = "red", lty = 2)
lines(ICsup_bh, col = "red", lty = 2)
legend("topleft", legend = c("Estimativa direta", "Taxa de desocupação: model-based", "IC 95%: model-based"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)
mtext("01 - Belo Horizonte (Mod. Multivariado)", side = 3, outer = FALSE, line = 0.5, font = 2, cex = 1.2)


#### 02 - COLAR E ENTORNO METROPOLITANO DE BELO HORIZONTE ######################

est_direta_ent <- baseestr8reg$`02-Colar e Entorno metropolitano de BH`$Taxa.de.desocupação
est_direta_ent <-  window(ts.union(ts(est_direta_ent, start = 2012, frequency = 4)), start = c(2013,4))
est_direta_ent <- est_direta_ent*100

sig_desoc_ent <- env1$modelo_mult$ts.signal_2 
sig_ocup_ent <- env2$modelo_mult$ts.signal_2
var_desoc_ent <- (env1$modelo_mult$se.signal_2)^2
var_ocup_ent <- (env2$modelo_mult$se.signal_2)^2
S_ent <- sig_desoc_ent+sig_ocup_ent

txdesoc_ent <- sig_desoc_ent/ (sig_desoc_ent+sig_ocup_ent)
var_txdesoc_ent <- (sig_ocup_ent^2*var_ocup_ent+sig_desoc_ent^2*var_desoc_ent)/(S_ent^4)
se_txdesoc_ent <- sqrt(var_txdesoc_ent)
txdesoc_ent <- txdesoc_ent*100
se_txdesoc_ent <- se_txdesoc_ent*100

ICinf_ent<-txdesoc_ent-1.96*se_txdesoc_ent
ICsup_ent<-txdesoc_ent+1.96*se_txdesoc_ent

txdesoc_ent <- window(ts.union(ts(txdesoc_ent, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_ent <- window(ts.union(ts(ICinf_ent, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_ent <- window(ts.union(ts(ICsup_ent, start = 2012, frequency = 4)), start = c(2013,4))

plot(est_direta_ent, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa de desocupação (%)", ylim=c(1,22))
lines(txdesoc_ent, col = "red", lty = 1, lwd = 2)
lines(ICinf_ent, col = "red", lty = 2)
lines(ICsup_ent, col = "red", lty = 2)
legend("topleft", legend = c("Estimativa direta", "Taxa de desocupação: model-based", "IC 95%: model-based"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte (Mod. Multivariado)", side = 3, outer = FALSE, line = 0.5, font = 2, cex = 1.2)


#### 03 - SUL DE MINAS #########################################################

est_direta_sul <- baseestr8reg$`03-Sul de Minas`$Taxa.de.desocupação
est_direta_sul <-  window(ts.union(ts(est_direta_sul, start = 2012, frequency = 4)), start = c(2013,4))
est_direta_sul <- est_direta_sul*100

sig_desoc_sul <- env1$modelo_mult$ts.signal_3
sig_ocup_sul <- env2$modelo_mult$ts.signal_3
var_desoc_sul <- (env1$modelo_mult$se.signal_3)^2
var_ocup_sul <- (env2$modelo_mult$se.signal_3)^2
S_sul <- sig_desoc_sul+sig_ocup_sul

txdesoc_sul <- sig_desoc_sul/ (sig_desoc_sul+sig_ocup_sul)
var_txdesoc_sul <- (sig_ocup_sul^2*var_ocup_sul+sig_desoc_sul^2*var_desoc_sul)/(S_sul^4)
se_txdesoc_sul <- sqrt(var_txdesoc_sul)
txdesoc_sul <- txdesoc_sul*100
se_txdesoc_sul <- se_txdesoc_sul*100

ICinf_sul<-txdesoc_sul-1.96*se_txdesoc_sul
ICsup_sul<-txdesoc_sul+1.96*se_txdesoc_sul

txdesoc_sul <- window(ts.union(ts(txdesoc_sul, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_sul <- window(ts.union(ts(ICinf_sul, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_sul <- window(ts.union(ts(ICsup_sul, start = 2012, frequency = 4)), start = c(2013,4))

plot(est_direta_sul, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa de desocupação (%)", ylim=c(-2,17))
lines(txdesoc_sul, col = "red", lty = 1, lwd = 2)
lines(ICinf_sul, col = "red", lty = 2)
lines(ICsup_sul, col = "red", lty = 2)
legend("topleft", legend = c("Estimativa direta", "Taxa de desocupação: model-based", "IC 95%: model-based"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)
mtext("03 - Sul de Minas (Mod. Multivariado)", side = 3, outer = FALSE, line = 0.5, font = 2, cex = 1.2)


#### 04 - TRIANGULO MINEIRO ####################################################

est_direta_trg <- baseestr8reg$`04-Triângulo Mineiro`$Taxa.de.desocupação
est_direta_trg <-  window(ts.union(ts(est_direta_trg, start = 2012, frequency = 4)), start = c(2013,4))
est_direta_trg <- est_direta_trg*100

sig_desoc_trg <- env1$modelo_mult$ts.signal_4
sig_ocup_trg <- env2$modelo_mult$ts.signal_4
var_desoc_trg <- (env1$modelo_mult$se.signal_4)^2
var_ocup_trg <- (env2$modelo_mult$se.signal_4)^2
S_trg <- sig_desoc_trg+sig_ocup_trg

txdesoc_trg <- sig_desoc_trg/ (sig_desoc_trg+sig_ocup_trg)
var_txdesoc_trg <- (sig_ocup_trg^2*var_ocup_trg+sig_desoc_trg^2*var_desoc_trg)/(S_trg^4)
se_txdesoc_trg <- sqrt(var_txdesoc_trg)
txdesoc_trg <- txdesoc_trg*100
se_txdesoc_trg <- se_txdesoc_trg*100

ICinf_trg<-txdesoc_trg-1.96*se_txdesoc_trg
ICsup_trg<-txdesoc_trg+1.96*se_txdesoc_trg

txdesoc_trg <- window(ts.union(ts(txdesoc_trg, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_trg <- window(ts.union(ts(ICinf_trg, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_trg <- window(ts.union(ts(ICsup_trg, start = 2012, frequency = 4)), start = c(2013,4))

plot(est_direta_trg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa de desocupação (%)", ylim=c(-8,21))
lines(txdesoc_trg, col = "red", lty = 1, lwd = 2)
lines(ICinf_trg, col = "red", lty = 2)
lines(ICsup_trg, col = "red", lty = 2)
legend("topleft", legend = c("Estimativa direta", "Taxa de desocupação: model-based", "IC 95%: model-based"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)
mtext("04 - Triângulo Mineiro (Mod. Multivariado)", side = 3, outer = FALSE, line = 0.5, font = 2, cex = 1.2)


#### 05 - ZONA DA MATA ####################################################

est_direta_mat <- baseestr8reg$`05-Mata de Minas Gerais`$Taxa.de.desocupação
est_direta_mat <-  window(ts.union(ts(est_direta_mat, start = 2012, frequency = 4)), start = c(2013,4))
est_direta_mat <- est_direta_mat*100

sig_desoc_mat <- env1$modelo_mult$ts.signal_5 
sig_ocup_mat <- env2$modelo_mult$ts.signal_5
var_desoc_mat <- (env1$modelo_mult$se.signal_5)^2
var_ocup_mat <- (env2$modelo_mult$se.signal_5)^2
S_mat <- sig_desoc_mat+sig_ocup_mat

txdesoc_mat <- sig_desoc_mat/ (sig_desoc_mat+sig_ocup_mat)
var_txdesoc_mat <- (sig_ocup_mat^2*var_ocup_mat+sig_desoc_mat^2*var_desoc_mat)/(S_mat^4)
se_txdesoc_mat <- sqrt(var_txdesoc_mat)
txdesoc_mat <- txdesoc_mat*100
se_txdesoc_mat <- se_txdesoc_mat*100

ICinf_mat<-txdesoc_mat-1.96*se_txdesoc_mat
ICsup_mat<-txdesoc_mat+1.96*se_txdesoc_mat

txdesoc_mat <- window(ts.union(ts(txdesoc_mat, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_mat <- window(ts.union(ts(ICinf_mat, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_mat <- window(ts.union(ts(ICsup_mat, start = 2012, frequency = 4)), start = c(2013,4))

plot(est_direta_mat, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa de desocupação (%)", ylim=c(-1,18))
lines(txdesoc_mat, col = "red", lty = 1, lwd = 2)
lines(ICinf_mat, col = "red", lty = 2)
lines(ICsup_mat, col = "red", lty = 2)
legend("topleft", legend = c("Estimativa direta", "Taxa de desocupação: model-based", "IC 95%: model-based"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)
mtext("05 - Zona da Mata (Mod. Multivariado)", side = 3, outer = FALSE, line = 0.5, font = 2, cex = 1.2)


#### 06 - NORTE DE MINAS #######################################################

est_direta_nrt <- baseestr8reg$`06-Norte de Minas`$Taxa.de.desocupação
est_direta_nrt <-  window(ts.union(ts(est_direta_nrt, start = 2012, frequency = 4)), start = c(2013,4))
est_direta_nrt <- est_direta_nrt*100

sig_desoc_nrt <- env1$modelo_mult$ts.signal_6 
sig_ocup_nrt <- env2$modelo_mult$ts.signal_6
var_desoc_nrt <- (env1$modelo_mult$se.signal_6)^2
var_ocup_nrt <- (env2$modelo_mult$se.signal_6)^2
S_nrt <- sig_desoc_nrt+sig_ocup_nrt

txdesoc_nrt <- sig_desoc_nrt/ (sig_desoc_nrt+sig_ocup_nrt)
var_txdesoc_nrt <- (sig_ocup_nrt^2*var_ocup_nrt+sig_desoc_nrt^2*var_desoc_nrt)/(S_nrt^4)
se_txdesoc_nrt <- sqrt(var_txdesoc_nrt)
txdesoc_nrt <- txdesoc_nrt*100
se_txdesoc_nrt <- se_txdesoc_nrt*100

ICinf_nrt<-txdesoc_nrt-1.96*se_txdesoc_nrt
ICsup_nrt<-txdesoc_nrt+1.96*se_txdesoc_nrt

txdesoc_nrt <- window(ts.union(ts(txdesoc_nrt, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_nrt <- window(ts.union(ts(ICinf_nrt, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_nrt <- window(ts.union(ts(ICsup_nrt, start = 2012, frequency = 4)), start = c(2013,4))

plot(est_direta_nrt, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa de desocupação (%)", ylim=c(-1,25))
lines(txdesoc_nrt, col = "red", lty = 1, lwd = 2)
lines(ICinf_nrt, col = "red", lty = 2)
lines(ICsup_nrt, col = "red", lty = 2)
legend("topleft", legend = c("Estimativa direta", "Taxa de desocupação: model-based", "IC 95%: model-based"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)
mtext("06 - Norte de Minas (Mod. Multivariado)", side = 3, outer = FALSE, line = 0.5, font = 2, cex = 1.2)


#### 07 - VALE DO RIO DOCE #####################################################

est_direta_val <- baseestr8reg$`07-Vale do Rio Doce`$Taxa.de.desocupação
est_direta_val <-  window(ts.union(ts(est_direta_val, start = 2012, frequency = 4)), start = c(2013,4))
est_direta_val <- est_direta_val*100

sig_desoc_val <- env1$modelo_mult$ts.signal_7 
sig_ocup_val <- env2$modelo_mult$ts.signal_7
var_desoc_val <- (env1$modelo_mult$se.signal_7)^2
var_ocup_val <- (env2$modelo_mult$se.signal_7)^2
S_val <- sig_desoc_val+sig_ocup_val

txdesoc_val <- sig_desoc_val/ (sig_desoc_val+sig_ocup_val)
var_txdesoc_val <- (sig_ocup_val^2*var_ocup_val+sig_desoc_val^2*var_desoc_val)/(S_val^4)
se_txdesoc_val <- sqrt(var_txdesoc_val)
txdesoc_val <- txdesoc_val*100
se_txdesoc_val <- se_txdesoc_val*100

ICinf_val<-txdesoc_val-1.96*se_txdesoc_val
ICsup_val<-txdesoc_val+1.96*se_txdesoc_val

txdesoc_val <- window(ts.union(ts(txdesoc_val, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_val <- window(ts.union(ts(ICinf_val, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_val <- window(ts.union(ts(ICsup_val, start = 2012, frequency = 4)), start = c(2013,4))

plot(est_direta_val, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa de desocupação (%)", ylim=c(-2,23))
lines(txdesoc_val, col = "red", lty = 1, lwd = 2)
lines(ICinf_val, col = "red", lty = 2)
lines(ICsup_val, col = "red", lty = 2)
legend("topleft", legend = c("Estimativa direta", "Taxa de desocupação: model-based", "IC 95%: model-based"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)
mtext("07 - Vale do Rio Doce (Mod. Multivariado)", side = 3, outer = FALSE, line = 0.5, font = 2, cex = 1.2)


#### 08 - CENTRAL ##############################################################

est_direta_cen <- baseestr8reg$`08-Central`$Taxa.de.desocupação
est_direta_cen <-  window(ts.union(ts(est_direta_cen, start = 2012, frequency = 4)), start = c(2013,4))
est_direta_cen <- est_direta_cen*100

sig_desoc_cen <- env1$modelo_mult$ts.signal_8
sig_ocup_cen <- env2$modelo_mult$ts.signal_8
var_desoc_cen <- (env1$modelo_mult$se.signal_8)^2
var_ocup_cen <- (env2$modelo_mult$se.signal_8)^2
S_cen <- sig_desoc_cen+sig_ocup_cen

txdesoc_cen <- sig_desoc_cen/ (sig_desoc_cen+sig_ocup_cen)
var_txdesoc_cen <- (sig_ocup_cen^2*var_ocup_cen+sig_desoc_cen^2*var_desoc_cen)/(S_cen^4)
se_txdesoc_cen <- sqrt(var_txdesoc_cen)
txdesoc_cen <- txdesoc_cen*100
se_txdesoc_cen <- se_txdesoc_cen*100

ICinf_cen<-txdesoc_cen-1.96*se_txdesoc_cen
ICsup_cen<-txdesoc_cen+1.96*se_txdesoc_cen

txdesoc_cen <- window(ts.union(ts(txdesoc_cen, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_cen <- window(ts.union(ts(ICinf_cen, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_cen <- window(ts.union(ts(ICsup_cen, start = 2012, frequency = 4)), start = c(2013,4))

plot(est_direta_cen, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa de desocupação (%)", ylim=c(0,16))
lines(txdesoc_cen, col = "red", lty = 1, lwd = 2)
lines(ICinf_cen, col = "red", lty = 2)
lines(ICsup_cen, col = "red", lty = 2)
legend("topleft", legend = c("Estimativa direta", "Taxa de desocupação: model-based", "IC 95%: model-based"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)
mtext("08 - Central (Mod. Multivariado)", side = 3, outer = FALSE, line = 0.5, font = 2, cex = 1.2)


