################################################################################
##          SCRIPT CÁLCULO DA TAXA DE DESOCUPAÇÃO - OUT OF MODEL              ##
################################################################################

rm(list=ls())
gc()
options(scipen=999)

# Aqui, os dados para desoc vêm do mod mult. e os dados para ocup vêm do mod uni

#### UPLOAD DA BASE

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")

# Desocupação

env17<-new.env()
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/estimados/01_mod_comcorr.Rdata", envir = env17)

# Ocupação

env9<-new.env()
env10<-new.env()
env11<-new.env()
env12<-new.env()
env13<-new.env()
env14<-new.env()
env15<-new.env()
env16<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/01_mod_bh.Rdata", envir = env9)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/02_mod_ent.Rdata", envir = env10)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/03_mod_sul.Rdata", envir = env11)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/04_mod_trg.Rdata", envir = env12)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/05_mod_mat.Rdata", envir = env13)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/06_mod_nrt.Rdata", envir = env14)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/07_mod_val.Rdata", envir = env15)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/08_mod_cen.Rdata", envir = env16)

#### 01 - BELO HORIZONTE #######################################################

est_direta_bh <- baseestr8reg$`01-Belo Horizonte`$Taxa.de.desocupação
est_direta_bh <-  window(ts.union(ts(est_direta_bh, start = 2012, frequency = 4)), start = c(2014,1))
est_direta_bh <- est_direta_bh*100
cv_direta_bh <- baseestr8reg$`01-Belo Horizonte`$CV.taxa
cv_direta_bh <-  window(ts.union(ts(cv_direta_bh, start = 2012, frequency = 4)), start = c(2014,1))

# Cálculo utilizando sinal

sig_desoc_bh <- env17$modelo_mult$ts.signal_1
sig_ocup_bh <- env9$ar1_bh$ts.signal
var_desoc_bh <- (env17$modelo_mult$se.signal_1)^2
var_ocup_bh <- (env9$ar1_bh$se.signal)^2
S_bh <- sig_desoc_bh+sig_ocup_bh

txdesoc_bh <- sig_desoc_bh/(sig_desoc_bh+sig_ocup_bh)
var_txdesoc_bh <- ((1/(S_bh^2)*var_desoc_bh)+((sig_desoc_bh^2/S_bh^4)*(var_desoc_bh+var_ocup_bh)))
se_txdesoc_bh <- sqrt(var_txdesoc_bh)
txdesoc_bh <- txdesoc_bh*100
se_txdesoc_bh <- se_txdesoc_bh*100

cv_txdesoc_bh <- se_txdesoc_bh/txdesoc_bh
cv_txdesoc_bh <- cv_txdesoc_bh*100

ICinf_bh<-txdesoc_bh-1.96*se_txdesoc_bh
ICsup_bh<-txdesoc_bh+1.96*se_txdesoc_bh

txdesoc_bh <- window(ts.union(ts(txdesoc_bh, start = 2012, frequency = 4)), start = c(2014,1))
cv_txdesoc_bh <- window(ts.union(ts(cv_txdesoc_bh, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_bh <- window(ts.union(ts(ICinf_bh, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_bh <- window(ts.union(ts(ICsup_bh, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(sig_desoc_bh, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(sig_ocup_bh, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(sig_desoc_bh, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(S_bh, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_bh, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(3,18))
lines(txdesoc_bh, col = "red", lty = 1, lwd = 2)
lines(ICinf_bh, col = "red", lty = 2)
lines(ICsup_bh, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Taxa de desocupação - est. indireta", "IC 95% - est. indireta"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_bh, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(3,14))
lines(cv_txdesoc_bh, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV est. indireta"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("01 - Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# Cálculo da tendência da taxa de desemprego

trend_desoc_bh <- env17$modelo_mult$ts.trend_1
trend_ocup_bh <- env9$ar1_bh$ts.trend
var.trend_desoc_bh <- (env17$modelo_mult$se.trend_1)^2
var.trend_ocup_bh <- (env9$ar1_bh$se.trend)^2
T_bh <- trend_desoc_bh+trend_ocup_bh

trend_tx_bh <- trend_desoc_bh/(trend_desoc_bh+trend_ocup_bh)
var.trend_tx_bh <- ((1/(T_bh^2)*var.trend_desoc_bh)+((trend_desoc_bh^2/T_bh^4)*(var.trend_desoc_bh+var.trend_ocup_bh)))
se.trend_tx_bh <- sqrt(var.trend_tx_bh)
trend_tx_bh <- trend_tx_bh*100
se.trend_tx_bh <- se.trend_tx_bh*100

cv.trend_tx_bh <- se.trend_tx_bh/trend_tx_bh
cv.trend_tx_bh <- cv.trend_tx_bh*100

trend_ICinf_bh<-trend_tx_bh-1.96*se.trend_tx_bh
trend_ICsup_bh<-trend_tx_bh+1.96*se.trend_tx_bh

trend_tx_bh <- window(ts.union(ts(trend_tx_bh, start = 2012, frequency = 4)), start = c(2014,1))
cv.trend_tx_bh <- window(ts.union(ts(cv.trend_tx_bh, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICinf_bh <- window(ts.union(ts(trend_ICinf_bh, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICsup_bh <- window(ts.union(ts(trend_ICsup_bh, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(trend_desoc_bh, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(trend_ocup_bh, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(trend_desoc_bh, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(T_bh, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_bh, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(3,18))
lines(trend_tx_bh, col = "red", lty = 1, lwd = 2)
lines(trend_ICinf_bh, col = "red", lty = 2)
lines(trend_ICsup_bh, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Tendência da taxa de desocupação: est. indireta", "IC 95% da tendência"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_bh, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(3,14))
lines(cv.trend_tx_bh, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV da tendência da taxa de desocupação"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("01 - Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


#### 02 - COLAR E ENTORNO METROPOLITANO DE BELO HORIZONTE ######################

est_direta_ent <- baseestr8reg$`02-Colar e Entorno metropolitano de BH`$Taxa.de.desocupação
est_direta_ent <-  window(ts.union(ts(est_direta_ent, start = 2012, frequency = 4)), start = c(2014,1))
est_direta_ent <- est_direta_ent*100
cv_direta_ent <- baseestr8reg$`02-Colar e Entorno metropolitano de BH`$CV.taxa
cv_direta_ent <-  window(ts.union(ts(cv_direta_ent, start = 2012, frequency = 4)), start = c(2014,1))

# Cálculo utilizando sinal

sig_desoc_ent <- env17$modelo_mult$ts.signal_2
sig_ocup_ent <- env10$ar1_ent$ts.signal
var_desoc_ent <- (env17$modelo_mult$se.signal_2)^2
var_ocup_ent <- (env10$ar1_ent$se.signal)^2
S_ent <- sig_desoc_ent+sig_ocup_ent

txdesoc_ent <- sig_desoc_ent/(sig_desoc_ent+sig_ocup_ent)
var_txdesoc_ent <- ((1/(S_ent^2)*var_desoc_ent)+((sig_desoc_ent^2/S_ent^4)*(var_desoc_ent+var_ocup_ent)))
se_txdesoc_ent <- sqrt(var_txdesoc_ent)
txdesoc_ent <- txdesoc_ent*100
se_txdesoc_ent <- se_txdesoc_ent*100

cv_txdesoc_ent <- se_txdesoc_ent/txdesoc_ent
cv_txdesoc_ent <- cv_txdesoc_ent*100

ICinf_ent<-txdesoc_ent-1.96*se_txdesoc_ent
ICsup_ent<-txdesoc_ent+1.96*se_txdesoc_ent

txdesoc_ent <- window(ts.union(ts(txdesoc_ent, start = 2012, frequency = 4)), start = c(2014,1))
cv_txdesoc_ent <- window(ts.union(ts(cv_txdesoc_ent, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_ent <- window(ts.union(ts(ICinf_ent, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_ent <- window(ts.union(ts(ICsup_ent, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(sig_desoc_ent, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(sig_ocup_ent, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(sig_desoc_ent, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(S_ent, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_ent, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(4,22))
lines(txdesoc_ent, col = "red", lty = 1, lwd = 2)
lines(ICinf_ent, col = "red", lty = 2)
lines(ICsup_ent, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Taxa de desocupação - est. indireta", "IC 95% - est. indireta"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_ent, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(3,13))
lines(cv_txdesoc_ent, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV est. indireta"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Cálculo da tendência da taxa de desemprego

trend_desoc_ent <- env17$modelo_mult$ts.trend_2
trend_ocup_ent <- env10$ar1_ent$ts.trend
var.trend_desoc_ent <- (env17$modelo_mult$se.trend_2)^2
var.trend_ocup_ent <- (env10$ar1_ent$se.trend)^2
T_ent <- trend_desoc_ent+trend_ocup_ent

trend_tx_ent <- trend_desoc_ent/(trend_desoc_ent+trend_ocup_ent)
var.trend_tx_ent <- ((1/(T_ent^2)*var.trend_desoc_ent)+((trend_desoc_ent^2/T_ent^4)*(var.trend_desoc_ent+var.trend_ocup_ent)))
se.trend_tx_ent <- sqrt(var.trend_tx_ent)
trend_tx_ent <- trend_tx_ent*100
se.trend_tx_ent <- se.trend_tx_ent*100

cv.trend_tx_ent <- se.trend_tx_ent/trend_tx_ent
cv.trend_tx_ent <- cv.trend_tx_ent*100

trend_ICinf_ent<-trend_tx_ent-1.96*se.trend_tx_ent
trend_ICsup_ent<-trend_tx_ent+1.96*se.trend_tx_ent

trend_tx_ent <- window(ts.union(ts(trend_tx_ent, start = 2012, frequency = 4)), start = c(2014,1))
cv.trend_tx_ent <- window(ts.union(ts(cv.trend_tx_ent, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICinf_ent <- window(ts.union(ts(trend_ICinf_ent, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICsup_ent <- window(ts.union(ts(trend_ICsup_ent, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(trend_desoc_ent, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(trend_ocup_ent, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(trend_desoc_ent, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(T_ent, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_ent, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(4,22))
lines(trend_tx_ent, col = "red", lty = 1, lwd = 2)
lines(trend_ICinf_ent, col = "red", lty = 2)
lines(trend_ICsup_ent, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Tendência da taxa de desocupação: est. indireta", "IC 95% da tendência"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_ent, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(3,13))
lines(cv.trend_tx_ent, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV da tendência da taxa de desocupação"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


#### 03 - SUL DE MINAS #########################################################

est_direta_sul <- baseestr8reg$`03-Sul de Minas`$Taxa.de.desocupação
est_direta_sul <-  window(ts.union(ts(est_direta_sul, start = 2012, frequency = 4)), start = c(2014,1))
est_direta_sul <- est_direta_sul*100
cv_direta_sul <- baseestr8reg$`03-Sul de Minas`$CV.taxa
cv_direta_sul <-  window(ts.union(ts(cv_direta_sul, start = 2012, frequency = 4)), start = c(2014,1))

# Cálculo utilizando sinal

sig_desoc_sul <- env17$modelo_mult$ts.signal_3
sig_ocup_sul <- env11$ar1_sul$ts.signal
var_desoc_sul <- (env17$modelo_mult$se.signal_3)^2
var_ocup_sul <- (env11$ar1_sul$se.signal)^2
S_sul <- sig_desoc_sul+sig_ocup_sul

txdesoc_sul <- sig_desoc_sul/(sig_desoc_sul+sig_ocup_sul)
var_txdesoc_sul <- ((1/(S_sul^2)*var_desoc_sul)+((sig_desoc_sul^2/S_sul^4)*(var_desoc_sul+var_ocup_sul)))
se_txdesoc_sul <- sqrt(var_txdesoc_sul)
txdesoc_sul <- txdesoc_sul*100
se_txdesoc_sul <- se_txdesoc_sul*100

cv_txdesoc_sul <- se_txdesoc_sul/txdesoc_sul
cv_txdesoc_sul <- cv_txdesoc_sul*100

ICinf_sul<-txdesoc_sul-1.96*se_txdesoc_sul
ICsup_sul<-txdesoc_sul+1.96*se_txdesoc_sul

txdesoc_sul <- window(ts.union(ts(txdesoc_sul, start = 2012, frequency = 4)), start = c(2014,1))
cv_txdesoc_sul <- window(ts.union(ts(cv_txdesoc_sul, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_sul <- window(ts.union(ts(ICinf_sul, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_sul <- window(ts.union(ts(ICsup_sul, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(sig_desoc_sul, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(sig_ocup_sul, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(sig_desoc_sul, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(S_sul, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_sul, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(2,15))
lines(txdesoc_sul, col = "red", lty = 1, lwd = 2)
lines(ICinf_sul, col = "red", lty = 2)
lines(ICsup_sul, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Taxa de desocupação - est. indireta", "IC 95% - est. indireta"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_sul, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(6,19))
lines(cv_txdesoc_sul, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV est. indireta"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("03 - Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Cálculo da tendência da taxa de desemprego

trend_desoc_sul <- env17$modelo_mult$ts.trend_3
trend_ocup_sul <- env11$ar1_sul$ts.trend
var.trend_desoc_sul <- (env17$modelo_mult$se.trend_3)^2
var.trend_ocup_sul <- (env11$ar1_sul$se.trend)^2
T_sul <- trend_desoc_sul+trend_ocup_sul

trend_tx_sul <- trend_desoc_sul/(trend_desoc_sul+trend_ocup_sul)
var.trend_tx_sul <- ((1/(T_sul^2)*var.trend_desoc_sul)+((trend_desoc_sul^2/T_sul^4)*(var.trend_desoc_sul+var.trend_ocup_sul)))
se.trend_tx_sul <- sqrt(var.trend_tx_sul)
trend_tx_sul <- trend_tx_sul*100
se.trend_tx_sul <- se.trend_tx_sul*100

cv.trend_tx_sul <- se.trend_tx_sul/trend_tx_sul
cv.trend_tx_sul <- cv.trend_tx_sul*100

trend_ICinf_sul<-trend_tx_sul-1.96*se.trend_tx_sul
trend_ICsup_sul<-trend_tx_sul+1.96*se.trend_tx_sul

trend_tx_sul <- window(ts.union(ts(trend_tx_sul, start = 2012, frequency = 4)), start = c(2014,1))
cv.trend_tx_sul <- window(ts.union(ts(cv.trend_tx_sul, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICinf_sul <- window(ts.union(ts(trend_ICinf_sul, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICsup_sul <- window(ts.union(ts(trend_ICsup_sul, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(trend_desoc_sul, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(trend_ocup_sul, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(trend_desoc_sul, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(T_sul, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_sul, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(2,15))
lines(trend_tx_sul, col = "red", lty = 1, lwd = 2)
lines(trend_ICinf_sul, col = "red", lty = 2)
lines(trend_ICsup_sul, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Tendência da taxa de desocupação: est. indireta", "IC 95% da tendência"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_sul, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(6,19))
lines(cv.trend_tx_sul, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV da tendência da taxa de desocupação"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("03 - Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


#### 04 - TRIANGULO MINEIRO ####################################################

est_direta_trg <- baseestr8reg$`04-Triângulo Mineiro`$Taxa.de.desocupação
est_direta_trg <-  window(ts.union(ts(est_direta_trg, start = 2012, frequency = 4)), start = c(2014,1))
est_direta_trg <- est_direta_trg*100
cv_direta_trg <- baseestr8reg$`04-Triângulo Mineiro`$CV.taxa
cv_direta_trg <-  window(ts.union(ts(cv_direta_trg, start = 2012, frequency = 4)), start = c(2014,1))

# Cálculo utilizando sinal

sig_desoc_trg <- env17$modelo_mult$ts.signal_4
sig_ocup_trg <- env12$ar1_trg$ts.signal
var_desoc_trg <- (env17$modelo_mult$se.signal_4)^2
var_ocup_trg <- (env12$ar1_trg$se.signal)^2
S_trg <- sig_desoc_trg+sig_ocup_trg

txdesoc_trg <- sig_desoc_trg/(sig_desoc_trg+sig_ocup_trg)
var_txdesoc_trg <- ((1/(S_trg^2)*var_desoc_trg)+((sig_desoc_trg^2/S_trg^4)*(var_desoc_trg+var_ocup_trg)))
se_txdesoc_trg <- sqrt(var_txdesoc_trg)
txdesoc_trg <- txdesoc_trg*100
se_txdesoc_trg <- se_txdesoc_trg*100

cv_txdesoc_trg <- se_txdesoc_trg/txdesoc_trg
cv_txdesoc_trg <- cv_txdesoc_trg*100

ICinf_trg<-txdesoc_trg-1.96*se_txdesoc_trg
ICsup_trg<-txdesoc_trg+1.96*se_txdesoc_trg

txdesoc_trg <- window(ts.union(ts(txdesoc_trg, start = 2012, frequency = 4)), start = c(2014,1))
cv_txdesoc_trg <- window(ts.union(ts(cv_txdesoc_trg, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_trg <- window(ts.union(ts(ICinf_trg, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_trg <- window(ts.union(ts(ICsup_trg, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(sig_desoc_trg, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(sig_ocup_trg, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(sig_desoc_trg, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(S_trg, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_trg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(2,13))
lines(txdesoc_trg, col = "red", lty = 1, lwd = 2)
lines(ICinf_trg, col = "red", lty = 2)
lines(ICsup_trg, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Taxa de desocupação - est. indireta", "IC 95% - est. indireta"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_trg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(5,19))
lines(cv_txdesoc_trg, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV est. indireta"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("04 - Triângulo Mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Cálculo da tendência da taxa de desemprego

trend_desoc_trg <- env17$modelo_mult$ts.trend_4
trend_ocup_trg <- env12$ar1_trg$ts.trend
var.trend_desoc_trg <- (env17$modelo_mult$se.trend_4)^2
var.trend_ocup_trg <- (env12$ar1_trg$se.trend)^2
T_trg <- trend_desoc_trg+trend_ocup_trg

trend_tx_trg <- trend_desoc_trg/(trend_desoc_trg+trend_ocup_trg)
var.trend_tx_trg <- ((1/(T_trg^2)*var.trend_desoc_trg)+((trend_desoc_trg^2/T_trg^4)*(var.trend_desoc_trg+var.trend_ocup_trg)))
se.trend_tx_trg <- sqrt(var.trend_tx_trg)
trend_tx_trg <- trend_tx_trg*100
se.trend_tx_trg <- se.trend_tx_trg*100

cv.trend_tx_trg <- se.trend_tx_trg/trend_tx_trg
cv.trend_tx_trg <- cv.trend_tx_trg*100

trend_ICinf_trg<-trend_tx_trg-1.96*se.trend_tx_trg
trend_ICsup_trg<-trend_tx_trg+1.96*se.trend_tx_trg

trend_tx_trg <- window(ts.union(ts(trend_tx_trg, start = 2012, frequency = 4)), start = c(2014,1))
cv.trend_tx_trg <- window(ts.union(ts(cv.trend_tx_trg, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICinf_trg <- window(ts.union(ts(trend_ICinf_trg, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICsup_trg <- window(ts.union(ts(trend_ICsup_trg, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(trend_desoc_trg, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(trend_ocup_trg, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(trend_desoc_trg, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(T_trg, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_trg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(2,13))
lines(trend_tx_trg, col = "red", lty = 1, lwd = 2)
lines(trend_ICinf_trg, col = "red", lty = 2)
lines(trend_ICsup_trg, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Tendência da taxa de desocupação: est. indireta", "IC 95% da tendência"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_trg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(5,19))
lines(cv.trend_tx_trg, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV da tendência da taxa de desocupação"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("04 - Triângulo Mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

#### 05 - ZONA DA MATA ####################################################

est_direta_mat <- baseestr8reg$`05-Mata de Minas Gerais`$Taxa.de.desocupação
est_direta_mat <-  window(ts.union(ts(est_direta_mat, start = 2012, frequency = 4)), start = c(2014,1))
est_direta_mat <- est_direta_mat*100
cv_direta_mat <- baseestr8reg$`05-Mata de Minas Gerais`$CV.taxa
cv_direta_mat<-  window(ts.union(ts(cv_direta_mat, start = 2012, frequency = 4)), start = c(2014,1))

# Cálculo utilizando sinal

sig_desoc_mat <- env17$modelo_mult$ts.signal_5
sig_ocup_mat <- env13$ar1_mat$ts.signal
var_desoc_mat <- (env17$modelo_mult$se.signal_5)^2
var_ocup_mat <- (env13$ar1_mat$se.signal)^2
S_mat <- sig_desoc_mat+sig_ocup_mat

txdesoc_mat <- sig_desoc_mat/(sig_desoc_mat+sig_ocup_mat)
var_txdesoc_mat <- ((1/(S_mat^2)*var_desoc_mat)+((sig_desoc_mat^2/S_mat^4)*(var_desoc_mat+var_ocup_mat)))
se_txdesoc_mat <- sqrt(var_txdesoc_mat)
txdesoc_mat <- txdesoc_mat*100
se_txdesoc_mat <- se_txdesoc_mat*100

cv_txdesoc_mat <- se_txdesoc_mat/txdesoc_mat
cv_txdesoc_mat <- cv_txdesoc_mat*100

ICinf_mat<-txdesoc_mat-1.96*se_txdesoc_mat
ICsup_mat<-txdesoc_mat+1.96*se_txdesoc_mat

txdesoc_mat <- window(ts.union(ts(txdesoc_mat, start = 2012, frequency = 4)), start = c(2014,1))
cv_txdesoc_mat <- window(ts.union(ts(cv_txdesoc_mat, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_mat <- window(ts.union(ts(ICinf_mat, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_mat <- window(ts.union(ts(ICsup_mat, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(sig_desoc_mat, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(sig_ocup_mat, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(sig_desoc_mat, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(S_mat, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_mat, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(3,14))
lines(txdesoc_mat, col = "red", lty = 1, lwd = 2)
lines(ICinf_mat, col = "red", lty = 2)
lines(ICsup_mat, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Taxa de desocupação - est. indireta", "IC 95% - est. indireta"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_mat, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(5,17))
lines(cv_txdesoc_mat, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV est. indireta"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("05 - Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Cálculo da tendência da taxa de desemprego

trend_desoc_mat <- env17$modelo_mult$ts.trend_5
trend_ocup_mat <- env13$ar1_mat$ts.trend
var.trend_desoc_mat <- (env17$modelo_mult$se.trend_5)^2
var.trend_ocup_mat <- (env13$ar1_mat$se.trend)^2
T_mat <- trend_desoc_mat+trend_ocup_mat

trend_tx_mat <- trend_desoc_mat/(trend_desoc_mat+trend_ocup_mat)
var.trend_tx_mat <- ((1/(T_mat^2)*var.trend_desoc_mat)+((trend_desoc_mat^2/T_mat^4)*(var.trend_desoc_mat+var.trend_ocup_mat)))
se.trend_tx_mat <- sqrt(var.trend_tx_mat)
trend_tx_mat <- trend_tx_mat*100
se.trend_tx_mat <- se.trend_tx_mat*100

cv.trend_tx_mat <- se.trend_tx_mat/trend_tx_mat
cv.trend_tx_mat <- cv.trend_tx_mat*100

trend_ICinf_mat<-trend_tx_mat-1.96*se.trend_tx_mat
trend_ICsup_mat<-trend_tx_mat+1.96*se.trend_tx_mat

trend_tx_mat <- window(ts.union(ts(trend_tx_mat, start = 2012, frequency = 4)), start = c(2014,1))
cv.trend_tx_mat <- window(ts.union(ts(cv.trend_tx_mat, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICinf_mat <- window(ts.union(ts(trend_ICinf_mat, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICsup_mat <- window(ts.union(ts(trend_ICsup_mat, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(trend_desoc_mat, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(trend_ocup_mat, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(trend_desoc_mat, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(T_mat, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_mat, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(3,14))
lines(trend_tx_mat, col = "red", lty = 1, lwd = 2)
lines(trend_ICinf_mat, col = "red", lty = 2)
lines(trend_ICsup_mat, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Tendência da taxa de desocupação: est. indireta", "IC 95% da tendência"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_mat, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(5,17))
lines(cv.trend_tx_mat, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV da tendência da taxa de desocupação"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("05 - Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

#### 06 - NORTE DE MINAS #######################################################

est_direta_nrt <- baseestr8reg$`06-Norte de Minas`$Taxa.de.desocupação
est_direta_nrt <-  window(ts.union(ts(est_direta_nrt, start = 2012, frequency = 4)), start = c(2014,1))
est_direta_nrt <- est_direta_nrt*100
cv_direta_nrt <- baseestr8reg$`06-Norte de Minas`$CV.taxa
cv_direta_nrt <-  window(ts.union(ts(cv_direta_nrt, start = 2012, frequency = 4)), start = c(2014,1))

# Cálculo utilizando sinal

sig_desoc_nrt <- env17$modelo_mult$ts.signal_6
sig_ocup_nrt <- env14$ar1_nrt$ts.signal
var_desoc_nrt <- (env17$modelo_mult$se.signal_6)^2
var_ocup_nrt <- (env14$ar1_nrt$se.signal)^2
S_nrt <- sig_desoc_nrt+sig_ocup_nrt

txdesoc_nrt <- sig_desoc_nrt/(sig_desoc_nrt+sig_ocup_nrt)
var_txdesoc_nrt <- ((1/(S_nrt^2)*var_desoc_nrt)+((sig_desoc_nrt^2/S_nrt^4)*(var_desoc_nrt+var_ocup_nrt)))
se_txdesoc_nrt <- sqrt(var_txdesoc_nrt)
txdesoc_nrt <- txdesoc_nrt*100
se_txdesoc_nrt <- se_txdesoc_nrt*100

cv_txdesoc_nrt <- se_txdesoc_nrt/txdesoc_nrt
cv_txdesoc_nrt <- cv_txdesoc_nrt*100

ICinf_nrt<-txdesoc_nrt-1.96*se_txdesoc_nrt
ICsup_nrt<-txdesoc_nrt+1.96*se_txdesoc_nrt

txdesoc_nrt <- window(ts.union(ts(txdesoc_nrt, start = 2012, frequency = 4)), start = c(2014,1))
cv_txdesoc_nrt <- window(ts.union(ts(cv_txdesoc_nrt, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_nrt <- window(ts.union(ts(ICinf_nrt, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_nrt <- window(ts.union(ts(ICsup_nrt, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(sig_desoc_nrt, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(sig_ocup_nrt, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(sig_desoc_nrt, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(S_nrt, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_nrt, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(4,21))
lines(txdesoc_nrt, col = "red", lty = 1, lwd = 2)
lines(ICinf_nrt, col = "red", lty = 2)
lines(ICsup_nrt, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Taxa de desocupação - est. indireta", "IC 95% - est. indireta"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_nrt, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(4,20))
lines(cv_txdesoc_nrt, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV est. indireta"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Cálculo da tendência da taxa de desemprego

trend_desoc_nrt <- env17$modelo_mult$ts.trend_6
trend_ocup_nrt <- env14$ar1_nrt$ts.trend
var.trend_desoc_nrt <- (env17$modelo_mult$se.trend_6)^2
var.trend_ocup_nrt <- (env14$ar1_nrt$se.trend)^2
T_nrt <- trend_desoc_nrt+trend_ocup_nrt

trend_tx_nrt <- trend_desoc_nrt/(trend_desoc_nrt+trend_ocup_nrt)
var.trend_tx_nrt <- ((1/(T_nrt^2)*var.trend_desoc_nrt)+((trend_desoc_nrt^2/T_nrt^4)*(var.trend_desoc_nrt+var.trend_ocup_nrt)))
se.trend_tx_nrt <- sqrt(var.trend_tx_nrt)
trend_tx_nrt <- trend_tx_nrt*100
se.trend_tx_nrt <- se.trend_tx_nrt*100

cv.trend_tx_nrt <- se.trend_tx_nrt/trend_tx_nrt
cv.trend_tx_nrt <- cv.trend_tx_nrt*100

trend_ICinf_nrt<-trend_tx_nrt-1.96*se.trend_tx_nrt
trend_ICsup_nrt<-trend_tx_nrt+1.96*se.trend_tx_nrt

trend_tx_nrt <- window(ts.union(ts(trend_tx_nrt, start = 2012, frequency = 4)), start = c(2014,1))
cv.trend_tx_nrt <- window(ts.union(ts(cv.trend_tx_nrt, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICinf_nrt <- window(ts.union(ts(trend_ICinf_nrt, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICsup_nrt <- window(ts.union(ts(trend_ICsup_nrt, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(trend_desoc_nrt, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(trend_ocup_nrt, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(trend_desoc_nrt, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(T_nrt, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_nrt, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(4,21))
lines(trend_tx_nrt, col = "red", lty = 1, lwd = 2)
lines(trend_ICinf_nrt, col = "red", lty = 2)
lines(trend_ICsup_nrt, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Tendência da taxa de desocupação: est. indireta", "IC 95% da tendência"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_nrt, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(4,20))
lines(cv.trend_tx_nrt, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV da tendência da taxa de desocupação"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

#### 07 - VALE DO RIO DOCE #####################################################

est_direta_val <- baseestr8reg$`07-Vale do Rio Doce`$Taxa.de.desocupação
est_direta_val <-  window(ts.union(ts(est_direta_val, start = 2012, frequency = 4)), start = c(2014,1))
est_direta_val <- est_direta_val*100
cv_direta_val <- baseestr8reg$`07-Vale do Rio Doce`$CV.taxa
cv_direta_val <-  window(ts.union(ts(cv_direta_val, start = 2012, frequency = 4)), start = c(2014,1))

# Cálculo utilizando sinal

sig_desoc_val <- env17$modelo_mult$ts.signal_7
sig_ocup_val <- env15$ar1_val$ts.signal
var_desoc_val <- (env17$modelo_mult$se.signal_7)^2
var_ocup_val <- (env15$ar1_val$se.signal)^2
S_val <- sig_desoc_val+sig_ocup_val

txdesoc_val <- sig_desoc_val/(sig_desoc_val+sig_ocup_val)
var_txdesoc_val <- ((1/(S_val^2)*var_desoc_val)+((sig_desoc_val^2/S_val^4)*(var_desoc_val+var_ocup_val)))
se_txdesoc_val <- sqrt(var_txdesoc_val)
txdesoc_val <- txdesoc_val*100
se_txdesoc_val <- se_txdesoc_val*100

cv_txdesoc_val <- se_txdesoc_val/txdesoc_val
cv_txdesoc_val <- cv_txdesoc_val*100

ICinf_val<-txdesoc_val-1.96*se_txdesoc_val
ICsup_val<-txdesoc_val+1.96*se_txdesoc_val

txdesoc_val <- window(ts.union(ts(txdesoc_val, start = 2012, frequency = 4)), start = c(2014,1))
cv_txdesoc_val <- window(ts.union(ts(cv_txdesoc_val, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_val <- window(ts.union(ts(ICinf_val, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_val <- window(ts.union(ts(ICsup_val, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(sig_desoc_val, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(sig_ocup_val, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(sig_desoc_val, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(S_val, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_val, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(3,22))
lines(txdesoc_val, col = "red", lty = 1, lwd = 2)
lines(ICinf_val, col = "red", lty = 2)
lines(ICsup_val, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Taxa de desocupação - est. indireta", "IC 95% - est. indireta"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_val, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(4,18))
lines(cv_txdesoc_val, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV est. indireta"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("07 - Vale do Rio Doce", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Cálculo da tendência da taxa de desemprego

trend_desoc_val <- env17$modelo_mult$ts.trend_7
trend_ocup_val <- env15$ar1_val$ts.trend
var.trend_desoc_val <- (env17$modelo_mult$se.trend_7)^2
var.trend_ocup_val <- (env15$ar1_val$se.trend)^2
T_val <- trend_desoc_val+trend_ocup_val

trend_tx_val <- trend_desoc_val/(trend_desoc_val+trend_ocup_val)
var.trend_tx_val <- ((1/(T_val^2)*var.trend_desoc_val)+((trend_desoc_val^2/T_val^4)*(var.trend_desoc_val+var.trend_ocup_val)))
se.trend_tx_val <- sqrt(var.trend_tx_val)
trend_tx_val <- trend_tx_val*100
se.trend_tx_val <- se.trend_tx_val*100

cv.trend_tx_val <- se.trend_tx_val/trend_tx_val
cv.trend_tx_val <- cv.trend_tx_val*100

trend_ICinf_val<-trend_tx_val-1.96*se.trend_tx_val
trend_ICsup_val<-trend_tx_val+1.96*se.trend_tx_val

trend_tx_val <- window(ts.union(ts(trend_tx_val, start = 2012, frequency = 4)), start = c(2014,1))
cv.trend_tx_val <- window(ts.union(ts(cv.trend_tx_val, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICinf_val <- window(ts.union(ts(trend_ICinf_val, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICsup_val <- window(ts.union(ts(trend_ICsup_val, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(trend_desoc_val, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(trend_ocup_val, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(trend_desoc_val, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(T_val, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_val, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(3,22))
lines(trend_tx_val, col = "red", lty = 1, lwd = 2)
lines(trend_ICinf_val, col = "red", lty = 2)
lines(trend_ICsup_val, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Tendência da taxa de desocupação: est. indireta", "IC 95% da tendência"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_val, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(4,18))
lines(cv.trend_tx_val, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV da tendência da taxa de desocupação"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("07 - Vale do Rio Doce", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

#### 08 - CENTRAL ##############################################################

est_direta_cen <- baseestr8reg$`08-Central`$Taxa.de.desocupação
est_direta_cen <-  window(ts.union(ts(est_direta_cen, start = 2012, frequency = 4)), start = c(2014,1))
est_direta_cen <- est_direta_cen*100
cv_direta_cen <- baseestr8reg$`08-Central`$CV.taxa
cv_direta_cen <-  window(ts.union(ts(cv_direta_cen, start = 2012, frequency = 4)), start = c(2014,1))

# Cálculo utilizando sinal

sig_desoc_cen <- env17$modelo_mult$ts.signal_8
sig_ocup_cen <- env16$ar1_cen$ts.signal
var_desoc_cen <- (env17$modelo_mult$se.signal_8)^2
var_ocup_cen <- (env16$ar1_cen$se.signal)^2
S_cen <- sig_desoc_cen+sig_ocup_cen

txdesoc_cen <- sig_desoc_cen/(sig_desoc_cen+sig_ocup_cen)
var_txdesoc_cen <- ((1/(S_cen^2)*var_desoc_cen)+((sig_desoc_cen^2/S_cen^4)*(var_desoc_cen+var_ocup_cen)))
se_txdesoc_cen <- sqrt(var_txdesoc_cen)
txdesoc_cen <- txdesoc_cen*100
se_txdesoc_cen <- se_txdesoc_cen*100

cv_txdesoc_cen <- se_txdesoc_cen/txdesoc_cen
cv_txdesoc_cen <- cv_txdesoc_cen*100

ICinf_cen<-txdesoc_cen-1.96*se_txdesoc_cen
ICsup_cen<-txdesoc_cen+1.96*se_txdesoc_cen

txdesoc_cen <- window(ts.union(ts(txdesoc_cen, start = 2012, frequency = 4)), start = c(2014,1))
cv_txdesoc_cen <- window(ts.union(ts(cv_txdesoc_cen, start = 2012, frequency = 4)), start = c(2014,1))
ICinf_cen <- window(ts.union(ts(ICinf_cen, start = 2012, frequency = 4)), start = c(2014,1))
ICsup_cen <- window(ts.union(ts(ICsup_cen, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(sig_desoc_cen, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(sig_ocup_cen, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(sig_desoc_cen, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(S_cen, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_cen, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(3,15))
lines(txdesoc_cen, col = "red", lty = 1, lwd = 2)
lines(ICinf_cen, col = "red", lty = 2)
lines(ICsup_cen, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Taxa de desocupação - est. indireta", "IC 95% - est. indireta"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_cen, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(3,21))
lines(cv_txdesoc_cen, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV est. indireta"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("08 - Central", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Cálculo da tendência da taxa de desemprego

trend_desoc_cen <- env17$modelo_mult$ts.trend_8
trend_ocup_cen <- env16$ar1_cen$ts.trend
var.trend_desoc_cen <- (env17$modelo_mult$se.trend_8)^2
var.trend_ocup_cen <- (env16$ar1_cen$se.trend)^2
T_cen <- trend_desoc_cen+trend_ocup_cen

trend_tx_cen <- trend_desoc_cen/(trend_desoc_cen+trend_ocup_cen)
var.trend_tx_cen <- ((1/(T_cen^2)*var.trend_desoc_cen)+((trend_desoc_cen^2/T_cen^4)*(var.trend_desoc_cen+var.trend_ocup_cen)))
se.trend_tx_cen <- sqrt(var.trend_tx_cen)
trend_tx_cen <- trend_tx_cen*100
se.trend_tx_cen <- se.trend_tx_cen*100

cv.trend_tx_cen <- se.trend_tx_cen/trend_tx_cen
cv.trend_tx_cen <- cv.trend_tx_cen*100

trend_ICinf_cen<-trend_tx_cen-1.96*se.trend_tx_cen
trend_ICsup_cen<-trend_tx_cen+1.96*se.trend_tx_cen

trend_tx_cen <- window(ts.union(ts(trend_tx_cen, start = 2012, frequency = 4)), start = c(2014,1))
cv.trend_tx_cen <- window(ts.union(ts(cv.trend_tx_cen, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICinf_cen <- window(ts.union(ts(trend_ICinf_cen, start = 2012, frequency = 4)), start = c(2014,1))
trend_ICsup_cen <- window(ts.union(ts(trend_ICsup_cen, start = 2012, frequency = 4)), start = c(2014,1))

cor.test(window(ts.union(ts(trend_desoc_cen, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(trend_ocup_cen, start = 2012, frequency = 4)), start = c(2014,1)))

cor.test(window(ts.union(ts(trend_desoc_cen, start = 2012, frequency = 4)), start = c(2014,1)),
         window(ts.union(ts(T_cen, start = 2012, frequency = 4)), start = c(2014,1)))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(est_direta_cen, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Taxa da desocupação (%)", ylim=c(3,15))
lines(trend_tx_cen, col = "red", lty = 1, lwd = 2)
lines(trend_ICinf_cen, col = "red", lty = 2)
lines(trend_ICsup_cen, col = "red", lty = 2)
legend("topleft", legend = c("Est. direta", "Tendência da taxa de desocupação: est. indireta", "IC 95% da tendência"), 
       col = c("black","red","red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n", cex=0.8)

plot(cv_direta_cen, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(4,21))
lines(cv.trend_tx_cen, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV est. direta","CV da tendência da taxa de desocupação"), 
       col = c("black","red"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("08 - Central", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


