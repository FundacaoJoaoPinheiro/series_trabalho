################################################################################
##    VERIFICANDO SE A MUDANÇA NO CALCULO SAZONAL IMPACTOU OS RESULTADOS      ##
################################################################################

# A PRINCÍPIO, NESTE SCRIPT OS RESUTADOS FORAM MODIFICADOS APENAS PARA BH e VALE

## Para visualizar os gráficos em segunda tela:

dev.new()
dev.new()


### 01 - BELO HORIZONTE ########################################################
rm(list = ls())
gc()

env1<-new.env()
env2<-new.env()
env3<-new.env()
env4<-new.env()

load("D:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/01_mod_bh.Rdata", envir = env1)
load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/01_mod_bh.Rdata", envir = env2)
load("D:/FJP2425/Programacao/data/Rdatas/Antes da correção sazonal/01_smooth_bh_desoc.Rdata", envir = env3)
load("D:/FJP2425/Programacao/data/Rdatas/Antes da correção sazonal/01_estrutural_bh_desoc.Rdata", envir = env4)

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
bh<-baseestr8reg$`01-Belo Horizonte`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtbh<-baseal8reg$`01-Belo Horizonte` 
dbbh<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/01_params_bh.RDS")

desoc_bh <- bh$Total.de.desocupados/1000
se_db<- bh$sd_d/1000
cv_bh <- se_db/desoc_bh
ICinf_bh<-desoc_bh-1.96*se_db
ICsup_bh<-desoc_bh+1.96*se_db

desoc_bh <- window(ts.union(ts(desoc_bh, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_bh <- window(ts.union(ts(ICinf_bh, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_bh <- window(ts.union(ts(ICsup_bh, start = 2012, frequency = 4)), start = c(2013,3))
cv_bh <- window(ts.union(ts(cv_bh, start = 2012, frequency = 4)), start = c(2013,3))

# Comparativo AR(1)

smtsinal_ar1bh <- env1$ar1_bh$ts.signal
smtsinal_ar1bh <- window(ts.union(ts(smtsinal_ar1bh, start = 2012, frequency = 4)), start = c(2013,3))
estsinal_ar1bh <- env2$ar1_bh$ts.signal
estsinal_ar1bh <- window(ts.union(ts(estsinal_ar1bh, start = 2012, frequency = 4)), start = c(2013,3))

prebh_smtsinal_ar1 <- env3$ar1_bh$ts.signal
prebh_smtsinal_ar1 <- window(ts.union(ts(prebh_smtsinal_ar1, start = 2012, frequency = 4)), start = c(2013,3))
prebh_estsinal_ar1 <- env4$ar1_bh$ts.signal
prebh_estsinal_ar1 <- window(ts.union(ts(prebh_estsinal_ar1, start = 2012, frequency = 4)), start = c(2013,3))
  
smtcv_ar1bh <- env1$ar1_bh$cv.signal
smtcv_ar1bh <- window(ts.union(ts(smtcv_ar1bh, start = 2012, frequency = 4)), start = c(2013,3))
estcv_ar1bh <- env2$ar1_bh$cv.signal
estcv_ar1bh <- window(ts.union(ts(estcv_ar1bh, start = 2012, frequency = 4)), start = c(2013,3))

prebh_smtcv_ar1 <- env3$ar1_bh$cv.signal
prebh_smtcv_ar1 <- window(ts.union(ts(prebh_smtcv_ar1, start = 2012, frequency = 4)), start = c(2013,3))
prebh_estcv_ar1 <- env4$ar1_bh$cv.signal
prebh_estcv_ar1 <- window(ts.union(ts(prebh_estcv_ar1, start = 2012, frequency = 4)), start = c(2013,3))


par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_bh, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)", ylim=c(40,245))
lines(smtsinal_ar1bh, col = "red", lty = 1, lwd = 2)
lines(estsinal_ar1bh, col = "blue", lty = 1, lwd = 2)
lines(prebh_smtsinal_ar1, col= "green", lty = 1, lwd = 2)
lines(prebh_estsinal_ar1, col= "yellow", lty = 1, lwd = 2)
lines(ICinf_bh, col = "black", lty = 2)
lines(ICsup_bh, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação", "Sinal Smooth - Ajustado","Sinal Estrutural - Ajustado",
                             "Sinal Smooth - Pré","Sinal Estrutural - Pré","IC 95% - estimativa direta"), 
       col = c("black","red", "blue","green","yellow","black"),lty = c(1,1,1,1,1,2),lwd = c(2,2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_bh*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",  ylim=c(4,15))
lines(smtcv_ar1bh, col = "red",lwd=2,lty = 1)
lines(estcv_ar1bh, col = "blue",lwd=2, lty = 1)
lines(prebh_smtcv_ar1, col = "green",lwd=2,lty = 1)
lines(prebh_estcv_ar1, col = "yellow",lwd=2,lty = 1)
legend("topleft", legend = c("CV desocupados","Smooth CV - Ajustado","Estrutural CV - Ajustado", "Smooth CV - Pré", "Estrutural CV - Pré"), 
       col = c("black","red", "blue","green","yellow"),lty = c(1,1,1,1,1),lwd = c(2,2,2,2,2),bty = "n", cex=0.8)
mtext("01 - Belo Horizonte AR(1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Comparativo MA(1)

smtsinal_ma1bh <- env1$ma1_bh$ts.signal
smtsinal_ma1bh <- window(ts.union(ts(smtsinal_ma1bh, start = 2012, frequency = 4)), start = c(2013,3))
estsinal_ma1bh <- env2$ma1_bh$ts.signal
estsinal_ma1bh <- window(ts.union(ts(estsinal_ma1bh, start = 2012, frequency = 4)), start = c(2013,3))

prebh_smtsinal_ma1 <- env3$ma1_bh$ts.signal
prebh_smtsinal_ma1 <- window(ts.union(ts(prebh_smtsinal_ma1, start = 2012, frequency = 4)), start = c(2013,3))
prebh_estsinal_ma1 <- env4$ma1_bh$ts.signal
prebh_estsinal_ma1 <- window(ts.union(ts(prebh_estsinal_ma1, start = 2012, frequency = 4)), start = c(2013,3))

smtcv_ma1bh <- env1$ma1_bh$cv.signal
smtcv_ma1bh <- window(ts.union(ts(smtcv_ma1bh, start = 2012, frequency = 4)), start = c(2013,3))
estcv_ma1bh <- env2$ma1_bh$cv.signal
estcv_ma1bh <- window(ts.union(ts(estcv_ma1bh, start = 2012, frequency = 4)), start = c(2013,3))

prebh_smtcv_ma1 <- env3$ma1_bh$cv.signal
prebh_smtcv_ma1 <- window(ts.union(ts(prebh_smtcv_ma1, start = 2012, frequency = 4)), start = c(2013,3))
prebh_estcv_ma1 <- env4$ma1_bh$cv.signal
prebh_estcv_ma1 <- window(ts.union(ts(prebh_estcv_ma1, start = 2012, frequency = 4)), start = c(2013,3))
  
par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_bh, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)", ylim=c(40,245))
lines(smtsinal_ma1bh, col = "red", lty = 1, lwd = 2)
lines(estsinal_ma1bh, col = "blue", lty = 1, lwd = 2)
lines(prebh_smtsinal_ma1, col= "green", lty = 1, lwd = 2)
lines(prebh_estsinal_ma1, col= "yellow", lty = 1, lwd = 2)
lines(ICinf_bh, col = "black", lty = 2)
lines(ICsup_bh, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação", "Sinal Smooth - Ajustado","Sinal Estrutural - Ajustado",
                             "Sinal Smooth - Pré","Sinal Estrutural - Pré","IC 95% - estimativa direta"), 
       col = c("black","red", "blue","green","yellow","black"),lty = c(1,1,1,1,1,2),lwd = c(2,2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_bh*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(3,15))
lines(smtcv_ma1bh, col = "red",lwd=2,lty = 1)
lines(estcv_ma1bh, col = "blue",lwd=2, lty = 1)
lines(prebh_smtcv_ma1, col = "green",lwd=2,lty = 1)
lines(prebh_estcv_ma1, col = "yellow",lwd=2,lty = 1)
legend("topleft", legend = c("CV desocupados","Smooth CV - Ajustado","Estrutural CV - Ajustado", "Smooth CV - Pré", "Estrutural CV - Pré"), 
       col = c("black","red", "blue","green","yellow"),lty = c(1,1,1,1,1),lwd = c(2,2,2,2,2),bty = "n", cex=0.8)
mtext("01 - Belo Horizonte MA(1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


# Verificando se o MA(1) se manteve como o melhor

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_bh, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)", ylim=c(40,245))
lines(smtsinal_ar1bh, col = "purple", lty = 1, lwd = 2)
lines(estsinal_ar1bh, col = "red", lty = 1, lwd = 2)
lines(smtsinal_ma1bh, col = "brown", lty = 1, lwd = 2)
lines(estsinal_ma1bh, col = "blue", lty = 1, lwd = 2)
lines(prebh_smtsinal_ar1, col= "orange", lty = 1, lwd = 2)
lines(prebh_estsinal_ar1, col= "green", lty = 1, lwd = 2)
lines(prebh_smtsinal_ma1, col= "pink", lty = 1, lwd = 2)
lines(prebh_estsinal_ma1, col= "yellow", lty = 1, lwd = 2)
lines(ICinf_bh, col = "black", lty = 2)
lines(ICsup_bh, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação", "Sinal Smooth AR(1) - Ajustado","Sinal Estrutural AR(1) - Ajustado",
                             "Sinal Smooth MA(1) - Ajustado", "Sinal Estrutural MA(1) - Ajustado",
                             "Sinal Smooth AR(1) - Pré","Sinal Estrutural AR(1) - Pré", "Sinal Smooth MA(1) - Pré", 
                             "Sinal Estrutural MA(1) - Pré","IC 95% - estimativa direta"), 
       col = c("black","purple","red","brown","blue","orange","green","pink","yellow","black"),
       lty = c(1,1,1,1,1,1,1,1,1,2),lwd = c(2,2,2,2,2,2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_bh*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(3,15))
lines(smtcv_ar1bh, col = "purple", lty = 1, lwd = 2)
lines(estcv_ar1bh, col = "red",lwd=2,lty = 1)
lines(smtcv_ma1bh, col = "brown", lty = 1, lwd = 2)
lines(estcv_ma1bh, col = "blue",lwd=2, lty = 1)
lines(prebh_smtcv_ar1, col= "orange", lty = 1, lwd = 2)
lines(prebh_estcv_ar1, col = "green",lwd=2,lty = 1)
lines(prebh_smtcv_ma1, col= "pink", lty = 1, lwd = 2)
lines(prebh_estcv_ma1, col = "yellow",lwd=2,lty = 1)
legend("topleft", legend = c("Desocupação", "CV Smooth AR(1) - Ajustado","CV Estrutural AR(1) - Ajustado",
                             "CV Smooth MA(1) - Ajustado", "CV Estrutural MA(1) - Ajustado",
                             "CV Smooth AR(1) - Pré","CV Estrutural AR(1) - Pré", "CV Smooth MA(1) - Pré", 
                             "CV Estrutural MA(1) - Pré"), 
       col = c("black","purple","red","brown","blue","orange","green","pink","yellow"),
       lty = c(1,1,1,1,1,1,1,1,1),lwd = c(2,2,2,2,2,2,2,2,2),bty = "n", cex=0.8)
mtext("Comparativo geral para Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 07 - VALE DO RIO DOCE ######################################################
rm(list = ls())
gc()

env1<-new.env()
env2<-new.env()
env3<-new.env()
env4<-new.env()

load("D:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/07_mod_val.Rdata", envir = env1)
load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/07_mod_val.Rdata", envir = env2)
load("D:/FJP2425/Programacao/data/Rdatas/Antes da correção sazonal/07_smooth_val_desoc.Rdata", envir = env3)
load("D:/FJP2425/Programacao/data/Rdatas/Antes da correção sazonal/07_estrutural_val_desoc.Rdata", envir = env4)

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
vl<-baseestr8reg$`07-Vale do Rio Doce`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtvl<-baseal8reg$`07-Vale do Rio Doce`
dbvl<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/07_params_rio.RDS") 

desoc_val <- vl$Total.de.desocupados/1000
se_db <- vl$sd_d/1000
cv_val <- se_db/desoc_val
ICinf_val<-desoc_val-1.96*se_db
ICsup_val<-desoc_val+1.96*se_db

desoc_val <- window(ts(desoc_val, start = 2012, frequency = 4), start = c(2013,3))
cv_val <- window(ts(cv_val, start = 2012, frequency = 4), start = c(2013,3))
ICinf_val <- window(ts(ICinf_val, start = 2012, frequency = 4), start = c(2013,3))
ICsup_val <- window(ts(ICsup_val, start = 2012, frequency = 4), start = c(2013,3))

# Comparativo AR(1)

smtsinal_ar1val <- env1$ar1_vl$ts.signal
smtsinal_ar1val <- window(ts(smtsinal_ar1val, start = 2012, frequency = 4), start = c(2013,3))
estsinal_ar1val <- env2$ar1_val$ts.signal
estsinal_ar1val <- window(ts(estsinal_ar1val, start = 2012, frequency = 4), start = c(2013,3))

preval_smtsinal_ar1 <- env3$ar1_vl$ts.signal
preval_smtsinal_ar1 <- window(ts(preval_smtsinal_ar1, start = 2012, frequency = 4), start = c(2013,3))
preval_estsinal_ar1 <- env4$ar1_val$ts.signal
preval_estsinal_ar1 <- window(ts(preval_estsinal_ar1, start = 2012, frequency = 4), start = c(2013,3))

smtcv_ar1val <- env1$ar1_vl$cv.signal
smtcv_ar1val <- window(ts(smtcv_ar1val, start = 2012, frequency = 4), start = c(2013,3))
estcv_ar1val <- env2$ar1_val$cv.signal
estcv_ar1val <- window(ts(estcv_ar1val, start = 2012, frequency = 4), start = c(2013,3))

preval_smtcv_ar1 <- env3$ar1_vl$cv.signal
preval_smtcv_ar1 <- window(ts(preval_smtcv_ar1, start = 2012, frequency = 4), start = c(2013,3))
preval_estcv_ar1 <- env4$ar1_val$cv.signal
preval_estcv_ar1 <- window(ts(preval_estcv_ar1, start = 2012, frequency = 4), start = c(2013,3))


par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_val, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)", ylim=c(20,230))
lines(smtsinal_ar1val, col = "red", lty = 1, lwd = 2)
lines(estsinal_ar1val, col = "blue", lty = 1, lwd = 2)
lines(preval_smtsinal_ar1, col= "green", lty = 1, lwd = 2)
lines(preval_estsinal_ar1, col= "yellow", lty = 1, lwd = 2)
lines(ICinf_val, col = "black", lty = 2)
lines(ICsup_val, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação", "Sinal Smooth - Ajustado","Sinal Estrutural - Ajustado",
                             "Sinal Smooth - Pré","Sinal Estrutural - Pré","IC 95% - estimativa direta"), 
       col = c("black","red", "blue","green","yellow","black"),lty = c(1,1,1,1,1,2),lwd = c(2,2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_val*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Coeficiente de Variação (%)",  ylim=c(2.5,22))
lines(smtcv_ar1val, col = "red",lwd=2,lty = 1)
lines(estcv_ar1val, col = "blue",lwd=2, lty = 1)
lines(preval_smtcv_ar1, col = "green",lwd=2,lty = 1)
lines(preval_estcv_ar1, col = "yellow",lwd=2,lty = 1)
legend("topleft", legend = c("CV desocupados","Smooth CV - Ajustado","Estrutural CV - Ajustado", "Smooth CV - Pré", "Estrutural CV - Pré"), 
       col = c("black","red", "blue","green","yellow"),lty = c(1,1,1,1,1),lwd = c(2,2,2,2,2),bty = "n", cex=0.8)
mtext("07 - Vale do Rio Doce AR(1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

