################################################################################
##   COMPARATIVO DAS SAZONALIDADES DOS MODELOS ESTRUTURAIS PARA DESOCUPADOS   ##
################################################################################

## Para visualizar os gráficos em segunda tela:

dev.new()
dev.new()


### 01 - BELO HORIZONTE ########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/01_mod_bh.Rdata", envir = env1)
load("D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/01_mod_bh.Rdata", envir = env2)

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

desoc_bh <- window(ts.union(ts(desoc_bh, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_bh <- window(ts.union(ts(ICinf_bh, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_bh <- window(ts.union(ts(ICsup_bh, start = 2012, frequency = 4)), start = c(2013,4))
cv_bh <- window(ts.union(ts(cv_bh, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_bh <- env1$ma1_bh$ts.signal
estsinal_bh <- window(ts.union(ts(estsinal_bh, start = 2012, frequency = 4)), start = c(2013,4))
dmysinal_bh <- env2$ma1_bh$ts.signal
dmysinal_bh <- window(ts.union(ts(dmysinal_bh, start = 2012, frequency = 4)), start = c(2013,4))

cv_estsinal_bh <- env1$ma1_bh$cv.signal
cv_estsinal_bh <- window(ts.union(ts(cv_estsinal_bh, start = 2012, frequency = 4)), start = c(2013,4))
cv_dmysinal_bh <- env2$ma1_bh$cv.signal
cv_dmysinal_bh <- window(ts.union(ts(cv_dmysinal_bh, start = 2012, frequency = 4)), start = c(2013,4))

esttrend_bh <- env1$ma1_bh$ts.trend
esttrend_bh <- window(ts.union(ts(esttrend_bh, start = 2012, frequency = 4)), start = c(2013,4))
dmytrend_bh <- env2$ma1_bh$ts.trend
dmytrend_bh <- window(ts.union(ts(dmytrend_bh, start = 2012, frequency = 4)), start = c(2013,4))

estsaz_bh <- env1$ma1_bh$ts.seasonal
estsaz_bh <- window(ts.union(ts(estsaz_bh, start = 2012, frequency = 4)), start = c(2013,4))
dmysaz_bh <- env2$ma1_bh$ts.seasonal
dmysaz_bh <- window(ts.union(ts(dmysaz_bh, start = 2012, frequency = 4)), start = c(2013,4))

estirr_bh <- env1$ma1_bh$ts.irregular
estirr_bh <- window(ts.union(ts(estirr_bh, start = 2012, frequency = 4)), start = c(2013,4))
dmyirr_bh <- env2$ma1_bh$ts.irregular
dmyirr_bh <- window(ts.union(ts(dmyirr_bh, start = 2012, frequency = 4)), start = c(2013,4))
  
esterro_bh <- env1$ma1_bh$ts.sampling_error
esterro_bh <- window(ts.union(ts(esterro_bh, start = 2012, frequency = 4)), start = c(2013,4))
dmyerro_bh <- env2$ma1_bh$ts.sampling_error
dmyerro_bh <- window(ts.union(ts(dmyerro_bh, start = 2012, frequency = 4)), start = c(2013,4))
  
par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_bh, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(45,270))
lines(estsinal_bh, col = "red", lty = 1, lwd = 2)
lines(dmysinal_bh, col = "blue", lty = 1, lwd = 2)
lines(ICinf_bh, col = "black", lty = 2)
lines(ICsup_bh, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Trigonométrica",
                             "Sinal da Desocupação MA(1) - Dummy","IC 95%: signal-based"), 
       col = c("black","red", "blue","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_bh*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(0,15))
lines(cv_estsinal_bh, col = "red",lwd=2,lty = 1)
lines(cv_dmysinal_bh, col = "blue",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV - trigonométrico","Sinal CV - dummy"), 
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("01 - Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(desoc_bh, type = "l", col = "black", lwd = 2,
     main = "Tendência: signal-based",
     xlab = "Ano", ylab = "Desocupados")
lines(esttrend_bh, col = "red", lty = 1, lwd = 2)
lines(dmytrend_bh, col = "blue", lty = 1, lwd = 2)
legend("bottom", legend = c("Desocupação: design-based", "Trigonométrica","Dummy"), 
      col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)

plot(estsaz_bh, type = "l", col = "red", lwd = 2,
     main = "Sazonalidade: signal-based",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-15,20))
lines(dmysaz_bh, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"), 
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(estirr_bh, type = "l", col = "red", lwd = 2,
     main = "Termo irregular",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-25,25))
lines(dmyirr_bh, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"), 
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(esterro_bh, type = "l", col = "red", lwd = 2,
     main = "Erro amostral",
     xlab = "Ano", ylab = "Desocupados")
lines(dmyerro_bh, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"), 
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("01 - Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 02 - COLAR E ENTORNO METROPOLITANO DE BELO HORIZONTE #######################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/02_mod_ent.Rdata", envir = env1)
load("D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/02_mod_ent.Rdata", envir = env2)

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
ent<-baseestr8reg$`02-Colar e Entorno metropolitano de BH`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtent<-baseal8reg$`02-Colar e Entorno Metropolitano de BH`
dbent<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/02_params_ent.RDS")

desoc_ent <- (ent$Total.de.desocupados)/1000
se_db <- (ent$sd_d)/1000
cv_ent <- se_db/desoc_ent
ICinf_ent<-desoc_ent-1.96*se_db
ICsup_ent<-desoc_ent+1.96*se_db

desoc_ent <- window(ts.union(ts(desoc_ent, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_ent <- window(ts.union(ts(ICinf_ent, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_ent <- window(ts.union(ts(ICsup_ent, start = 2012, frequency = 4)), start = c(2013,4))
cv_ent <- window(ts.union(ts(cv_ent, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_ent <- env1$ma1_ent$ts.signal
estsinal_ent <- window(ts.union(ts(estsinal_ent, start = 2012, frequency = 4)), start = c(2013,4))
dmysinal_ent <- env2$ma1_ent$ts.signal
dmysinal_ent <- window(ts.union(ts(dmysinal_ent, start = 2012, frequency = 4)), start = c(2013,4))

cv_estsinal_ent <- env1$ma1_ent$cv.signal
cv_estsinal_ent <- window(ts.union(ts(cv_estsinal_ent, start = 2012, frequency = 4)), start = c(2013,4))
cv_dmysinal_ent <- env2$ma1_ent$cv.signal
cv_dmysinal_ent <- window(ts.union(ts(cv_dmysinal_ent, start = 2012, frequency = 4)), start = c(2013,4))

esttrend_ent <- env1$ma1_ent$ts.trend
esttrend_ent <- window(ts.union(ts(esttrend_ent, start = 2012, frequency = 4)), start = c(2013,4))
dmytrend_ent <- env2$ma1_ent$ts.trend
dmytrend_ent <- window(ts.union(ts(dmytrend_ent, start = 2012, frequency = 4)), start = c(2013,4))

estsaz_ent <- env1$ma1_ent$ts.seasonal
estsaz_ent <- window(ts.union(ts(estsaz_ent, start = 2012, frequency = 4)), start = c(2013,4))
dmysaz_ent <- env2$ma1_ent$ts.seasonal
dmysaz_ent <- window(ts.union(ts(dmysaz_ent, start = 2012, frequency = 4)), start = c(2013,4))

estirr_ent <- env1$ma1_ent$ts.irregular
estirr_ent <- window(ts.union(ts(estirr_ent, start = 2012, frequency = 4)), start = c(2013,4))
dmyirr_ent <- env2$ma1_ent$ts.irregular
dmyirr_ent <- window(ts.union(ts(dmyirr_ent, start = 2012, frequency = 4)), start = c(2013,4))

esterro_ent <- env1$ma1_ent$ts.sampling_error
esterro_ent <- window(ts.union(ts(esterro_ent, start = 2012, frequency = 4)), start = c(2013,4))
dmyerro_ent <- env2$ma1_ent$ts.sampling_error
dmyerro_ent <- window(ts.union(ts(dmyerro_ent, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_ent, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(75,450))
lines(estsinal_ent, col = "red", lty = 1, lwd = 2)
lines(dmysinal_ent, col = "blue", lty = 1, lwd = 2)
lines(ICinf_ent, col = "black", lty = 2)
lines(ICsup_ent, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Trigonométrica",
                             "Sinal da Desocupação MA(1) - Dummy","IC 95%: signal-based"),
       col = c("black","red", "blue","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_ent*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(2,14))
lines(cv_estsinal_ent, col = "red",lwd=2,lty = 1)
lines(cv_dmysinal_ent, col = "blue",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV - trigonométrico","Sinal CV - dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("02 - Colar e Entorno metropolitano de Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(desoc_ent, type = "l", col = "black", lwd = 2,
     main = "Tendência: signal-based",
     xlab = "Ano", ylab = "Desocupados")
lines(esttrend_ent, col = "red", lty = 1, lwd = 2)
lines(dmytrend_ent, col = "blue", lty = 1, lwd = 2)
legend("bottom", legend = c("Desocupação: design-based", "Trigonométrica","Dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)

plot(estsaz_ent, type = "l", col = "red", lwd = 2,
     main = "Sazonalidade: signal-based",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-30,25))
lines(dmysaz_ent, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(estirr_ent, type = "l", col = "red", lwd = 2,
     main = "Termo irregular",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-35,30))
lines(dmyirr_ent, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(esterro_ent, type = "l", col = "red", lwd = 2,
     main = "Erro amostral",
     xlab = "Ano", ylab = "Desocupados")
lines(dmyerro_ent, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("02 - Colar e Entorno metropolitano de Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 03 - SUL DE MINAS ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/03_mod_sul.Rdata", envir = env1)
load("D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/03_mod_sul.Rdata", envir = env2)
  
baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
sul<-baseestr8reg$`03-Sul de Minas`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtsul<-baseal8reg$`03-Sul de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbsul<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/03_params_sul.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

desoc_sul <- sul$Total.de.desocupados/1000
se_db <- sul$sd_d/1000
cv_sul <- se_db/desoc_sul
ICinf_sul<-desoc_sul-1.96*se_db
ICsup_sul<-desoc_sul+1.96*se_db

desoc_sul <- window(ts.union(ts(desoc_sul, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_sul <- window(ts.union(ts(ICinf_sul, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_sul <- window(ts.union(ts(ICsup_sul, start = 2012, frequency = 4)), start = c(2013,4))
cv_sul <- window(ts.union(ts(cv_sul, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_sul <- env1$arma11_sul$ts.signal
estsinal_sul <- window(ts.union(ts(estsinal_sul, start = 2012, frequency = 4)), start = c(2013,4))
dmysinal_sul <- env2$arma11_sul$ts.signal
dmysinal_sul <- window(ts.union(ts(dmysinal_sul, start = 2012, frequency = 4)), start = c(2013,4))

cv_estsinal_sul <- env1$arma11_sul$cv.signal
cv_estsinal_sul <- window(ts.union(ts(cv_estsinal_sul, start = 2012, frequency = 4)), start = c(2013,4))
cv_dmysinal_sul <- env2$arma11_sul$cv.signal
cv_dmysinal_sul <- window(ts.union(ts(cv_dmysinal_sul, start = 2012, frequency = 4)), start = c(2013,4))

esttrend_sul <- env1$arma11_sul$ts.trend
esttrend_sul <- window(ts.union(ts(esttrend_sul, start = 2012, frequency = 4)), start = c(2013,4))
dmytrend_sul <- env2$arma11_sul$ts.trend
dmytrend_sul <- window(ts.union(ts(dmytrend_sul, start = 2012, frequency = 4)), start = c(2013,4))

estsaz_sul <- env1$arma11_sul$ts.seasonal
estsaz_sul <- window(ts.union(ts(estsaz_sul, start = 2012, frequency = 4)), start = c(2013,4))
dmysaz_sul <- -(env2$arma11_sul$ts.seasonal)
dmysaz_sul <- window(ts.union(ts(dmysaz_sul, start = 2012, frequency = 4)), start = c(2013,4))

estirr_sul <- env1$arma11_sul$ts.irregular
estirr_sul <- window(ts.union(ts(estirr_sul, start = 2012, frequency = 4)), start = c(2013,4))
dmyirr_sul <- env2$arma11_sul$ts.irregular
dmyirr_sul <- window(ts.union(ts(dmyirr_sul, start = 2012, frequency = 4)), start = c(2013,4))

esterro_sul <- env1$arma11_sul$ts.sampling_error
esterro_sul <- window(ts.union(ts(esterro_sul, start = 2012, frequency = 4)), start = c(2013,4))
dmyerro_sul <- env2$arma11_sul$ts.sampling_error
dmyerro_sul <- window(ts.union(ts(dmyerro_sul, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_sul, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(20,220))
lines(estsinal_sul, col = "red", lty = 1, lwd = 2)
lines(dmysinal_sul, col = "blue", lty = 1, lwd = 2)
lines(ICinf_sul, col = "black", lty = 2)
lines(ICsup_sul, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação ARMA(1,1) - Trigonométrica",
                             "Sinal da Desocupação ARMA(1,1) - Dummy","IC 95%: signal-based"),
       col = c("black","red", "blue","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_sul*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(5,20))
lines(cv_estsinal_sul, col = "red",lwd=2,lty = 1)
lines(cv_dmysinal_sul, col = "blue",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV - trigonométrico","Sinal CV - dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("03 - Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(desoc_sul, type = "l", col = "black", lwd = 2,
     main = "Tendência: signal-based",
     xlab = "Ano", ylab = "Desocupados")
lines(esttrend_sul, col = "red", lty = 1, lwd = 2)
lines(dmytrend_sul, col = "blue", lty = 1, lwd = 2)
legend("bottom", legend = c("Desocupação: design-based", "Trigonométrica","Dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)

plot(estsaz_sul, type = "l", col = "red", lwd = 2,
     main = "Sazonalidade: signal-based",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-15,20))
lines(dmysaz_sul, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(estirr_sul, type = "l", col = "red", lwd = 2,
     main = "Termo irregular",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-20,32))
lines(dmyirr_sul, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(esterro_sul, type = "l", col = "red", lwd = 2,
     main = "Erro amostral",
     xlab = "Ano", ylab = "Desocupados")
lines(dmyerro_sul, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("03 - Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

a<- data.frame(cbind(desoc_sul,esttrend_sul,dmytrend_sul,estsaz_sul,dmysaz_sul,estirr_sul,dmyirr_sul,esterro_sul,dmyerro_sul))
a<- a %>% mutate(
  provatrig = esttrend_sul+estsaz_sul+estirr_sul+esterro_sul,
  provadmy = dmytrend_sul+dmysaz_sul+dmyirr_sul+dmyerro_sul
)

### 04 - TRIÂNGULO MINEIRO #####################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/04_mod_trg.Rdata", envir = env1)
load("D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/04_mod_trg.Rdata", envir = env2)

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
trg<-baseestr8reg$`04-Triângulo Mineiro`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dttrg<-baseal8reg$`04-Triângulo Mineiro` 
dbtrg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/04_params_trg.RDS") 

desoc_trg <- trg$Total.de.desocupados/1000
se_db <- trg$sd_d/1000
cv_trg <- se_db/desoc_trg
ICinf_trg<-desoc_trg-1.96*se_db
ICsup_trg<-desoc_trg+1.96*se_db

desoc_trg <- window(ts.union(ts(desoc_trg, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_trg <- window(ts.union(ts(ICinf_trg, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_trg <- window(ts.union(ts(ICsup_trg, start = 2012, frequency = 4)), start = c(2013,4))
cv_trg <- window(ts.union(ts(cv_trg, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_trg <- env1$ma1_trg$ts.signal
estsinal_trg <- window(ts.union(ts(estsinal_trg, start = 2012, frequency = 4)), start = c(2013,4))
dmysinal_trg <- env2$ma1_trg$ts.signal
dmysinal_trg <- window(ts.union(ts(dmysinal_trg, start = 2012, frequency = 4)), start = c(2013,4))

cv_estsinal_trg <- env1$ma1_trg$cv.signal
cv_estsinal_trg <- window(ts.union(ts(cv_estsinal_trg, start = 2012, frequency = 4)), start = c(2013,4))
cv_dmysinal_trg <- env2$ma1_trg$cv.signal
cv_dmysinal_trg <- window(ts.union(ts(cv_dmysinal_trg, start = 2012, frequency = 4)), start = c(2013,4))

esttrend_trg <- env1$ma1_trg$ts.trend
esttrend_trg <- window(ts.union(ts(esttrend_trg, start = 2012, frequency = 4)), start = c(2013,4))
dmytrend_trg <- env2$ma1_trg$ts.trend
dmytrend_trg <- window(ts.union(ts(dmytrend_trg, start = 2012, frequency = 4)), start = c(2013,4))

estsaz_trg <- env1$ma1_trg$ts.seasonal
estsaz_trg <- window(ts.union(ts(estsaz_trg, start = 2012, frequency = 4)), start = c(2013,4))
dmysaz_trg <- env2$ma1_trg$ts.seasonal
dmysaz_trg <- window(ts.union(ts(dmysaz_trg, start = 2012, frequency = 4)), start = c(2013,4))

estirr_trg <- env1$ma1_trg$ts.irregular
estirr_trg <- window(ts.union(ts(estirr_trg, start = 2012, frequency = 4)), start = c(2013,4))
dmyirr_trg <- env2$ma1_trg$ts.irregular
dmyirr_trg <- window(ts.union(ts(dmyirr_trg, start = 2012, frequency = 4)), start = c(2013,4))

esterro_trg <- env1$ma1_trg$ts.sampling_error
esterro_trg <- window(ts.union(ts(esterro_trg, start = 2012, frequency = 4)), start = c(2013,4))
dmyerro_trg <- env2$ma1_trg$ts.sampling_error
dmyerro_trg <- window(ts.union(ts(dmyerro_trg, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_trg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(35,210))
lines(estsinal_trg, col = "red", lty = 1, lwd = 2)
lines(dmysinal_trg, col = "blue", lty = 1, lwd = 2)
lines(ICinf_trg, col = "black", lty = 2)
lines(ICsup_trg, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Trigonométrica",
                             "Sinal da Desocupação MA(1) - Dummy","IC 95%: signal-based"),
       col = c("black","red", "blue","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_trg*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(4,20))
lines(cv_estsinal_trg, col = "red",lwd=2,lty = 1)
lines(cv_dmysinal_trg, col = "blue",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV - trigonométrico","Sinal CV - dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("04 - Triângulo Mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(desoc_trg, type = "l", col = "black", lwd = 2,
     main = "Tendência: signal-based",
     xlab = "Ano", ylab = "Desocupados")
lines(esttrend_trg, col = "red", lty = 1, lwd = 2)
lines(dmytrend_trg, col = "blue", lty = 1, lwd = 2)
legend("bottom", legend = c("Desocupação: design-based", "Trigonométrica","Dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)

plot(estsaz_trg, type = "l", col = "red", lwd = 2,
     main = "Sazonalidade: signal-based",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-20,20))
lines(dmysaz_trg, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(estirr_trg, type = "l", col = "red", lwd = 2,
     main = "Termo irregular",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-25,25))
lines(dmyirr_trg, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(esterro_trg, type = "l", col = "red", lwd = 2,
     main = "Erro amostral",
     xlab = "Ano", ylab = "Desocupados")
lines(dmyerro_trg, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("04 - Triângulo Mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 05 - ZONA DA MATA ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/05_mod_mat.Rdata", envir = env1)
load("D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/05_mod_mat.Rdata", envir = env2)

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
mat<-baseestr8reg$`05-Mata de Minas Gerais`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtmat<-baseal8reg$`05-Mata de Minas Gerais`
dbmat<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/05_params_mat.RDS")

desoc_mat<- mat$Total.de.desocupados/1000
se_db <- mat$sd_d/1000
cv_mat <- se_db/desoc_mat
ICinf_mat<-desoc_mat-1.96*se_db
ICsup_mat<-desoc_mat+1.96*se_db

desoc_mat <- window(ts.union(ts(desoc_mat, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_mat <- window(ts.union(ts(ICinf_mat, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_mat <- window(ts.union(ts(ICsup_mat, start = 2012, frequency = 4)), start = c(2013,4))
cv_mat <- window(ts.union(ts(cv_mat, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_mat <- env1$ma1_mat$ts.signal
estsinal_mat <- window(ts.union(ts(estsinal_mat, start = 2012, frequency = 4)), start = c(2013,4))
dmysinal_mat <- env2$ma1_mat$ts.signal
dmysinal_mat <- window(ts.union(ts(dmysinal_mat, start = 2012, frequency = 4)), start = c(2013,4))

cv_estsinal_mat <- env1$ma1_mat$cv.signal
cv_estsinal_mat <- window(ts.union(ts(cv_estsinal_mat, start = 2012, frequency = 4)), start = c(2013,4))
cv_dmysinal_mat <- env2$ma1_mat$cv.signal
cv_dmysinal_mat <- window(ts.union(ts(cv_dmysinal_mat, start = 2012, frequency = 4)), start = c(2013,4))

esttrend_mat <- env1$ma1_mat$ts.trend
esttrend_mat <- window(ts.union(ts(esttrend_mat, start = 2012, frequency = 4)), start = c(2013,4))
dmytrend_mat <- env2$ma1_mat$ts.trend
dmytrend_mat <- window(ts.union(ts(dmytrend_mat, start = 2012, frequency = 4)), start = c(2013,4))

estsaz_mat <- env1$ma1_mat$ts.seasonal
estsaz_mat <- window(ts.union(ts(estsaz_mat, start = 2012, frequency = 4)), start = c(2013,4))
dmysaz_mat <- env2$ma1_mat$ts.seasonal
dmysaz_mat <- window(ts.union(ts(dmysaz_mat, start = 2012, frequency = 4)), start = c(2013,4))

estirr_mat <- env1$ma1_mat$ts.irregular
estirr_mat <- window(ts.union(ts(estirr_mat, start = 2012, frequency = 4)), start = c(2013,4))
dmyirr_mat <- env2$ma1_mat$ts.irregular
dmyirr_mat <- window(ts.union(ts(dmyirr_mat, start = 2012, frequency = 4)), start = c(2013,4))

esterro_mat <- env1$ma1_mat$ts.sampling_error
esterro_mat <- window(ts.union(ts(esterro_mat, start = 2012, frequency = 4)), start = c(2013,4))
dmyerro_mat <- env2$ma1_mat$ts.sampling_error
dmyerro_mat <- window(ts.union(ts(dmyerro_mat, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_mat, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(30,200))
lines(estsinal_mat, col = "red", lty = 1, lwd = 2)
lines(dmysinal_mat, col = "blue", lty = 1, lwd = 2)
lines(ICinf_mat, col = "black", lty = 2)
lines(ICsup_mat, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Trigonométrica",
                             "Sinal da Desocupação MA(1) - Dummy","IC 95%: signal-based"),
       col = c("black","red", "blue","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_mat*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(4,20))
lines(cv_estsinal_mat, col = "red",lwd=2,lty = 1)
lines(cv_dmysinal_mat, col = "blue",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV - trigonométrico","Sinal CV - dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("05 - Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(desoc_mat, type = "l", col = "black", lwd = 2,
     main = "Tendência: signal-based",
     xlab = "Ano", ylab = "Desocupados")
lines(esttrend_mat, col = "red", lty = 1, lwd = 2)
lines(dmytrend_mat, col = "blue", lty = 1, lwd = 2)
legend("bottom", legend = c("Desocupação: design-based", "Trigonométrica","Dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)

plot(estsaz_mat, type = "l", col = "red", lwd = 2,
     main = "Sazonalidade: signal-based",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-25,20))
lines(dmysaz_mat, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(estirr_mat, type = "l", col = "red", lwd = 2,
     main = "Termo irregular",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-20,20))
lines(dmyirr_mat, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(esterro_mat, type = "l", col = "red", lwd = 2,
     main = "Erro amostral",
     xlab = "Ano", ylab = "Desocupados")
lines(dmyerro_mat, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("05 - Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### 06 - NORTE DE MINAS ########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/06_mod_nrt.Rdata", envir = env1)
load("D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/06_mod_nrt.Rdata", envir = env2)

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
nrt<-baseestr8reg$`06-Norte de Minas`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtnrt<-baseal8reg$`06-Norte de Minas`
dbnrt<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/06_params_nrt.RDS") 

desoc_nrt <- nrt$Total.de.desocupados/1000
se_db <- nrt$sd_d/1000
cv_nrt <- se_db/desoc_nrt
ICinf_nrt<-desoc_nrt-1.96*se_db
ICsup_nrt<-desoc_nrt+1.96*se_db

desoc_nrt <- window(ts.union(ts(desoc_nrt, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_nrt <- window(ts.union(ts(ICinf_nrt, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_nrt <- window(ts.union(ts(ICsup_nrt, start = 2012, frequency = 4)), start = c(2013,4))
cv_nrt <- window(ts.union(ts(cv_nrt, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_nrt <- env1$ma1_nrt$ts.signal
estsinal_nrt <- window(ts.union(ts(estsinal_nrt, start = 2012, frequency = 4)), start = c(2013,4))
dmysinal_nrt <- env2$ma1_nrt$ts.signal
dmysinal_nrt <- window(ts.union(ts(dmysinal_nrt, start = 2012, frequency = 4)), start = c(2013,4))

cv_estsinal_nrt <- env1$ma1_nrt$cv.signal
cv_estsinal_nrt <- window(ts.union(ts(cv_estsinal_nrt, start = 2012, frequency = 4)), start = c(2013,4))
cv_dmysinal_nrt <- env2$ma1_nrt$cv.signal
cv_dmysinal_nrt <- window(ts.union(ts(cv_dmysinal_nrt, start = 2012, frequency = 4)), start = c(2013,4))

esttrend_nrt <- env1$ma1_nrt$ts.trend
esttrend_nrt <- window(ts.union(ts(esttrend_nrt, start = 2012, frequency = 4)), start = c(2013,4))
dmytrend_nrt <- env2$ma1_nrt$ts.trend
dmytrend_nrt <- window(ts.union(ts(dmytrend_nrt, start = 2012, frequency = 4)), start = c(2013,4))

estsaz_nrt <- env1$ma1_nrt$ts.seasonal
estsaz_nrt <- window(ts.union(ts(estsaz_nrt, start = 2012, frequency = 4)), start = c(2013,4))
dmysaz_nrt <- env2$ma1_nrt$ts.seasonal
dmysaz_nrt <- window(ts.union(ts(dmysaz_nrt, start = 2012, frequency = 4)), start = c(2013,4))

estirr_nrt <- env1$ma1_nrt$ts.irregular
estirr_nrt <- window(ts.union(ts(estirr_nrt, start = 2012, frequency = 4)), start = c(2013,4))
dmyirr_nrt <- env2$ma1_nrt$ts.irregular
dmyirr_nrt <- window(ts.union(ts(dmyirr_nrt, start = 2012, frequency = 4)), start = c(2013,4))

esterro_nrt <- env1$ma1_nrt$ts.sampling_error
esterro_nrt <- window(ts.union(ts(esterro_nrt, start = 2012, frequency = 4)), start = c(2013,4))
dmyerro_nrt <- env2$ma1_nrt$ts.sampling_error
dmyerro_nrt <- window(ts.union(ts(dmyerro_nrt, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_nrt, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(45,275))
lines(estsinal_nrt, col = "red", lty = 1, lwd = 2)
lines(dmysinal_nrt, col = "blue", lty = 1, lwd = 2)
lines(ICinf_nrt, col = "black", lty = 2)
lines(ICsup_nrt, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Trigonométrica",
                             "Sinal da Desocupação MA(1) - Dummy","IC 95%: signal-based"),
       col = c("black","red", "blue","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_nrt*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(2.5,23))
lines(cv_estsinal_nrt, col = "red",lwd=2,lty = 1)
lines(cv_dmysinal_nrt, col = "blue",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV - trigonométrico","Sinal CV - dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(desoc_nrt, type = "l", col = "black", lwd = 2,
     main = "Tendência: signal-based",
     xlab = "Ano", ylab = "Desocupados")
lines(esttrend_nrt, col = "red", lty = 1, lwd = 2)
lines(dmytrend_nrt, col = "blue", lty = 1, lwd = 2)
legend("bottom", legend = c("Desocupação: design-based", "Trigonométrica","Dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)

plot(estsaz_nrt, type = "l", col = "red", lwd = 2,
     main = "Sazonalidade: signal-based",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-25,25))
lines(dmysaz_nrt, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(estirr_nrt, type = "l", col = "red", lwd = 2,
     main = "Termo irregular",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-25,25))
lines(dmyirr_nrt, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(esterro_nrt, type = "l", col = "red", lwd = 2,
     main = "Erro amostral",
     xlab = "Ano", ylab = "Desocupados")
lines(dmyerro_nrt, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### 07 - VALE DO RIO DOCE ######################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/07_mod_val.Rdata", envir = env1)
load("D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/07_mod_val.Rdata", envir = env2)

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

desoc_val <- window(ts.union(ts(desoc_val, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_val <- window(ts.union(ts(ICinf_val, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_val <- window(ts.union(ts(ICsup_val, start = 2012, frequency = 4)), start = c(2013,4))
cv_val <- window(ts.union(ts(cv_val, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_val <- env1$ar1_val$ts.signal
estsinal_val <- window(ts.union(ts(estsinal_val, start = 2012, frequency = 4)), start = c(2013,4))
dmysinal_val <- env2$ar1_val$ts.signal
dmysinal_val <- window(ts.union(ts(dmysinal_val, start = 2012, frequency = 4)), start = c(2013,4))

cv_estsinal_val <- env1$ar1_val$cv.signal
cv_estsinal_val <- window(ts.union(ts(cv_estsinal_val, start = 2012, frequency = 4)), start = c(2013,4))
cv_dmysinal_val <- env2$ar1_val$cv.signal
cv_dmysinal_val <- window(ts.union(ts(cv_dmysinal_val, start = 2012, frequency = 4)), start = c(2013,4))

esttrend_val <- env1$ar1_val$ts.trend
esttrend_val <- window(ts.union(ts(esttrend_val, start = 2012, frequency = 4)), start = c(2013,4))
dmytrend_val <- env2$ar1_val$ts.trend
dmytrend_val <- window(ts.union(ts(dmytrend_val, start = 2012, frequency = 4)), start = c(2013,4))

estsaz_val <- env1$ar1_val$ts.seasonal
estsaz_val <- window(ts.union(ts(estsaz_val, start = 2012, frequency = 4)), start = c(2013,4))
dmysaz_val <- env2$ar1_val$ts.seasonal
dmysaz_val <- window(ts.union(ts(dmysaz_val, start = 2012, frequency = 4)), start = c(2013,4))

estirr_val <- env1$ar1_val$ts.irregular
estirr_val <- window(ts.union(ts(estirr_val, start = 2012, frequency = 4)), start = c(2013,4))
dmyirr_val <- env2$ar1_val$ts.irregular
dmyirr_val <- window(ts.union(ts(dmyirr_val, start = 2012, frequency = 4)), start = c(2013,4))

esterro_val <- env1$ar1_val$ts.sampling_error
esterro_val <- window(ts.union(ts(esterro_val, start = 2012, frequency = 4)), start = c(2013,4))
dmyerro_val <- env2$ar1_val$ts.sampling_error
dmyerro_val <- window(ts.union(ts(dmyerro_val, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_val, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(25,225))
lines(estsinal_val, col = "red", lty = 1, lwd = 2)
lines(dmysinal_val, col = "blue", lty = 1, lwd = 2)
lines(ICinf_val, col = "black", lty = 2)
lines(ICsup_val, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Trigonométrica",
                             "Sinal da Desocupação MA(1) - Dummy","IC 95%: signal-based"),
       col = c("black","red", "blue","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_val*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(3,17))
lines(cv_estsinal_val, col = "red",lwd=2,lty = 1)
lines(cv_dmysinal_val, col = "blue",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV - trigonométrico","Sinal CV - dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("07 - Vale do Rio Doce", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(desoc_val, type = "l", col = "black", lwd = 2,
     main = "Tendência: signal-based",
     xlab = "Ano", ylab = "Desocupados")
lines(esttrend_val, col = "red", lty = 1, lwd = 2)
lines(dmytrend_val, col = "blue", lty = 1, lwd = 2)
legend("bottom", legend = c("Desocupação: design-based", "Trigonométrica","Dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)

plot(estsaz_val, type = "l", col = "red", lwd = 2,
     main = "Sazonalidade: signal-based",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-20,20))
lines(dmysaz_val, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(estirr_val, type = "l", col = "red", lwd = 2,
     main = "Termo irregular",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-18,18))
lines(dmyirr_val, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(esterro_val, type = "l", col = "red", lwd = 2,
     main = "Erro amostral",
     xlab = "Ano", ylab = "Desocupados")
lines(dmyerro_val, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("07 - Vale do Rio Doce", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 08 - CENTRAL ###############################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/08_mod_cen.Rdata", envir = env1)
load("D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/08_mod_cen.Rdata", envir = env2)


baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
cen<-baseestr8reg$`08-Central`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtcen<-baseal8reg$`08-Central`
dbcen<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/08_params_cen.RDS") 

desoc_cen <- cen$Total.de.desocupados/1000
se_db <- cen$sd_d/1000
cv_cen <- se_db/desoc_cen
ICinf_cen<-desoc_cen-1.96*se_db
ICsup_cen<-desoc_cen+1.96*se_db

desoc_cen <- window(ts.union(ts(desoc_cen, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_cen <- window(ts.union(ts(ICinf_cen, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_cen <- window(ts.union(ts(ICsup_cen, start = 2012, frequency = 4)), start = c(2013,4))
cv_cen <- window(ts.union(ts(cv_cen, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_cen <- env1$ma1_cen$ts.signal
estsinal_cen <- window(ts.union(ts(estsinal_cen, start = 2012, frequency = 4)), start = c(2013,4))
dmysinal_cen <- env2$ma1_cen$ts.signal
dmysinal_cen <- window(ts.union(ts(dmysinal_cen, start = 2012, frequency = 4)), start = c(2013,4))

cv_estsinal_cen <- env1$ma1_cen$cv.signal
cv_estsinal_cen <- window(ts.union(ts(cv_estsinal_cen, start = 2012, frequency = 4)), start = c(2013,4))
cv_dmysinal_cen <- env2$ma1_cen$cv.signal
cv_dmysinal_cen <- window(ts.union(ts(cv_dmysinal_cen, start = 2012, frequency = 4)), start = c(2013,4))

esttrend_cen <- env1$ma1_cen$ts.trend
esttrend_cen <- window(ts.union(ts(esttrend_cen, start = 2012, frequency = 4)), start = c(2013,4))
dmytrend_cen <- env2$ma1_cen$ts.trend
dmytrend_cen <- window(ts.union(ts(dmytrend_cen, start = 2012, frequency = 4)), start = c(2013,4))

estsaz_cen <- env1$ma1_cen$ts.seasonal
estsaz_cen <- window(ts.union(ts(estsaz_cen, start = 2012, frequency = 4)), start = c(2013,4))
dmysaz_cen <- env2$ma1_cen$ts.seasonal
dmysaz_cen <- window(ts.union(ts(dmysaz_cen, start = 2012, frequency = 4)), start = c(2013,4))

estirr_cen <- env1$ma1_cen$ts.irregular
estirr_cen <- window(ts.union(ts(estirr_cen, start = 2012, frequency = 4)), start = c(2013,4))
dmyirr_cen <- env2$ma1_cen$ts.irregular
dmyirr_cen <- window(ts.union(ts(dmyirr_cen, start = 2012, frequency = 4)), start = c(2013,4))

esterro_cen <- env1$ma1_cen$ts.sampling_error
esterro_cen <- window(ts.union(ts(esterro_cen, start = 2012, frequency = 4)), start = c(2013,4))
dmyerro_cen <- env2$ma1_cen$ts.sampling_error
dmyerro_cen <- window(ts.union(ts(dmyerro_cen, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_cen, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(25,220))
lines(estsinal_cen, col = "red", lty = 1, lwd = 2)
lines(dmysinal_cen, col = "blue", lty = 1, lwd = 2)
lines(ICinf_cen, col = "black", lty = 2)
lines(ICsup_cen, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Trigonométrica",
                             "Sinal da Desocupação MA(1) - Dummy","IC 95%: signal-based"),
       col = c("black","red", "blue","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_cen*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(2,20))
lines(cv_estsinal_cen, col = "red",lwd=2,lty = 1)
lines(cv_dmysinal_cen, col = "blue",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV - trigonométrico","Sinal CV - dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("08 - Central", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(desoc_cen, type = "l", col = "black", lwd = 2,
     main = "Tendência: signal-based",
     xlab = "Ano", ylab = "Desocupados")
lines(esttrend_cen, col = "red", lty = 1, lwd = 2)
lines(dmytrend_cen, col = "blue", lty = 1, lwd = 2)
legend("bottom", legend = c("Desocupação: design-based", "Trigonométrica","Dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)

plot(estsaz_cen, type = "l", col = "red", lwd = 2,
     main = "Sazonalidade: signal-based",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-27,27))
lines(dmysaz_cen, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(estirr_cen, type = "l", col = "red", lwd = 2,
     main = "Termo irregular",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-25,25))
lines(dmyirr_cen, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(esterro_cen, type = "l", col = "red", lwd = 2,
     main = "Erro amostral",
     xlab = "Ano", ylab = "Desocupados")
lines(dmyerro_cen, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("08 - Central", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### 09 - MINAS GERAIS ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/09_mod_mg.Rdata", envir = env1)
load("D:/FJP2425/Programacao/data/Rdatas/10_estdummy - desoc_8reg/09_mod_mg.Rdata", envir = env2)

baseestr8reg <- readRDS("D:/FJP2425/Programacao/data/baseestr8reg.RDS")
mg<-baseestr8reg$`09 - Minas Gerais`
baseal8reg<- readRDS("D:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtmg<-baseal8reg$`09 - Minas Gerais` 
dbmg<-readRDS("D:/FJP2425/Programacao/data/pseudoerros_8reg/09_params_mg.RDS")

desoc_mg <- mg$Total.de.desocupados/1000
se_db <- mg$sd_d/1000
cv_mg <- se_db/desoc_mg
ICinf_mg<-desoc_mg-1.96*se_db
ICsup_mg<-desoc_mg+1.96*se_db

desoc_mg <- window(ts.union(ts(desoc_mg, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_mg <- window(ts.union(ts(ICinf_mg, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_mg <- window(ts.union(ts(ICsup_mg, start = 2012, frequency = 4)), start = c(2013,4))
cv_mg <- window(ts.union(ts(cv_mg, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_mg <- env1$ma1_mg$ts.signal
estsinal_mg <- window(ts.union(ts(estsinal_mg, start = 2012, frequency = 4)), start = c(2013,4))
dmysinal_mg <- env2$ma1_mg$ts.signal
dmysinal_mg <- window(ts.union(ts(dmysinal_mg, start = 2012, frequency = 4)), start = c(2013,4))

cv_estsinal_mg <- env1$ma1_mg$cv.signal
cv_estsinal_mg <- window(ts.union(ts(cv_estsinal_mg, start = 2012, frequency = 4)), start = c(2013,4))
cv_dmysinal_mg <- env2$ma1_mg$cv.signal
cv_dmysinal_mg <- window(ts.union(ts(cv_dmysinal_mg, start = 2012, frequency = 4)), start = c(2013,4))

esttrend_mg <- env1$ma1_mg$ts.trend
esttrend_mg <- window(ts.union(ts(esttrend_mg, start = 2012, frequency = 4)), start = c(2013,4))
dmytrend_mg <- env2$ma1_mg$ts.trend
dmytrend_mg <- window(ts.union(ts(dmytrend_mg, start = 2012, frequency = 4)), start = c(2013,4))

estsaz_mg <- env1$ma1_mg$ts.seasonal
estsaz_mg <- window(ts.union(ts(estsaz_mg, start = 2012, frequency = 4)), start = c(2013,4))
dmysaz_mg <- env2$ma1_mg$ts.seasonal
dmysaz_mg <- window(ts.union(ts(dmysaz_mg, start = 2012, frequency = 4)), start = c(2013,4))

estirr_mg <- env1$ma1_mg$ts.irregular
estirr_mg <- window(ts.union(ts(estirr_mg, start = 2012, frequency = 4)), start = c(2013,4))
dmyirr_mg <- env2$ma1_mg$ts.irregular
dmyirr_mg <- window(ts.union(ts(dmyirr_mg, start = 2012, frequency = 4)), start = c(2013,4))

esterro_mg <- env1$ma1_mg$ts.sampling_error
esterro_mg <- window(ts.union(ts(esterro_mg, start = 2012, frequency = 4)), start = c(2013,4))
dmyerro_mg <- env2$ma1_mg$ts.sampling_error
dmyerro_mg <- window(ts.union(ts(dmyerro_mg, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_mg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(400,1700))
lines(estsinal_mg, col = "red", lty = 1, lwd = 2)
lines(dmysinal_mg, col = "blue", lty = 1, lwd = 2)
lines(ICinf_mg, col = "black", lty = 2)
lines(ICsup_mg, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Trigonométrica",
                             "Sinal da Desocupação MA(1) - Dummy","IC 95%: signal-based"),
       col = c("black","red", "blue","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_mg*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(1.5,6))
lines(cv_estsinal_mg, col = "red",lwd=2,lty = 1)
lines(cv_dmysinal_mg, col = "blue",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV - trigonométrico","Sinal CV - dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("09 - Minas Gerais", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(desoc_mg, type = "l", col = "black", lwd = 2,
     main = "Tendência: signal-based",
     xlab = "Ano", ylab = "Desocupados")
lines(esttrend_mg, col = "red", lty = 1, lwd = 2)
lines(dmytrend_mg, col = "blue", lty = 1, lwd = 2)
legend("bottom", legend = c("Desocupação: design-based", "Trigonométrica","Dummy"),
       col = c("black","red", "blue"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)

plot(estsaz_mg, type = "l", col = "red", lwd = 2,
     main = "Sazonalidade: signal-based",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-120,150))
lines(dmysaz_mg, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(estirr_mg, type = "l", col = "red", lwd = 2,
     main = "Termo irregular",
     xlab = "Ano", ylab = "Desocupados", ylim=c(-150,120))
lines(dmyirr_mg, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)

plot(esterro_mg, type = "l", col = "red", lwd = 2,
     main = "Erro amostral",
     xlab = "Ano", ylab = "Desocupados")
lines(dmyerro_mg, col = "blue", lty = 1, lwd = 2)
legend("bottomleft", legend = c("Trigonométrica", "Dummy"),
       col = c("red","blue"),lty = c(1,1),lwd = c(2,2),bty = "n", cex=0.8)
mtext("09 - Minas Gerais", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

