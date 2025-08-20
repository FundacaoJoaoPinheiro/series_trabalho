################################################################################
## COMPARANDO RESULTADOS DO MODELO MULTIVARIADO COM MODELO UNIVARIADO - OCUP ##
################################################################################

## Para visualizar os gráficos em segunda tela:

dev.new()
dev.new()


### 01 - BELO HORIZONTE ########################################################
rm(list = ls())
gc()

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/01_mod_bh.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/13_multivariado_semcorrelacao - ocup_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
bh<-baseestr8reg$`01-Belo Horizonte`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtbh<-baseal8reg$`01-Belo Horizonte` 
dbbh<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/01_params_bh.RDS")

ocup_bh <- bh$Total.de.ocupados/1000
se_db<- bh$sd_o/1000
cv_bh <- se_db/ocup_bh
ICinf_bh<-ocup_bh-1.96*se_db
ICsup_bh<-ocup_bh+1.96*se_db

ocup_bh <- window(ts.union(ts(ocup_bh, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_bh <- window(ts.union(ts(ICinf_bh, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_bh <- window(ts.union(ts(ICsup_bh, start = 2012, frequency = 4)), start = c(2013,3))
cv_bh <- window(ts.union(ts(cv_bh, start = 2012, frequency = 4)), start = c(2013,3))

estsinal_ar1bh <- env1$ar1_bh$ts.signal
estsinal_ar1bh <- window(ts.union(ts(estsinal_ar1bh, start = 2012, frequency = 4)), start = c(2013,3))
estcv_ar1bh <- env1$ar1_bh$cv.signal
estcv_ar1bh <- window(ts.union(ts(estcv_ar1bh, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_semcorr_bh <- env2$modelo_mult_sem_corr$ts.signal_1
multsinal_semcorr_bh <- window(ts.union(ts(multsinal_semcorr_bh, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_semcorr_bh <- env2$modelo_mult_sem_corr$cv.signal_1
cv_multsinal_semcorr_bh <- window(ts.union(ts(cv_multsinal_semcorr_bh, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_bh <- env3$modelo_mult$ts.signal_1
multsinal_bh <- window(ts.union(ts(multsinal_bh, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_bh <- env3$modelo_mult$cv.signal_1
cv_multsinal_bh <- window(ts.union(ts(cv_multsinal_bh, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1bh <- env1$ar1_bh$ts.trend
esttrend_ar1bh <- window(ts.union(ts(esttrend_ar1bh, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_bh <- env1$ar1_bh$cv.trend
cv_esttrend_bh <- window(ts.union(ts(cv_esttrend_bh, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_semcorr_bh <- env2$modelo_mult_sem_corr$ts.trend_1
multtrend_semcorr_bh <- window(ts.union(ts(multtrend_semcorr_bh, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_semcorr_bh <- env2$modelo_mult_sem_corr$cv.trend_1
cv_multtrend_semcorr_bh <- window(ts.union(ts(cv_multtrend_semcorr_bh, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_bh <- env3$modelo_mult$ts.trend_1
multtrend_bh <- window(ts.union(ts(multtrend_bh, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_bh <- env3$modelo_mult$cv.trend_1
cv_multtrend_bh <- window(ts.union(ts(cv_multtrend_bh, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_bh, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(1000,1500))
lines(estsinal_ar1bh, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_bh, col= "red", lty = 1, lwd = 2)
lines(multsinal_bh, col= "green", lty = 1, lwd = 2)
lines(ICinf_bh, col = "black", lty = 2)
lines(ICsup_bh, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Sinal univariado - AR(1)","Sinal multivariado sem corr. - AR(1)", 
                             "Sinal multivariado com corr - AR(1)", "IC 95% - estimativa direta"), 
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_bh*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(1,3))
lines(estcv_ar1bh, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_bh, col = "red",lwd=2,lty = 1)
lines(cv_multsinal_bh, col = "green", lty = 1, lwd = 2)
legend("topleft", legend = c("CV ocupados","CV sinal - univariado", "CV sinal - multivariado sem corr.", "CV sinal - multivariado com corr."), 
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("01 - Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


## Gráfico das tendências

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_bh, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(1000,1500))
lines(esttrend_ar1bh, col = "blue", lty = 1, lwd = 2)
lines(multtrend_semcorr_bh, col= "red", lty = 1, lwd = 2)
lines(multtrend_bh, col= "green", lty = 1, lwd = 2)
lines(ICinf_bh, col = "black", lty = 2)
lines(ICsup_bh, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Tendência univariado - AR(1)","Tendência multivariado sem corr. - AR(1)", 
                             "Tendência multivariado com corr - AR(1)", "IC 95% - estimativa direta"), 
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_bh*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(1,3))
lines(cv_esttrend_bh, col = "blue",lwd=2, lty = 1)
lines(cv_multtrend_semcorr_bh, col = "red",lwd=2,lty = 1)
lines(cv_multtrend_bh, col = "green", lty = 1, lwd = 2)
legend("topleft", legend = c("CV ocupados","CV tendência - univariado", "CV tendência - multivariado sem corr.", "CV tendência - multivariado com corr."), 
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("01 - Belo Horizonte (tendências)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 02 - COLAR E ENTORNO METROPOLITANO DE BELO HORIZONTE #######################
rm(list = ls())

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/02_mod_ent.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/13_multivariado_semcorrelacao - ocup_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
ent<-baseestr8reg$`02-Colar e Entorno metropolitano de BH`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtent<-baseal8reg$`02-Colar e Entorno Metropolitano de BH`
dbent<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/02_params_ent.RDS")

ocup_ent <- ent$Total.de.ocupados/1000
se_db<- ent$sd_o/1000
cv_ent <- se_db/ocup_ent
ICinf_ent<-ocup_ent-1.96*se_db
ICsup_ent<-ocup_ent+1.96*se_db

ocup_ent <- window(ts.union(ts(ocup_ent, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_ent <- window(ts.union(ts(ICinf_ent, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_ent <- window(ts.union(ts(ICsup_ent, start = 2012, frequency = 4)), start = c(2013,3))
cv_ent <- window(ts.union(ts(cv_ent, start = 2012, frequency = 4)), start = c(2013,3))

estsinal_ar1ent <- env1$ar1_ent$ts.signal
estsinal_ar1ent <- window(ts.union(ts(estsinal_ar1ent, start = 2012, frequency = 4)), start = c(2013,3))
estcv_ar1ent <- env1$ar1_ent$cv.signal
estcv_ar1ent <- window(ts.union(ts(estcv_ar1ent, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_semcorr_ent <- env2$modelo_mult_sem_corr$ts.signal_2
multsinal_semcorr_ent <- window(ts.union(ts(multsinal_semcorr_ent, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_semcorr_ent <- env2$modelo_mult_sem_corr$cv.signal_2
cv_multsinal_semcorr_ent <- window(ts.union(ts(cv_multsinal_semcorr_ent, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_ent <- env3$modelo_mult$ts.signal_2
multsinal_ent <- window(ts.union(ts(multsinal_ent, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_ent <- env3$modelo_mult$cv.signal_2
cv_multsinal_ent <- window(ts.union(ts(cv_multsinal_ent, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1ent <- env1$ar1_ent$ts.trend
esttrend_ar1ent <- window(ts.union(ts(esttrend_ar1ent, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_ent <- env1$ar1_ent$cv.trend
cv_esttrend_ent <- window(ts.union(ts(cv_esttrend_ent, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_semcorr_ent <- env2$modelo_mult_sem_corr$ts.trend_2
multtrend_semcorr_ent <- window(ts.union(ts(multtrend_semcorr_ent, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_semcorr_ent <- env2$modelo_mult_sem_corr$cv.trend_2
cv_multtrend_semcorr_ent <- window(ts.union(ts(cv_multtrend_semcorr_ent, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_ent <- env3$modelo_mult$ts.trend_2
multtrend_ent <- window(ts.union(ts(multtrend_ent, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_ent <- env3$modelo_mult$cv.trend_2
cv_multtrend_ent <- window(ts.union(ts(cv_multtrend_ent, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(4, 0, 2, 0), cex = 0.8)
plot(ocup_ent, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(1300,2100))
lines(estsinal_ar1ent, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_ent, col= "red", lty = 1, lwd = 2)
lines(multsinal_ent, col = "green", lty = 1, lwd = 2)
lines(ICinf_ent, col = "black", lty = 2)
lines(ICsup_ent, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Sinal univariado - AR(1)","Sinal multivariado sem corr. - AR(1)", 
                             "Sinal multivariado com corr - AR(1)", "IC 95% - estimativa direta"), 
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_ent*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(0.5,3))
lines(estcv_ar1ent, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_ent, col = "red",lwd=2,lty = 1)
lines(cv_multsinal_ent, col = "green",lwd=2,lty = 1)
legend("topleft", legend = c("CV ocupados","CV sinal - univariado", "CV sinal - multivariado sem corr.", "CV sinal - multivariado com corr."),
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("02 - Entorno metropolitano de Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Tendências:

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(4, 0, 2, 0), cex = 0.8)
plot(ocup_ent, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(1300,2100))
lines(esttrend_ar1ent, col = "blue", lty = 1, lwd = 2)
lines(multtrend_semcorr_ent, col= "red", lty = 1, lwd = 2)
lines(multtrend_ent, col = "green", lty = 1, lwd = 2)
lines(ICinf_ent, col = "black", lty = 2)
lines(ICsup_ent, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Tendência univariado - AR(1)","Tendência multivariado sem corr. - AR(1)", 
                             "Tendência multivariado com corr - AR(1)", "IC 95% - estimativa direta"), 
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_ent*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(0.5,3))
lines(cv_esttrend_ent, col = "blue",lwd=2, lty = 1)
lines(cv_multtrend_semcorr_ent, col = "red",lwd=2,lty = 1)
lines(cv_multtrend_ent, col = "green",lwd=2,lty = 1)
legend("topleft", legend = c("CV ocupados","CV tendência - univariado", "CV tendência - multivariado sem corr.", "CV tendência - multivariado com corr."), 
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("02 - Colar e entorno metropolitano de Belo Horizonte (tendências)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### 03 - SUL DE MINAS ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/03_mod_sul.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/13_multivariado_semcorrelacao - ocup_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
sul<-baseestr8reg$`03-Sul de Minas`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtsul<-baseal8reg$`03-Sul de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbsul<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/03_params_sul.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

ocup_sul <- sul$Total.de.ocupados/1000
se_db<- sul$sd_o/1000
cv_sul <- se_db/ocup_sul
ICinf_sul<-ocup_sul-1.96*se_db
ICsup_sul<-ocup_sul+1.96*se_db

ocup_sul <- window(ts.union(ts(ocup_sul, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_sul <- window(ts.union(ts(ICinf_sul, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_sul <- window(ts.union(ts(ICsup_sul, start = 2012, frequency = 4)), start = c(2013,3))
cv_sul <- window(ts.union(ts(cv_sul, start = 2012, frequency = 4)), start = c(2013,3))

estsinal_ar1sul <- env1$ar1_sul$ts.signal
estsinal_ar1sul <- window(ts.union(ts(estsinal_ar1sul, start = 2012, frequency = 4)), start = c(2013,3))
estcv_ar1sul <- env1$ar1_sul$cv.signal
estcv_ar1sul <- window(ts.union(ts(estcv_ar1sul, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_semcorr_sul <- env2$modelo_mult_sem_corr$ts.signal_3
multsinal_semcorr_sul <- window(ts.union(ts(multsinal_semcorr_sul, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_semcorr_sul <- env2$modelo_mult_sem_corr$cv.signal_3
cv_multsinal_semcorr_sul <- window(ts.union(ts(cv_multsinal_semcorr_sul, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_sul <- env3$modelo_mult$ts.signal_3
multsinal_sul <- window(ts.union(ts(multsinal_sul, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_sul <- env3$modelo_mult$cv.signal_3
cv_multsinal_sul <- window(ts.union(ts(cv_multsinal_sul, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1sul <- env1$ar1_sul$ts.trend
esttrend_ar1sul <- window(ts.union(ts(esttrend_ar1sul, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_sul <- env1$ar1_sul$cv.trend
cv_esttrend_sul <- window(ts.union(ts(cv_esttrend_sul, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_semcorr_sul <- env2$modelo_mult_sem_corr$ts.trend_3
multtrend_semcorr_sul <- window(ts.union(ts(multtrend_semcorr_sul, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_semcorr_sul <- env2$modelo_mult_sem_corr$cv.trend_3
cv_multtrend_semcorr_sul <- window(ts.union(ts(cv_multtrend_semcorr_sul, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_sul <- env3$modelo_mult$ts.trend_3
multtrend_sul <- window(ts.union(ts(multtrend_sul, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_sul <- env3$modelo_mult$cv.trend_3
cv_multtrend_sul <- window(ts.union(ts(cv_multtrend_sul, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_sul, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(1000,1600))
lines(estsinal_ar1sul, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_sul, col= "red", lty = 1, lwd = 2)
lines(multsinal_sul, col = "green", lty = 1, lwd = 2)
lines(ICinf_sul, col = "black", lty = 2)
lines(ICsup_sul, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Sinal univariado - AR(1)","Sinal multivariado sem corr. - AR(1)", 
                             "Sinal multivariado com corr - AR(1)", "IC 95% - estimativa direta"), 
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_sul*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(1,8))
lines(estcv_ar1sul, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_sul, col = "red",lwd=2,lty = 1)
lines(cv_multsinal_sul, col = "green", lty = 1, lwd = 2)
legend("topleft", legend = c("CV ocupados","CV sinal - univariado", "CV sinal - multivariado sem corr.", "CV sinal - multivariado com corr."),
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("03 - Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Tendências:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_sul, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(1000,1600))
lines(esttrend_ar1sul, col = "blue", lty = 1, lwd = 2)
lines(multtrend_semcorr_sul, col= "red", lty = 1, lwd = 2)
lines(multtrend_sul, col = "green", lty = 1, lwd = 2)
lines(ICinf_sul, col = "black", lty = 2)
lines(ICsup_sul, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Tendência univariado - ARMA(1,1)","Tendência multivariado sem corr - ARMA(1,1)",
                             "Tendência multivariado com corr - ARMA(1,1)","IC 95% - estimativa direta"),
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_sul*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(1,8))
lines(cv_esttrend_sul, col = "blue",lwd=2, lty = 1)
lines(cv_multtrend_semcorr_sul, col = "red",lwd=2,lty = 1)
lines(cv_multtrend_sul, col = "green", lty = 1, lwd = 2)
legend("topleft", legend = c("CV ocupados","CV Tendência - univariado", "CV Tendência - multivariado sem corr.", "CV Tendência - multivariado com corr."),
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("03 - Sul de Minas (tendências)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### 04 - TRIÂNGULO MINEIRO #####################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/04_mod_trg.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/13_multivariado_semcorrelacao - ocup_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
trg<-baseestr8reg$`04-Triângulo Mineiro`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dttrg<-baseal8reg$`04-Triângulo Mineiro` 
dbtrg<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/04_params_trg.RDS") 

ocup_trg <- trg$Total.de.ocupados/1000
se_db<- trg$sd_o/1000
cv_trg <- se_db/ocup_trg
ICinf_trg<-ocup_trg-1.96*se_db
ICsup_trg<-ocup_trg+1.96*se_db

ocup_trg <- window(ts.union(ts(ocup_trg, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_trg <- window(ts.union(ts(ICinf_trg, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_trg <- window(ts.union(ts(ICsup_trg, start = 2012, frequency = 4)), start = c(2013,3))
cv_trg <- window(ts.union(ts(cv_trg, start = 2012, frequency = 4)), start = c(2013,3))

estsinal_ar1trg <- env1$ar1_trg$ts.signal
estsinal_ar1trg <- window(ts.union(ts(estsinal_ar1trg, start = 2012, frequency = 4)), start = c(2013,3))
estcv_ar1trg <- env1$ar1_trg$cv.signal
estcv_ar1trg <- window(ts.union(ts(estcv_ar1trg, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_semcorr_trg <- env2$modelo_mult_sem_corr$ts.signal_4
multsinal_semcorr_trg <- window(ts.union(ts(multsinal_semcorr_trg, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_semcorr_trg <- env2$modelo_mult_sem_corr$cv.signal_4
cv_multsinal_semcorr_trg <- window(ts.union(ts(cv_multsinal_semcorr_trg, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_trg <- env3$modelo_mult$ts.signal_4
multsinal_trg <- window(ts.union(ts(multsinal_trg, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_trg <- env3$modelo_mult$cv.signal_4
cv_multsinal_trg <- window(ts.union(ts(cv_multsinal_trg, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1trg <- env1$ar1_trg$ts.trend
esttrend_ar1trg <- window(ts.union(ts(esttrend_ar1trg, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_trg <- env1$ar1_trg$cv.trend
cv_esttrend_trg <- window(ts.union(ts(cv_esttrend_trg, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_semcorr_trg <- env2$modelo_mult_sem_corr$ts.trend_4
multtrend_semcorr_trg <- window(ts.union(ts(multtrend_semcorr_trg, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_semcorr_trg <- env2$modelo_mult_sem_corr$cv.trend_4
cv_multtrend_semcorr_trg <- window(ts.union(ts(cv_multtrend_semcorr_trg, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_trg <- env3$modelo_mult$ts.trend_4
multtrend_trg <- window(ts.union(ts(multtrend_trg, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_trg <- env3$modelo_mult$cv.trend_4
cv_multtrend_trg <- window(ts.union(ts(cv_multtrend_trg, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_trg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(1000,1850))
lines(estsinal_ar1trg, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_trg, col= "red", lty = 1, lwd = 2)
lines(multsinal_trg, col = "green", lty = 1, lwd = 2)
lines(ICinf_trg, col = "black", lty = 2)
lines(ICsup_trg, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Sinal univariado - AR(1)","Sinal multivariado sem corr. - AR(1)", 
                             "Sinal multivariado com corr - AR(1)", "IC 95% - estimativa direta"), 
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_trg*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(3,21))
lines(estcv_ar1trg, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_trg, col = "red",lwd=2,lty = 1)
lines(cv_multsinal_trg, col = "green", lty = 1, lwd = 2)
legend("topright", legend = c("CV ocupados","CV sinal - univariado", "CV sinal - multivariado sem corr.", "CV sinal - multivariado com corr."),
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("04 - Triângulo mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Tendências:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_trg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(1000,1850))
lines(esttrend_ar1trg, col = "blue", lty = 1, lwd = 2)
lines(multtrend_semcorr_trg, col= "red", lty = 1, lwd = 2)
lines(multtrend_trg, col = "green", lty = 1, lwd = 2)
lines(ICinf_trg, col = "black", lty = 2)
lines(ICsup_trg, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Tendência univariado - AR(1)","Tendência multivariado sem corr - AR(1)",
                             "Tendência multivariado com corr - AR(1)","IC 95% - estimativa direta"),
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_trg*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(3,21))
lines(cv_esttrend_trg, col = "blue",lwd=2, lty = 1)
lines(cv_multtrend_semcorr_trg, col = "red",lwd=2,lty = 1)
lines(cv_multtrend_trg, col = "green", lty = 1, lwd = 2)
legend("topright", legend = c("CV ocupados","CV tendência - univariado", "CV tendência - multivariado sem corr.", "CV tendência - multivariado com corr."),
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("04 - Triângulo mineiro (tendências)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### 05 - ZONA DA MATA ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/05_mod_mat.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/13_multivariado_semcorrelacao - ocup_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
mat<-baseestr8reg$`05-Mata de Minas Gerais`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtmat<-baseal8reg$`05-Mata de Minas Gerais`
dbmat<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/05_params_mat.RDS")

ocup_mat <- mat$Total.de.ocupados/1000
se_db<- mat$sd_o/1000
cv_mat <- se_db/ocup_mat
ICinf_mat<-ocup_mat-1.96*se_db
ICsup_mat<-ocup_mat+1.96*se_db

ocup_mat <- window(ts.union(ts(ocup_mat, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_mat <- window(ts.union(ts(ICinf_mat, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_mat <- window(ts.union(ts(ICsup_mat, start = 2012, frequency = 4)), start = c(2013,3))
cv_mat <- window(ts.union(ts(cv_mat, start = 2012, frequency = 4)), start = c(2013,3))

estsinal_ar1mat <- env1$ar1_mat$ts.signal
estsinal_ar1mat <- window(ts.union(ts(estsinal_ar1mat, start = 2012, frequency = 4)), start = c(2013,3))
estcv_ar1mat <- env1$ar1_mat$cv.signal
estcv_ar1mat <- window(ts.union(ts(estcv_ar1mat, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_semcorr_mat <- env2$modelo_mult_sem_corr$ts.signal_5
multsinal_semcorr_mat <- window(ts.union(ts(multsinal_semcorr_mat, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_semcorr_mat <- env2$modelo_mult_sem_corr$cv.signal_5
cv_multsinal_semcorr_mat <- window(ts.union(ts(cv_multsinal_semcorr_mat, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_mat <- env3$modelo_mult$ts.signal_5
multsinal_mat <- window(ts.union(ts(multsinal_mat, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_mat <- env3$modelo_mult$cv.signal_5
cv_multsinal_mat <- window(ts.union(ts(cv_multsinal_mat, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1mat <- env1$ar1_mat$ts.trend
esttrend_ar1mat <- window(ts.union(ts(esttrend_ar1mat, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_mat <- env1$ar1_mat$cv.trend
cv_esttrend_mat <- window(ts.union(ts(cv_esttrend_mat, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_semcorr_mat <- env2$modelo_mult_sem_corr$ts.trend_5
multtrend_semcorr_mat <- window(ts.union(ts(multtrend_semcorr_mat, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_semcorr_mat <- env2$modelo_mult_sem_corr$cv.trend_5
cv_multtrend_semcorr_mat <- window(ts.union(ts(cv_multtrend_semcorr_mat, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_mat <- env3$modelo_mult$ts.trend_5
multtrend_mat <- window(ts.union(ts(multtrend_mat, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_mat <- env3$modelo_mult$cv.trend_5
cv_multtrend_mat <- window(ts.union(ts(cv_multtrend_mat, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_mat, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(800,1400))
lines(estsinal_ar1mat, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_mat, col= "red", lty = 1, lwd = 2)
lines(multsinal_mat, col = "green", lty = 1, lwd = 2)
lines(ICinf_mat, col = "black", lty = 2)
lines(ICsup_mat, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Sinal univariado - AR(1)","Sinal multivariado sem corr. - AR(1)", 
                             "Sinal multivariado com corr - AR(1)", "IC 95% - estimativa direta"), 
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_mat*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(2,7))
lines(estcv_ar1mat, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_mat, col = "red",lwd=2,lty = 1)
lines(cv_multsinal_mat, col = "green", lty = 1, lwd = 2)
legend("topleft", legend = c("CV ocupados","CV sinal - univariado", "CV sinal - multivariado sem corr.", "CV sinal - multivariado com corr."),
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("05 - Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Tendências:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_mat, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(800,1400))
lines(esttrend_ar1mat, col = "blue", lty = 1, lwd = 2)
lines(multtrend_semcorr_mat, col= "red", lty = 1, lwd = 2)
lines(multtrend_mat, col = "green", lty = 1, lwd = 2)
lines(ICinf_mat, col = "black", lty = 2)
lines(ICsup_mat, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Tendência univariado - AR(1)","Tendência multivariado sem corr - AR(1)",
                             "Tendência multivariado com corr - AR(1)","IC 95% - estimativa direta"),
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_mat*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(2,7))
lines(cv_esttrend_mat, col = "blue",lwd=2, lty = 1)
lines(cv_multtrend_semcorr_mat, col = "red",lwd=2,lty = 1)
lines(cv_multtrend_mat, col = "green", lty = 1, lwd = 2)
legend("topleft", legend = c("CV ocupados","CV tendência - univariado", "CV tendência - multivariado sem corr.", "CV tendência - multivariado com corr."),
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("05 - Zona da Mata (tendências)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### 06 - NORTE DE MINAS ########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/06_mod_nrt.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/13_multivariado_semcorrelacao - ocup_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
nrt<-baseestr8reg$`06-Norte de Minas`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtnrt<-baseal8reg$`06-Norte de Minas`
dbnrt<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/06_params_nrt.RDS") 

ocup_nrt <- nrt$Total.de.ocupados/1000
se_db<- nrt$sd_o/1000
cv_nrt <- se_db/ocup_nrt
ICinf_nrt<-ocup_nrt-1.96*se_db
ICsup_nrt<-ocup_nrt+1.96*se_db

ocup_nrt <- window(ts.union(ts(ocup_nrt, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_nrt <- window(ts.union(ts(ICinf_nrt, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_nrt <- window(ts.union(ts(ICsup_nrt, start = 2012, frequency = 4)), start = c(2013,3))
cv_nrt <- window(ts.union(ts(cv_nrt, start = 2012, frequency = 4)), start = c(2013,3))

estsinal_ar1nrt <- env1$ar1_nrt$ts.signal
estsinal_ar1nrt <- window(ts.union(ts(estsinal_ar1nrt, start = 2012, frequency = 4)), start = c(2013,3))
estcv_ar1nrt <- env1$ar1_nrt$cv.signal
estcv_ar1nrt <- window(ts.union(ts(estcv_ar1nrt, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_semcorr_nrt <- env2$modelo_mult_sem_corr$ts.signal_6
multsinal_semcorr_nrt <- window(ts.union(ts(multsinal_semcorr_nrt, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_semcorr_nrt <- env2$modelo_mult_sem_corr$cv.signal_6
cv_multsinal_semcorr_nrt <- window(ts.union(ts(cv_multsinal_semcorr_nrt, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_nrt <- env3$modelo_mult$ts.signal_6
multsinal_nrt <- window(ts.union(ts(multsinal_nrt, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_nrt <- env3$modelo_mult$cv.signal_6
cv_multsinal_nrt <- window(ts.union(ts(cv_multsinal_nrt, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1nrt <- env1$ar1_nrt$ts.trend
esttrend_ar1nrt <- window(ts.union(ts(esttrend_ar1nrt, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_nrt <- env1$ar1_nrt$cv.trend
cv_esttrend_nrt <- window(ts.union(ts(cv_esttrend_nrt, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_semcorr_nrt <- env2$modelo_mult_sem_corr$ts.trend_6
multtrend_semcorr_nrt <- window(ts.union(ts(multtrend_semcorr_nrt, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_semcorr_nrt <- env2$modelo_mult_sem_corr$cv.trend_6
cv_multtrend_semcorr_nrt <- window(ts.union(ts(cv_multtrend_semcorr_nrt, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_nrt <- env3$modelo_mult$ts.trend_6
multtrend_nrt <- window(ts.union(ts(multtrend_nrt, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_nrt <- env3$modelo_mult$cv.trend_6
cv_multtrend_nrt <- window(ts.union(ts(cv_multtrend_nrt, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_nrt, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(800,1500))
lines(estsinal_ar1nrt, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_nrt, col= "red", lty = 1, lwd = 2)
lines(multsinal_nrt, col = "green", lty = 1, lwd = 2)
lines(ICinf_nrt, col = "black", lty = 2)
lines(ICsup_nrt, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Sinal univariado - AR(1)","Sinal multivariado sem corr. - AR(1)", 
                             "Sinal multivariado com corr - AR(1)", "IC 95% - estimativa direta"), 
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_nrt*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(2.5,7.5))
lines(estcv_ar1nrt, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_nrt, col = "red",lwd=2,lty = 1)
lines(cv_multsinal_nrt, col = "green", lty = 1, lwd = 2)
legend("topleft", legend = c("CV ocupados","CV sinal - univariado", "CV sinal - multivariado sem corr.", "CV sinal - multivariado com corr."),
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Tendências:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_nrt, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(800,1500))
lines(esttrend_ar1nrt, col = "blue", lty = 1, lwd = 2)
lines(multtrend_semcorr_nrt, col= "red", lty = 1, lwd = 2)
lines(multtrend_nrt, col = "green", lty = 1, lwd = 2)
lines(ICinf_nrt, col = "black", lty = 2)
lines(ICsup_nrt, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Tendência univariado - AR(1)","Tendência multivariado sem corr - AR(1)",
                             "Tendência multivariado com corr - AR(1)","IC 95% - estimativa direta"),
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_nrt*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(2.5,7.5))
lines(cv_esttrend_nrt, col = "blue",lwd=2, lty = 1)
lines(cv_multtrend_semcorr_nrt, col = "red",lwd=2,lty = 1)
lines(cv_multtrend_nrt, col = "green", lty = 1, lwd = 2)
legend("topleft", legend = c("CV ocupados","CV tendência - univariado", "CV tendência - multivariado sem corr.", "CV tendência - multivariado com corr."),
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("06 - Norte de Minas (tendências)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### 07 - VALE DO RIO DOCE ######################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/07_mod_val.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/13_multivariado_semcorrelacao - ocup_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
vl<-baseestr8reg$`07-Vale do Rio Doce`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtvl<-baseal8reg$`07-Vale do Rio Doce`
dbvl<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/07_params_rio.RDS") 

ocup_val <- vl$Total.de.ocupados/1000
se_db<- vl$sd_o/1000
cv_val <- se_db/ocup_val
ICinf_val<-ocup_val-1.96*se_db
ICsup_val<-ocup_val+1.96*se_db

ocup_val <- window(ts.union(ts(ocup_val, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_val <- window(ts.union(ts(ICinf_val, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_val <- window(ts.union(ts(ICsup_val, start = 2012, frequency = 4)), start = c(2013,3))
cv_val <- window(ts.union(ts(cv_val, start = 2012, frequency = 4)), start = c(2013,3))

estsinal_ar1val <- env1$ar1_val$ts.signal
estsinal_ar1val <- window(ts.union(ts(estsinal_ar1val, start = 2012, frequency = 4)), start = c(2013,3))
estcv_ar1val <- env1$ar1_val$cv.signal
estcv_ar1val <- window(ts.union(ts(estcv_ar1val, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_semcorr_val <- env2$modelo_mult_sem_corr$ts.signal_7
multsinal_semcorr_val <- window(ts.union(ts(multsinal_semcorr_val, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_semcorr_val <- env2$modelo_mult_sem_corr$cv.signal_7
cv_multsinal_semcorr_val <- window(ts.union(ts(cv_multsinal_semcorr_val, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_val <- env3$modelo_mult$ts.signal_7
multsinal_val <- window(ts.union(ts(multsinal_val, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_val <- env3$modelo_mult$cv.signal_7
cv_multsinal_val <- window(ts.union(ts(cv_multsinal_val, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1val <- env1$ar1_val$ts.trend
esttrend_ar1val <- window(ts.union(ts(esttrend_ar1val, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_val <- env1$ar1_val$cv.trend
cv_esttrend_val <- window(ts.union(ts(cv_esttrend_val, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_semcorr_val <- env2$modelo_mult_sem_corr$ts.trend_7
multtrend_semcorr_val <- window(ts.union(ts(multtrend_semcorr_val, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_semcorr_val <- env2$modelo_mult_sem_corr$cv.trend_7
cv_multtrend_semcorr_val <- window(ts.union(ts(cv_multtrend_semcorr_val, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_val <- env3$modelo_mult$ts.trend_7
multtrend_val <- window(ts.union(ts(multtrend_val, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_val <- env3$modelo_mult$cv.trend_7
cv_multtrend_val <- window(ts.union(ts(cv_multtrend_val, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(4, 0, 2, 0), cex = 0.8)
plot(ocup_val, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(700,1300))
lines(estsinal_ar1val, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_val, col = "red", lty = 1, lwd = 2)
lines(multsinal_val, col= "green", lty = 1, lwd = 2)
lines(ICinf_val, col = "black", lty = 2)
lines(ICsup_val, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Sinal univariado - AR(1)","Sinal multivariado sem corr. - AR(1)", 
                             "Sinal multivariado com corr - AR(1)", "IC 95% - estimativa direta"), 
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_val*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(2,10))
lines(estcv_ar1val, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_val, col = "red",lwd=2, lty = 1)
lines(cv_multsinal_val, col = "green",lwd=2,lty = 1)
legend("topleft", legend = c("CV ocupados","CV sinal - univariado", "CV sinal - multivariado sem corr.",
                             "CV sinal - multivariado com corr."),
       col = c("black","blue","red","green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("07 - Vale do Rio Doce", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Tendências:

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(4, 0, 2, 0), cex = 0.8)
plot(ocup_val, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(700,1300))
lines(esttrend_ar1val, col = "blue", lty = 1, lwd = 2)
lines(multtrend_semcorr_val, col = "red", lty = 1, lwd = 2)
lines(multtrend_val, col= "green", lty = 1, lwd = 2)
lines(ICinf_val, col = "black", lty = 2)
lines(ICsup_val, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Tendência univariado - AR(1)","Tendência multivariado sem corr - AR(1)",
                             "Tendência multivariado com corr - AR(1)","IC 95% - estimativa direta"),
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_val*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(2,10))
lines(cv_esttrend_val, col = "blue",lwd=2, lty = 1)
lines(cv_multtrend_semcorr_val, col = "red",lwd=2, lty = 1)
lines(cv_multtrend_val, col = "green",lwd=2,lty = 1)
legend("topleft", legend = c("CV ocupados","CV tendência - univariado", "CV tendência - multivariado sem corr.",
                             "CV tendência - multivariado com corr."),
       col = c("black","blue","red","green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("07 - Vale do Rio Doce (tendências)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### 08 - CENTRAL ###############################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/08_mod_cen.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/13_multivariado_semcorrelacao - ocup_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
cen<-baseestr8reg$`08-Central`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtcen<-baseal8reg$`08-Central`
dbcen<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/08_params_cen.RDS") 

ocup_cen <- cen$Total.de.ocupados/1000
se_db<- cen$sd_o/1000
cv_cen <- se_db/ocup_cen
ICinf_cen<-ocup_cen-1.96*se_db
ICsup_cen<-ocup_cen+1.96*se_db

ocup_cen <- window(ts.union(ts(ocup_cen, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_cen <- window(ts.union(ts(ICinf_cen, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_cen <- window(ts.union(ts(ICsup_cen, start = 2012, frequency = 4)), start = c(2013,3))
cv_cen <- window(ts.union(ts(cv_cen, start = 2012, frequency = 4)), start = c(2013,3))

estsinal_ar1cen <- env1$ar1_cen$ts.signal
estsinal_ar1cen <- window(ts.union(ts(estsinal_ar1cen, start = 2012, frequency = 4)), start = c(2013,3))
estcv_ar1cen <- env1$ar1_cen$cv.signal
estcv_ar1cen <- window(ts.union(ts(estcv_ar1cen, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_semcorr_cen <- env2$modelo_mult_sem_corr$ts.signal_8
multsinal_semcorr_cen <- window(ts.union(ts(multsinal_semcorr_cen, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_semcorr_cen <- env2$modelo_mult_sem_corr$cv.signal_8
cv_multsinal_semcorr_cen <- window(ts.union(ts(cv_multsinal_semcorr_cen, start = 2012, frequency = 4)), start = c(2013,3))

multsinal_cen <- env3$modelo_mult$ts.signal_8
multsinal_cen <- window(ts.union(ts(multsinal_cen, start = 2012, frequency = 4)), start = c(2013,3))
cv_multsinal_cen <- env3$modelo_mult$cv.signal_8
cv_multsinal_cen <- window(ts.union(ts(cv_multsinal_cen, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1cen <- env1$ar1_cen$ts.trend
esttrend_ar1cen <- window(ts.union(ts(esttrend_ar1cen, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_cen <- env1$ar1_cen$cv.trend
cv_esttrend_cen <- window(ts.union(ts(cv_esttrend_cen, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_semcorr_cen <- env2$modelo_mult_sem_corr$ts.trend_8
multtrend_semcorr_cen <- window(ts.union(ts(multtrend_semcorr_cen, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_semcorr_cen <- env2$modelo_mult_sem_corr$cv.trend_8
cv_multtrend_semcorr_cen <- window(ts.union(ts(cv_multtrend_semcorr_cen, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_cen <- env3$modelo_mult$ts.trend_8
multtrend_cen <- window(ts.union(ts(multtrend_cen, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_cen <- env3$modelo_mult$cv.trend_8
cv_multtrend_cen <- window(ts.union(ts(cv_multtrend_cen, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_cen, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(900,1700))
lines(estsinal_ar1cen, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_cen, col = "red", lty = 1, lwd = 2)
lines(multsinal_cen, col= "green", lty = 1, lwd = 2)
lines(ICinf_cen, col = "black", lty = 2)
lines(ICsup_cen, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Sinal univariado - AR(1)","Sinal multivariado sem corr. - AR(1)", 
                             "Sinal multivariado com corr - AR(1)", "IC 95% - estimativa direta"), 
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_cen*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(0.5,7))
lines(estcv_ar1cen, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_cen, col = "red",lwd=2, lty = 1)
lines(cv_multsinal_cen, col = "green",lwd=2,lty = 1)
legend("topleft", legend = c("CV ocupados","CV sinal - univariado","CV sinal - multivariado sem corr.", "CV sinal - multivariado com corr."),
       col = c("black","blue","red","green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("08 - Central", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

# Tendências:

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_cen, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(900,1700))
lines(esttrend_ar1cen, col = "blue", lty = 1, lwd = 2)
lines(multtrend_semcorr_cen, col = "red", lty = 1, lwd = 2)
lines(multtrend_cen, col= "green", lty = 1, lwd = 2)
lines(ICinf_cen, col = "black", lty = 2)
lines(ICsup_cen, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação", "Tendência univariado - AR(1)","Tendência multivariado sem corr - AR(1)",
                             "Tendência multivariado com corr - AR(1)","IC 95% - estimativa direta"),
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_cen*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(0.5,7))
lines(cv_esttrend_cen, col = "blue",lwd=2, lty = 1)
lines(cv_multtrend_semcorr_cen, col = "red",lwd=2, lty = 1)
lines(cv_multtrend_cen, col = "green",lwd=2,lty = 1)
legend("topleft", legend = c("CV ocupados","CV tendência - univariado","CV tendência - multivariado sem corr.", "CV tendência - multivariado com corr."),
       col = c("black","blue","red","green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("08 - Central (tendências)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


##### GRÁFICOS GERAIS PARA TEXTO ###############################################

rm(list = ls())
gc()

env1 <- new.env()
env2 <- new.env()
env3 <- new.env()
env4 <- new.env()
env5 <- new.env()
env6 <- new.env()
env7 <- new.env()
env8 <- new.env()
env9 <- new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/01_mod_bh.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/02_mod_ent.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/03_mod_sul.Rdata", envir = env3)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/04_mod_trg.Rdata", envir = env4)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/05_mod_mat.Rdata", envir = env5)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/06_mod_nrt.Rdata", envir = env6)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/07_mod_val.Rdata", envir = env7)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/08_mod_cen.Rdata", envir = env8)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/iniciais/01_mod_comcorr.Rdata",envir = env9)

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

#bh

ocup_bh <- bh$Total.de.ocupados/1000
se_db<- bh$sd_o/1000
cv_bh <- se_db/ocup_bh
ICinf_bh<-ocup_bh-1.96*se_db
ICsup_bh<-ocup_bh+1.96*se_db

ocup_bh <- window(ts.union(ts(ocup_bh, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_bh <- window(ts.union(ts(ICinf_bh, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_bh <- window(ts.union(ts(ICsup_bh, start = 2012, frequency = 4)), start = c(2013,3))
cv_bh <- window(ts.union(ts(cv_bh, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1bh <- env1$ar1_bh$ts.trend
esttrend_ar1bh <- window(ts.union(ts(esttrend_ar1bh, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_bh <- env1$ar1_bh$cv.trend
cv_esttrend_bh <- window(ts.union(ts(cv_esttrend_bh, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_bh <- env9$modelo_mult$ts.trend_1
multtrend_bh <- window(ts.union(ts(multtrend_bh, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_bh <- env9$modelo_mult$cv.trend_1
cv_multtrend_bh <- window(ts.union(ts(cv_multtrend_bh, start = 2012, frequency = 4)), start = c(2013,3))

#ent

ocup_ent <- (ent$Total.de.ocupados)/1000
se_db <- (ent$sd_o)/1000
cv_ent <- se_db/ocup_ent
ICinf_ent<-ocup_ent-1.96*se_db
ICsup_ent<-ocup_ent+1.96*se_db

ocup_ent <- window(ts.union(ts(ocup_ent, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_ent <- window(ts.union(ts(ICinf_ent, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_ent <- window(ts.union(ts(ICsup_ent, start = 2012, frequency = 4)), start = c(2013,3))
cv_ent <- window(ts.union(ts(cv_ent, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1ent <- env2$ar1_ent$ts.trend
esttrend_ar1ent <- window(ts.union(ts(esttrend_ar1ent, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_ent <- env2$ar1_ent$cv.trend
cv_esttrend_ent <- window(ts.union(ts(cv_esttrend_ent, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_ent <- env9$modelo_mult$ts.trend_2
multtrend_ent <- window(ts.union(ts(multtrend_ent, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_ent <- env9$modelo_mult$cv.trend_2
cv_multtrend_ent <- window(ts.union(ts(cv_multtrend_ent, start = 2012, frequency = 4)), start = c(2013,3))

#sul

ocup_sul <- sul$Total.de.ocupados/1000
se_db <- sul$sd_o/1000
cv_sul <- se_db/ocup_sul
ICinf_sul<-ocup_sul-1.96*se_db
ICsup_sul<-ocup_sul+1.96*se_db

ocup_sul <- window(ts.union(ts(ocup_sul, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_sul <- window(ts.union(ts(ICinf_sul, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_sul <- window(ts.union(ts(ICsup_sul, start = 2012, frequency = 4)), start = c(2013,3))
cv_sul <- window(ts.union(ts(cv_sul, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1sul <- env3$ar1_sul$ts.trend
esttrend_ar1sul <- window(ts.union(ts(esttrend_ar1sul, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_sul <- env3$ar1_sul$cv.trend
cv_esttrend_sul <- window(ts.union(ts(cv_esttrend_sul, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_sul <- env9$modelo_mult$ts.trend_3
multtrend_sul <- window(ts.union(ts(multtrend_sul, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_sul <- env9$modelo_mult$cv.trend_3
cv_multtrend_sul <- window(ts.union(ts(cv_multtrend_sul, start = 2012, frequency = 4)), start = c(2013,3))

#trg

ocup_trg <- trg$Total.de.ocupados/1000
se_db <- trg$sd_o/1000
cv_trg <- se_db/ocup_trg
ICinf_trg<-ocup_trg-1.96*se_db
ICsup_trg<-ocup_trg+1.96*se_db

ocup_trg <- window(ts.union(ts(ocup_trg, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_trg <- window(ts.union(ts(ICinf_trg, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_trg <- window(ts.union(ts(ICsup_trg, start = 2012, frequency = 4)), start = c(2013,3))
cv_trg <- window(ts.union(ts(cv_trg, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1trg <- env4$ar1_trg$ts.trend
esttrend_ar1trg <- window(ts.union(ts(esttrend_ar1trg, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_trg <- env4$ar1_trg$cv.trend
cv_esttrend_trg <- window(ts.union(ts(cv_esttrend_trg, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_trg <- env9$modelo_mult$ts.trend_4
multtrend_trg <- window(ts.union(ts(multtrend_trg, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_trg <- env9$modelo_mult$cv.trend_4
cv_multtrend_trg <- window(ts.union(ts(cv_multtrend_trg, start = 2012, frequency = 4)), start = c(2013,3))

#mat

ocup_mat<- mat$Total.de.ocupados/1000
se_db <- mat$sd_o/1000
cv_mat <- se_db/ocup_mat
ICinf_mat<-ocup_mat-1.96*se_db
ICsup_mat<-ocup_mat+1.96*se_db

ocup_mat <- window(ts.union(ts(ocup_mat, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_mat <- window(ts.union(ts(ICinf_mat, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_mat <- window(ts.union(ts(ICsup_mat, start = 2012, frequency = 4)), start = c(2013,3))
cv_mat <- window(ts.union(ts(cv_mat, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1mat <- env5$ar1_mat$ts.trend
esttrend_ar1mat <- window(ts.union(ts(esttrend_ar1mat, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_mat <- env5$ar1_mat$cv.trend
cv_esttrend_mat <- window(ts.union(ts(cv_esttrend_mat, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_mat <- env9$modelo_mult$ts.trend_5
multtrend_mat <- window(ts.union(ts(multtrend_mat, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_mat <- env9$modelo_mult$cv.trend_5
cv_multtrend_mat <- window(ts.union(ts(cv_multtrend_mat, start = 2012, frequency = 4)), start = c(2013,3))

#nrt

ocup_nrt <- nrt$Total.de.ocupados/1000
se_db <- nrt$sd_o/1000
cv_nrt <- se_db/ocup_nrt
ICinf_nrt<-ocup_nrt-1.96*se_db
ICsup_nrt<-ocup_nrt+1.96*se_db

ocup_nrt <- window(ts.union(ts(ocup_nrt, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_nrt <- window(ts.union(ts(ICinf_nrt, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_nrt <- window(ts.union(ts(ICsup_nrt, start = 2012, frequency = 4)), start = c(2013,3))
cv_nrt <- window(ts.union(ts(cv_nrt, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1nrt <- env6$ar1_nrt$ts.trend
esttrend_ar1nrt <- window(ts.union(ts(esttrend_ar1nrt, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_nrt <- env6$ar1_nrt$cv.trend
cv_esttrend_nrt <- window(ts.union(ts(cv_esttrend_nrt, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_nrt <- env9$modelo_mult$ts.trend_6
multtrend_nrt <- window(ts.union(ts(multtrend_nrt, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_nrt <- env9$modelo_mult$cv.trend_6
cv_multtrend_nrt <- window(ts.union(ts(cv_multtrend_nrt, start = 2012, frequency = 4)), start = c(2013,3))

#val

ocup_val <- vl$Total.de.ocupados/1000
se_db <- vl$sd_o/1000
cv_val <- se_db/ocup_val
ICinf_val<-ocup_val-1.96*se_db
ICsup_val<-ocup_val+1.96*se_db

ocup_val <- window(ts.union(ts(ocup_val, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_val <- window(ts.union(ts(ICinf_val, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_val <- window(ts.union(ts(ICsup_val, start = 2012, frequency = 4)), start = c(2013,3))
cv_val <- window(ts.union(ts(cv_val, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1val <- env7$ar1_val$ts.trend
esttrend_ar1val <- window(ts.union(ts(esttrend_ar1val, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_val <- env7$ar1_val$cv.trend
cv_esttrend_val <- window(ts.union(ts(cv_esttrend_val, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_val <- env9$modelo_mult$ts.trend_7
multtrend_val <- window(ts.union(ts(multtrend_val, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_val <- env9$modelo_mult$cv.trend_7
cv_multtrend_val <- window(ts.union(ts(cv_multtrend_val, start = 2012, frequency = 4)), start = c(2013,3))

#cen

ocup_cen <- cen$Total.de.ocupados/1000
se_db <- cen$sd_o/1000
cv_cen <- se_db/ocup_cen
ICinf_cen<-ocup_cen-1.96*se_db
ICsup_cen<-ocup_cen+1.96*se_db

ocup_cen <- window(ts.union(ts(ocup_cen, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_cen <- window(ts.union(ts(ICinf_cen, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_cen <- window(ts.union(ts(ICsup_cen, start = 2012, frequency = 4)), start = c(2013,3))
cv_cen <- window(ts.union(ts(cv_cen, start = 2012, frequency = 4)), start = c(2013,3))

esttrend_ar1cen <- env8$ar1_cen$ts.trend
esttrend_ar1cen <- window(ts.union(ts(esttrend_ar1cen, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_cen <- env8$ar1_cen$cv.trend
cv_esttrend_cen <- window(ts.union(ts(cv_esttrend_cen, start = 2012, frequency = 4)), start = c(2013,3))

multtrend_cen <- env9$modelo_mult$ts.trend_8
multtrend_cen <- window(ts.union(ts(multtrend_cen, start = 2012, frequency = 4)), start = c(2013,3))
cv_multtrend_cen <- env9$modelo_mult$cv.trend_8
cv_multtrend_cen <- window(ts.union(ts(cv_multtrend_cen, start = 2012, frequency = 4)), start = c(2013,3))

## Gráficos

png("Figura_Ocupacao_1.png", width = 1000, height = 1500, res = 100)

par(mfrow = c(4, 2),mar = c(4.5, 4, 1.5, 1.5),oma = c(0, 0, 4, 0),cex = 0.8,cex.axis = 0.7,cex.lab = 0.8)
plot(ocup_bh, type = "l", col = "black", lwd = 2, xlab = "Ano", ylab = "Ocupados (mil pessoas)", ylim=c(1000,1500),
     cex.axis = 0.8, cex.lab = 0.8)
lines(esttrend_ar1bh, col = "blue", lty = 1, lwd = 2)
lines(multtrend_bh, col = "red", lty = 1, lwd = 2)
lines(ICinf_bh, col = "black", lty = 2)
lines(ICsup_bh, col = "black", lty = 2)
legend("topleft",
       legend = c("Estimativa direta", "Tendência - Mod. univariado", "Tendência - Mod. multivariado", "IC 95% - estimativa direta"),
       col = c("black", "blue", "red", "black"),lty = c(1, 1, 1, 2), lwd = c(2, 2, 2, 1),bty = "n", cex = 0.7)

plot(cv_bh * 100, type = "l", col = "black", lwd = 2, xlab = "Ano", ylab = "CV (%)", ylim = c(1,3),
     cex.axis = 0.8, cex.lab = 0.8)
lines(cv_esttrend_bh, col = "blue", lwd = 2, lty = 1)
lines(cv_multtrend_bh, col = "red", lty = 1, lwd = 2)
legend("topleft",legend = c("CV Estimativa direta", "CV Tendência - Mod. univariado", "CV Tendência - Mod. multivariado"),
       col = c("black", "blue", "red"),lty = c(1, 1, 1), lwd = c(2, 2, 2),bty = "n", cex = 0.7)
mtext("01 - Belo Horizonte", side = 3, line = -1, adj = 0.5, cex = 0.9, font = 2,outer = TRUE, at = 0.5,padj = 0) 

plot(ocup_ent, type = "l", col = "black", lwd = 2,xlab = "Ano", ylab = "Ocupados (mil pessoas)",  ylim=c(1300,2100),
     cex.axis = 0.8, cex.lab = 0.8)
lines(esttrend_ar1ent, col = "blue", lty = 1, lwd = 2)
lines(multtrend_ent, col = "red", lty = 1, lwd = 2)
lines(ICinf_ent, col = "black", lty = 2)
lines(ICsup_ent, col = "black", lty = 2)
legend("topleft",legend = c("Estimativa direta", "Tendência - Mod. univariado", "Tendência - Mod. multivariado", "IC 95% - estimativa direta"),
       col = c("black", "blue", "red", "black"),lty = c(1, 1, 1, 2), lwd = c(2, 2, 2, 1),bty = "n", cex = 0.7)

plot(cv_ent * 100, type = "l", col = "black", lwd = 2,xlab = "Ano", ylab = "CV (%)", ylim = c(0.5,3),cex.axis = 0.8, cex.lab = 0.8)
lines(cv_esttrend_ent, col = "blue", lwd = 2, lty = 1)
lines(cv_multtrend_ent, col = "red", lwd = 2, lty = 1)
legend("topleft",legend = c("CV Estimativa direta", "CV Tendência - Mod. univariado", "CV Tendência - Mod. multivariado"),
       col = c("black", "blue", "red"),lty = c(1, 1, 1), lwd = c(2, 2, 2),bty = "n", cex = 0.7)
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte", side = 3, line = -1, adj = 0.5, cex = 0.9, font = 2,outer = TRUE, at = 0.5, padj = 30) 

plot(ocup_sul, type = "l", col = "black", lwd = 2,xlab = "Ano", ylab = "Ocupados (mil pessoas)", ylim=c(1000,1600),
     cex.axis = 0.8, cex.lab = 0.8)
lines(esttrend_ar1sul, col = "blue", lty = 1, lwd = 2)
lines(multtrend_sul, col = "red", lty = 1, lwd = 2)
lines(ICinf_sul, col = "black", lty = 2)
lines(ICsup_sul, col = "black", lty = 2)
legend("topleft",legend = c("Estimativa direta", "Tendência - Mod. univariado", "Tendência - Mod. multivariado", "IC 95% - estimativa direta"),
       col = c("black", "blue", "red", "black"),lty = c(1, 1, 1, 2), lwd = c(2, 2, 2, 1),bty = "n", cex = 0.7)

plot(cv_sul * 100, type = "l", col = "black", lwd = 2,xlab = "Ano", ylab = "CV (%)", ylim = c(1, 8),cex.axis = 0.8, cex.lab = 0.8)
lines(cv_esttrend_sul, col = "blue", lwd = 2, lty = 1)
lines(cv_multtrend_sul, col = "red", lwd = 2, lty = 1)
legend("topleft",legend = c("CV Estimativa direta", "CV Tendência - Mod. univariado", "CV Tendência - Mod. multivariado"),
       col = c("black", "blue", "red"),lty = c(1, 1, 1), lwd = c(2, 2, 2),bty = "n", cex = 0.7)
mtext("03 - Sul de Minas", side = 3, line = -1, adj = 0.5, cex = 0.9, font = 2,outer = TRUE, at = 0.5, padj = 60)

plot(ocup_trg, type = "l", col = "black", lwd = 2,xlab = "Ano", ylab = "Ocupados (mil pessoas)", ylim=c(1000,1850),
     cex.axis = 0.8, cex.lab = 0.8)
lines(esttrend_ar1trg, col = "blue", lty = 1, lwd = 2)
lines(multtrend_trg, col = "red", lty = 1, lwd = 2)
lines(ICinf_trg, col = "black", lty = 2)
lines(ICsup_trg, col = "black", lty = 2)
legend("topleft",legend = c("Estimativa direta", "Tendência - Mod. univariado", "Tendência - Mod. multivariado", "IC 95% - estimativa direta"),
       col = c("black", "blue", "red", "black"),lty = c(1, 1, 1, 2), lwd = c(2, 2, 2, 1),bty = "n", cex = 0.7)

plot(cv_trg * 100, type = "l", col = "black", lwd = 2,xlab = "Ano", ylab = "CV (%)", ylim = c(3, 21),cex.axis = 0.8, cex.lab = 0.8)
lines(cv_esttrend_trg, col = "blue", lwd = 2, lty = 1)
lines(cv_multtrend_trg, col = "red", lwd = 2, lty = 1)
legend("topleft",legend = c("CV Estimativa direta", "CV Tendência - Mod. univariado", "CV Tendência - Mod. multivariado"),
       col = c("black", "blue", "red"),lty = c(1, 1, 1), lwd = c(2, 2, 2),bty = "n", cex = 0.7)
mtext("04 - Triângulo Mineiro", side = 3, line = -1, adj = 0.5, cex = 0.9, font = 2,outer = TRUE, at = 0.5, padj = 90)

dev.off()


png("Figura_Ocupacao_2.png", width = 1000, height = 1500, res = 100)

par(mfrow = c(4, 2),mar = c(4.5, 4, 1.5, 1.5),oma = c(0, 0, 4, 0),cex = 0.8,cex.axis = 0.7,cex.lab = 0.8)
plot(ocup_mat, type = "l", col = "black", lwd = 2, xlab = "Ano", ylab = "Ocupados (mil pessoas)", ylim=c(800,1400),
     cex.axis = 0.8, cex.lab = 0.8)
lines(esttrend_ar1mat, col = "blue", lty = 1, lwd = 2)
lines(multtrend_mat, col = "red", lty = 1, lwd = 2)
lines(ICinf_mat, col = "black", lty = 2)
lines(ICsup_mat, col = "black", lty = 2)
legend("topleft",
       legend = c("Estimativa direta", "Tendência - Mod. univariado", "Tendência - Mod. multivariado", "IC 95% - estimativa direta"),
       col = c("black", "blue", "red", "black"),lty = c(1, 1, 1, 2), lwd = c(2, 2, 2, 1),bty = "n", cex = 0.7)

plot(cv_mat * 100, type = "l", col = "black", lwd = 2, xlab = "Ano", ylab = "CV (%)", ylim = c(2,7),
     cex.axis = 0.8, cex.lab = 0.8)
lines(cv_esttrend_mat, col = "blue", lwd = 2, lty = 1)
lines(cv_multtrend_mat, col = "red", lty = 1, lwd = 2)
legend("topleft",legend = c("CV Estimativa direta", "CV Tendência - Mod. univariado", "CV Tendência - Mod. multivariado"),
       col = c("black", "blue", "red"),lty = c(1, 1, 1), lwd = c(2, 2, 2),bty = "n", cex = 0.7)
mtext("05 - Zona da Mata", side = 3, line = -1, adj = 0.5, cex = 0.9, font = 2,outer = TRUE, at = 0.5,padj = 0) 

plot(ocup_nrt, type = "l", col = "black", lwd = 2,xlab = "Ano", ylab = "Ocupados (mil pessoas)", ylim=c(800,1500),
     cex.axis = 0.8, cex.lab = 0.8)
lines(esttrend_ar1nrt, col = "blue", lty = 1, lwd = 2)
lines(multtrend_nrt, col = "red", lty = 1, lwd = 2)
lines(ICinf_nrt, col = "black", lty = 2)
lines(ICsup_nrt, col = "black", lty = 2)
legend("topleft",legend = c("Estimativa direta", "Tendência - Mod. univariado", "Tendência - Mod. multivariado", "IC 95% - estimativa direta"),
       col = c("black", "blue", "red", "black"),lty = c(1, 1, 1, 2), lwd = c(2, 2, 2, 1),bty = "n", cex = 0.7)

plot(cv_nrt * 100, type = "l", col = "black", lwd = 2,xlab = "Ano", ylab = "CV (%)", ylim = c(2.5, 7.5),cex.axis = 0.8, cex.lab = 0.8)
lines(cv_esttrend_nrt, col = "blue", lwd = 2, lty = 1)
lines(cv_multtrend_nrt, col = "red", lwd = 2, lty = 1)
legend("topleft",legend = c("CV Estimativa direta", "CV Tendência - Mod. univariado", "CV Tendência - Mod. multivariado"),
       col = c("black", "blue", "red"),lty = c(1, 1, 1), lwd = c(2, 2, 2),bty = "n", cex = 0.7)
mtext("06 - Norte de Minas", side = 3, line = -1, adj = 0.5, cex = 0.9, font = 2,outer = TRUE, at = 0.5, padj = 30) 

plot(ocup_val, type = "l", col = "black", lwd = 2,xlab = "Ano", ylab = "Ocupados (mil pessoas)", ylim=c(700,1300),
     cex.axis = 0.8, cex.lab = 0.8)
lines(esttrend_ar1val, col = "blue", lty = 1, lwd = 2)
lines(multtrend_val, col = "red", lty = 1, lwd = 2)
lines(ICinf_val, col = "black", lty = 2)
lines(ICsup_val, col = "black", lty = 2)
legend("topleft",legend = c("Estimativa direta", "Tendência - Mod. univariado", "Tendência - Mod. multivariado", "IC 95% - estimativa direta"),
       col = c("black", "blue", "red", "black"),lty = c(1, 1, 1, 2), lwd = c(2, 2, 2, 1),bty = "n", cex = 0.7)

plot(cv_val * 100, type = "l", col = "black", lwd = 2,xlab = "Ano", ylab = "CV (%)", ylim = c(2,10),cex.axis = 0.8, cex.lab = 0.8)
lines(cv_esttrend_val, col = "blue", lwd = 2, lty = 1)
lines(cv_multtrend_val, col = "red", lwd = 2, lty = 1)
legend("topleft",legend = c("CV Estimativa direta", "CV Tendência - Mod. univariado", "CV Tendência - Mod. multivariado"),
       col = c("black", "blue", "red"),lty = c(1, 1, 1), lwd = c(2, 2, 2),bty = "n", cex = 0.7)
mtext("07 - Vale do Rio Doce", side = 3, line = -1, adj = 0.5, cex = 0.9, font = 2,outer = TRUE, at = 0.5, padj = 60)

plot(ocup_cen, type = "l", col = "black", lwd = 2,xlab = "Ano", ylab = "Ocupados (mil pessoas)", ylim=c(900,1700),
     cex.axis = 0.8, cex.lab = 0.8)
lines(esttrend_ar1cen, col = "blue", lty = 1, lwd = 2)
lines(multtrend_cen, col = "red", lty = 1, lwd = 2)
lines(ICinf_cen, col = "black", lty = 2)
lines(ICsup_cen, col = "black", lty = 2)
legend("topleft",legend = c("Estimativa direta", "Tendência - Mod. univariado", "Tendência - Mod. multivariado", "IC 95% - estimativa direta"),
       col = c("black", "blue", "red", "black"),lty = c(1, 1, 1, 2), lwd = c(2, 2, 2, 1),bty = "n", cex = 0.7)

plot(cv_cen * 100, type = "l", col = "black", lwd = 2,xlab = "Ano", ylab = "CV (%)", ylim = c(0.5, 7),cex.axis = 0.8, cex.lab = 0.8)
lines(cv_esttrend_cen, col = "blue", lwd = 2, lty = 1)
lines(cv_multtrend_cen, col = "red", lwd = 2, lty = 1)
legend("topleft",legend = c("CV Estimativa direta", "CV Tendência - Mod. univariado", "CV Tendência - Mod. multivariado"),
       col = c("black", "blue", "red"),lty = c(1, 1, 1), lwd = c(2, 2, 2),bty = "n", cex = 0.7)
mtext("08 - Central", side = 3, line = -1, adj = 0.5, cex = 0.9, font = 2,outer = TRUE, at = 0.5, padj = 90)

dev.off()

