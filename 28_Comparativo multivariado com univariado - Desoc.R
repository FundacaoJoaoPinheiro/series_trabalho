################################################################################
## COMPARANDO RESULTADOS DO MODELO MULTIVARIADO COM MODELO UNIVARIADO - DESOC ##
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

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/01_mod_bh.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/11_multivariado_semcorr - desoc_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
bh<-baseestr8reg$`01-Belo Horizonte`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtbh<-baseal8reg$`01-Belo Horizonte` 
dbbh<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/01_params_bh.RDS")

desoc_bh <- bh$Total.de.desocupados/1000
se_db<- bh$sd_d/1000
cv_bh <- se_db/desoc_bh
ICinf_bh<-desoc_bh-1.96*se_db
ICsup_bh<-desoc_bh+1.96*se_db

desoc_bh <- window(ts.union(ts(desoc_bh, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_bh <- window(ts.union(ts(ICinf_bh, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_bh <- window(ts.union(ts(ICsup_bh, start = 2012, frequency = 4)), start = c(2013,4))
cv_bh <- window(ts.union(ts(cv_bh, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_ma1bh <- env1$ma1_bh$ts.signal
estsinal_ma1bh <- window(ts.union(ts(estsinal_ma1bh, start = 2012, frequency = 4)), start = c(2013,4))
estcv_ma1bh <- env1$ma1_bh$cv.signal
estcv_ma1bh <- window(ts.union(ts(estcv_ma1bh, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_semcorr_bh <- env2$modelo_mult_sem_corr$ts.signal_1
multsinal_semcorr_bh <- window(ts.union(ts(multsinal_semcorr_bh, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_semcorr_bh <- env2$modelo_mult_sem_corr$cv.signal_1
cv_multsinal_semcorr_bh <- window(ts.union(ts(cv_multsinal_semcorr_bh, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_bh <- env3$modelo_mult$ts.signal_1
multsinal_bh <- window(ts.union(ts(multsinal_bh, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_bh <- env3$modelo_mult$cv.signal_1
cv_multsinal_bh <- window(ts.union(ts(cv_multsinal_bh, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_bh, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)", ylim=c(40,245))
lines(estsinal_ma1bh, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_bh, col= "red", lty = 1, lwd = 2)
lines(multsinal_bh, col= "green", lty = 1, lwd = 2)
lines(ICinf_bh, col = "black", lty = 2)
lines(ICsup_bh, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação", "Sinal univariado - MA(1)","Sinal multivariado sem corr. - MA(1)", 
                             "Sinal multivariado com corr - MA(1)", "IC 95% - estimativa direta"), 
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_bh*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(3,15))
lines(estcv_ma1bh, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_bh, col = "red",lwd=2,lty = 1)
lines(cv_multsinal_bh, col = "green", lty = 1, lwd = 2)
legend("topleft", legend = c("CV desocupados","CV sinal - univariado", "CV sinal - multivariado sem corr.", "CV sinal - multivariado com corr."), 
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("01 - Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 02 - COLAR E ENTORNO METROPOLITANO DE BELO HORIZONTE #######################
rm(list = ls())

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/02_mod_ent.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/11_multivariado_semcorr - desoc_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
ent<-baseestr8reg$`02-Colar e Entorno metropolitano de BH`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtent<-baseal8reg$`02-Colar e Entorno Metropolitano de BH`
dbent<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/02_params_ent.RDS")

desoc_ent <- (ent$Total.de.desocupados)/1000
se_db <- (ent$sd_d)/1000
cv_ent <- se_db/desoc_ent
ICinf_ent<-desoc_ent-1.96*se_db
ICsup_ent<-desoc_ent+1.96*se_db

desoc_ent <- window(ts.union(ts(desoc_ent, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_ent <- window(ts.union(ts(ICinf_ent, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_ent <- window(ts.union(ts(ICsup_ent, start = 2012, frequency = 4)), start = c(2013,4))
cv_ent <- window(ts.union(ts(cv_ent, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_ma1ent <- env1$ma1_ent$ts.signal
estsinal_ma1ent <- window(ts.union(ts(estsinal_ma1ent, start = 2012, frequency = 4)), start = c(2013,4))
estcv_ma1ent <- env1$ma1_ent$cv.signal
estcv_ma1ent <- window(ts.union(ts(estcv_ma1ent, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_semcorr_ent <- env2$modelo_mult_sem_corr$ts.signal_2
multsinal_semcorr_ent <- window(ts.union(ts(multsinal_semcorr_ent, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_semcorr_ent <- env2$modelo_mult_sem_corr$cv.signal_2
cv_multsinal_semcorr_ent <- window(ts.union(ts(cv_multsinal_semcorr_ent, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_ent <- env3$modelo_mult$ts.signal_2
multsinal_ent <- window(ts.union(ts(multsinal_ent, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_ent <- env3$modelo_mult$cv.signal_2
cv_multsinal_ent <- window(ts.union(ts(cv_multsinal_ent, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(4, 0, 2, 0), cex = 0.8)
plot(desoc_ent, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)", ylim=c(70,450))
lines(estsinal_ma1ent, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_ent, col= "red", lty = 1, lwd = 2)
lines(multsinal_ent, col = "green", lty = 1, lwd = 2)
lines(ICinf_ent, col = "black", lty = 2)
lines(ICsup_ent, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação", "Sinal univariado - MA(1)","Sinal multivariado sem corr - MA(1)",
                             "Sinal multivariado com corr - MA(1)","IC 95% - estimativa direta"),
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_ent*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(2,13))
lines(estcv_ma1ent, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_ent, col = "red",lwd=2,lty = 1)
lines(cv_multsinal_ent, col = "green",lwd=2,lty = 1)
legend("topleft", legend = c("CV desocupados","CV sinal - univariado", "CV sinal - multivariado sem corr.", "CV sinal - multivariado com corr."),
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("02 - Entorno metropolitano de Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)
mtext("Nota: A estimação do sinal no modelo sem correlação ficou sobreajustada. Isso resultou em um CV próximo a 0.", 
      side = 1, outer = TRUE, line = 1.0, cex = 0.8, font = 3)


### 03 - SUL DE MINAS ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/03_mod_sul.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/11_multivariado_semcorr - desoc_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
sul<-baseestr8reg$`03-Sul de Minas`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtsul<-baseal8reg$`03-Sul de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbsul<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/03_params_sul.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

desoc_sul <- sul$Total.de.desocupados/1000
se_db <- sul$sd_d/1000
cv_sul <- se_db/desoc_sul
ICinf_sul<-desoc_sul-1.96*se_db
ICsup_sul<-desoc_sul+1.96*se_db

desoc_sul <- window(ts.union(ts(desoc_sul, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_sul <- window(ts.union(ts(ICinf_sul, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_sul <- window(ts.union(ts(ICsup_sul, start = 2012, frequency = 4)), start = c(2013,4))
cv_sul <- window(ts.union(ts(cv_sul, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_arma11sul <- env1$arma11_sul$ts.signal
estsinal_arma11sul <- window(ts.union(ts(estsinal_arma11sul, start = 2012, frequency = 4)), start = c(2013,4))
estcv_arma11sul <- env1$arma11_sul$cv.signal
estcv_arma11sul <- window(ts.union(ts(estcv_arma11sul, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_semcorr_sul <- env2$modelo_mult_sem_corr$ts.signal_3
multsinal_semcorr_sul <- window(ts.union(ts(multsinal_semcorr_sul, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_semcorr_sul <- env2$modelo_mult_sem_corr$cv.signal_3
cv_multsinal_semcorr_sul <- window(ts.union(ts(cv_multsinal_semcorr_sul, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_sul <- env3$modelo_mult$ts.signal_3
multsinal_sul <- window(ts.union(ts(multsinal_sul, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_sul <- env3$modelo_mult$cv.signal_3
cv_multsinal_sul <- window(ts.union(ts(cv_multsinal_sul, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_sul, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)", ylim=c(15,230))
lines(estsinal_arma11sul, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_sul, col= "red", lty = 1, lwd = 2)
lines(multsinal_sul, col = "green", lty = 1, lwd = 2)
lines(ICinf_sul, col = "black", lty = 2)
lines(ICsup_sul, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação", "Sinal univariado - ARMA(1,1)","Sinal multivariado sem corr - ARMA(1,1)",
                             "Sinal multivariado com corr - ARMA(1,1)","IC 95% - estimativa direta"),
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_sul*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(5,20))
lines(estcv_arma11sul, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_sul, col = "red",lwd=2,lty = 1)
lines(cv_multsinal_sul, col = "green", lty = 1, lwd = 2)
legend("topleft", legend = c("CV desocupados","CV sinal - univariado", "CV sinal - multivariado sem corr.", "CV sinal - multivariado com corr."),
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("03 - Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 04 - TRIÂNGULO MINEIRO #####################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/04_mod_trg.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/11_multivariado_semcorr - desoc_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
trg<-baseestr8reg$`04-Triângulo Mineiro`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dttrg<-baseal8reg$`04-Triângulo Mineiro` 
dbtrg<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/04_params_trg.RDS") 

desoc_trg <- trg$Total.de.desocupados/1000
se_db <- trg$sd_d/1000
cv_trg <- se_db/desoc_trg
ICinf_trg<-desoc_trg-1.96*se_db
ICsup_trg<-desoc_trg+1.96*se_db

desoc_trg <- window(ts.union(ts(desoc_trg, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_trg <- window(ts.union(ts(ICinf_trg, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_trg <- window(ts.union(ts(ICsup_trg, start = 2012, frequency = 4)), start = c(2013,4))
cv_trg <- window(ts.union(ts(cv_trg, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_ma1trg <- env1$ma1_trg$ts.signal
estsinal_ma1trg <- window(ts.union(ts(estsinal_ma1trg, start = 2012, frequency = 4)), start = c(2013,4))
estcv_ma1trg <- env1$ma1_trg$cv.signal
estcv_ma1trg <- window(ts.union(ts(estcv_ma1trg, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_semcorr_trg <- env2$modelo_mult_sem_corr$ts.signal_4
multsinal_semcorr_trg <- window(ts.union(ts(multsinal_semcorr_trg, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_semcorr_trg <- env2$modelo_mult_sem_corr$cv.signal_4
cv_multsinal_semcorr_trg <- window(ts.union(ts(cv_multsinal_semcorr_trg, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_trg <- env3$modelo_mult$ts.signal_4
multsinal_trg <- window(ts.union(ts(multsinal_trg, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_trg <- env3$modelo_mult$cv.signal_4
cv_multsinal_trg <- window(ts.union(ts(cv_multsinal_trg, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_trg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)", ylim=c(30,215))
lines(estsinal_ma1trg, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_trg, col= "red", lty = 1, lwd = 2)
lines(multsinal_trg, col = "green", lty = 1, lwd = 2)
lines(ICinf_trg, col = "black", lty = 2)
lines(ICsup_trg, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação", "Sinal univariado - MA(1)","Sinal multivariado sem corr - MA(1)",
                             "Sinal multivariado com corr - MA(1)","IC 95% - estimativa direta"),
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_trg*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(4,18))
lines(estcv_ma1trg, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_trg, col = "red",lwd=2,lty = 1)
lines(cv_multsinal_trg, col = "green", lty = 1, lwd = 2)
legend("topleft", legend = c("CV desocupados","CV sinal - univariado", "CV sinal - multivariado sem corr.", "CV sinal - multivariado com corr."),
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("04 - Triângulo mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### 05 - ZONA DA MATA ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/05_mod_mat.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/11_multivariado_semcorr - desoc_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
mat<-baseestr8reg$`05-Mata de Minas Gerais`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtmat<-baseal8reg$`05-Mata de Minas Gerais`
dbmat<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/05_params_mat.RDS")

desoc_mat<- mat$Total.de.desocupados/1000
se_db <- mat$sd_d/1000
cv_mat <- se_db/desoc_mat
ICinf_mat<-desoc_mat-1.96*se_db
ICsup_mat<-desoc_mat+1.96*se_db

desoc_mat <- window(ts.union(ts(desoc_mat, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_mat <- window(ts.union(ts(ICinf_mat, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_mat <- window(ts.union(ts(ICsup_mat, start = 2012, frequency = 4)), start = c(2013,4))
cv_mat <- window(ts.union(ts(cv_mat, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_ma1mat <- env1$ma1_mat$ts.signal
estsinal_ma1mat <- window(ts.union(ts(estsinal_ma1mat, start = 2012, frequency = 4)), start = c(2013,4))
estcv_ma1mat <- env1$ma1_mat$cv.signal
estcv_ma1mat <- window(ts.union(ts(estcv_ma1mat, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_semcorr_mat <- env2$modelo_mult_sem_corr$ts.signal_5
multsinal_semcorr_mat <- window(ts.union(ts(multsinal_semcorr_mat, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_semcorr_mat <- env2$modelo_mult_sem_corr$cv.signal_5
cv_multsinal_semcorr_mat <- window(ts.union(ts(cv_multsinal_semcorr_mat, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_mat <- env3$modelo_mult$ts.signal_5
multsinal_mat <- window(ts.union(ts(multsinal_mat, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_mat <- env3$modelo_mult$cv.signal_5
cv_multsinal_mat <- window(ts.union(ts(cv_multsinal_mat, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_mat, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)", ylim=c(25,210))
lines(estsinal_ma1mat, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_mat, col= "red", lty = 1, lwd = 2)
lines(multsinal_mat, col = "green", lty = 1, lwd = 2)
lines(ICinf_mat, col = "black", lty = 2)
lines(ICsup_mat, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação", "Sinal univariado - MA(1)","Sinal multivariado sem corr - MA(1)",
                             "Sinal multivariado com corr - MA(1)","IC 95% - estimativa direta"),
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_mat*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(3,18))
lines(estcv_ma1mat, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_mat, col = "red",lwd=2,lty = 1)
lines(cv_multsinal_mat, col = "green", lty = 1, lwd = 2)
legend("topleft", legend = c("CV desocupados","CV sinal - univariado", "CV sinal - multivariado sem corr.", "CV sinal - multivariado com corr."),
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("05 - Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 06 - NORTE DE MINAS ########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/06_mod_nrt.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/11_multivariado_semcorr - desoc_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
nrt<-baseestr8reg$`06-Norte de Minas`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtnrt<-baseal8reg$`06-Norte de Minas`
dbnrt<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/06_params_nrt.RDS") 

desoc_nrt <- nrt$Total.de.desocupados/1000
se_db <- nrt$sd_d/1000
cv_nrt <- se_db/desoc_nrt
ICinf_nrt<-desoc_nrt-1.96*se_db
ICsup_nrt<-desoc_nrt+1.96*se_db

desoc_nrt <- window(ts.union(ts(desoc_nrt, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_nrt <- window(ts.union(ts(ICinf_nrt, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_nrt <- window(ts.union(ts(ICsup_nrt, start = 2012, frequency = 4)), start = c(2013,4))
cv_nrt <- window(ts.union(ts(cv_nrt, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_ma1nrt <- env1$ma1_nrt$ts.signal
estsinal_ma1nrt <- window(ts.union(ts(estsinal_ma1nrt, start = 2012, frequency = 4)), start = c(2013,4))
estcv_ma1nrt <- env1$ma1_nrt$cv.signal
estcv_ma1nrt <- window(ts.union(ts(estcv_ma1nrt, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_semcorr_nrt <- env2$modelo_mult_sem_corr$ts.signal_6
multsinal_semcorr_nrt <- window(ts.union(ts(multsinal_semcorr_nrt, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_semcorr_nrt <- env2$modelo_mult_sem_corr$cv.signal_6
cv_multsinal_semcorr_nrt <- window(ts.union(ts(cv_multsinal_semcorr_nrt, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_nrt <- env3$modelo_mult$ts.signal_6
multsinal_nrt <- window(ts.union(ts(multsinal_nrt, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_nrt <- env3$modelo_mult$cv.signal_6
cv_multsinal_nrt <- window(ts.union(ts(cv_multsinal_nrt, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_nrt, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)", ylim=c(40,280))
lines(estsinal_ma1nrt, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_nrt, col= "red", lty = 1, lwd = 2)
lines(multsinal_nrt, col = "green", lty = 1, lwd = 2)
lines(ICinf_nrt, col = "black", lty = 2)
lines(ICsup_nrt, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação", "Sinal univariado - MA(1)","Sinal multivariado sem corr - MA(1)",
                             "Sinal multivariado com corr - MA(1)","IC 95% - estimativa direta"),
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_nrt*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(2.5,22))
lines(estcv_ma1nrt, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_nrt, col = "red",lwd=2,lty = 1)
lines(cv_multsinal_nrt, col = "green", lty = 1, lwd = 2)
legend("topleft", legend = c("CV desocupados","CV sinal - univariado", "CV sinal - multivariado sem corr.", "CV sinal - multivariado com corr."),
       col = c("black","blue", "red", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### 07 - VALE DO RIO DOCE ######################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/07_mod_val.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/11_multivariado_semcorr - desoc_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
vl<-baseestr8reg$`07-Vale do Rio Doce`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtvl<-baseal8reg$`07-Vale do Rio Doce`
dbvl<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/07_params_rio.RDS") 

desoc_val <- vl$Total.de.desocupados/1000
se_db <- vl$sd_d/1000
cv_val <- se_db/desoc_val
ICinf_val<-desoc_val-1.96*se_db
ICsup_val<-desoc_val+1.96*se_db

desoc_val <- window(ts.union(ts(desoc_val, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_val <- window(ts.union(ts(ICinf_val, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_val <- window(ts.union(ts(ICsup_val, start = 2012, frequency = 4)), start = c(2013,4))
cv_val <- window(ts.union(ts(cv_val, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_ar1val <- env1$ar1_val$ts.signal
estsinal_ar1val <- window(ts.union(ts(estsinal_ar1val, start = 2012, frequency = 4)), start = c(2013,4))
estcv_ar1val <- env1$ar1_val$cv.signal
estcv_ar1val <- window(ts.union(ts(estcv_ar1val, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_semcorr_val <- env2$modelo_mult_sem_corr$ts.signal_7
multsinal_semcorr_val <- window(ts.union(ts(multsinal_semcorr_val, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_semcorr_val <- env2$modelo_mult_sem_corr$cv.signal_7
cv_multsinal_semcorr_val <- window(ts.union(ts(cv_multsinal_semcorr_val, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_val <- env3$modelo_mult$ts.signal_7
multsinal_val <- window(ts.union(ts(multsinal_val, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_val <- env3$modelo_mult$cv.signal_7
cv_multsinal_val <- window(ts.union(ts(cv_multsinal_val, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_val, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)", ylim=c(20,230))
lines(estsinal_ar1val, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_val, col = "red", lty = 1, lwd = 2)
lines(multsinal_val, col= "green", lty = 1, lwd = 2)
lines(ICinf_val, col = "black", lty = 2)
lines(ICsup_val, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação", "Sinal univariado - AR(1)","Sinal multivariado sem corr - AR(1)",
                             "Sinal multivariado com corr - AR(1)","IC 95% - estimativa direta"),
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_val*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(3.5,18))
lines(estcv_ar1val, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_val, col = "red",lwd=2, lty = 1)
lines(cv_multsinal_val, col = "green",lwd=2,lty = 1)
legend("topleft", legend = c("CV desocupados","CV sinal - univariado", "CV sinal - multivariado sem corr.",
                             "CV sinal - multivariado com corr."),
       col = c("black","blue", "green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("07 - Vale do Rio Doce", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### 08 - CENTRAL ###############################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()
env3<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/08_mod_cen.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/11_multivariado_semcorr - desoc_8reg/iniciais/01_mod_semcorr.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/iniciais/01_mod_comcorr.Rdata",envir = env3)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
cen<-baseestr8reg$`08-Central`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtcen<-baseal8reg$`08-Central`
dbcen<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/08_params_cen.RDS") 

desoc_cen <- cen$Total.de.desocupados/1000
se_db <- cen$sd_d/1000
cv_cen <- se_db/desoc_cen
ICinf_cen<-desoc_cen-1.96*se_db
ICsup_cen<-desoc_cen+1.96*se_db

desoc_cen <- window(ts.union(ts(desoc_cen, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_cen <- window(ts.union(ts(ICinf_cen, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_cen <- window(ts.union(ts(ICsup_cen, start = 2012, frequency = 4)), start = c(2013,4))
cv_cen <- window(ts.union(ts(cv_cen, start = 2012, frequency = 4)), start = c(2013,4))

estsinal_ma1cen <- env1$ma1_cen$ts.signal
estsinal_ma1cen <- window(ts.union(ts(estsinal_ma1cen, start = 2012, frequency = 4)), start = c(2013,4))
estcv_ma1cen <- env1$ma1_cen$cv.signal
estcv_ma1cen <- window(ts.union(ts(estcv_ma1cen, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_semcorr_cen <- env2$modelo_mult_sem_corr$ts.signal_8
multsinal_semcorr_cen <- window(ts.union(ts(multsinal_semcorr_cen, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_semcorr_cen <- env2$modelo_mult_sem_corr$cv.signal_8
cv_multsinal_semcorr_cen <- window(ts.union(ts(cv_multsinal_semcorr_cen, start = 2012, frequency = 4)), start = c(2013,4))

multsinal_cen <- env3$modelo_mult$ts.signal_8
multsinal_cen <- window(ts.union(ts(multsinal_cen, start = 2012, frequency = 4)), start = c(2013,4))
cv_multsinal_cen <- env3$modelo_mult$cv.signal_8
cv_multsinal_cen <- window(ts.union(ts(cv_multsinal_cen, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_cen, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)", ylim=c(25,220))
lines(estsinal_ma1cen, col = "blue", lty = 1, lwd = 2)
lines(multsinal_semcorr_cen, col = "red", lty = 1, lwd = 2)
lines(multsinal_cen, col= "green", lty = 1, lwd = 2)
lines(ICinf_cen, col = "black", lty = 2)
lines(ICsup_cen, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação", "Sinal univariado - MA(1)","Sinal multivariado sem corr - MA(1)",
                             "Sinal multivariado com corr - MA(1)","IC 95% - estimativa direta"),
       col = c("black","blue","red","green","black"),lty = c(1,1,1,1,2),lwd = c(2,2,2,2,1),bty = "n", cex=0.8)

plot((cv_cen*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "CV (%)",  ylim=c(3,20))
lines(estcv_ma1cen, col = "blue",lwd=2, lty = 1)
lines(cv_multsinal_semcorr_cen, col = "red",lwd=2, lty = 1)
lines(cv_multsinal_cen, col = "green",lwd=2,lty = 1)
legend("topleft", legend = c("CV desocupados","CV sinal - univariado","CV sinal - multivariado sem corr.", "CV sinal - multivariado com corr."),
       col = c("black","blue","red","green"),lty = c(1,1,1,1),lwd = c(2,2,2,2),bty = "n", cex=0.8)
mtext("08 - Central", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

