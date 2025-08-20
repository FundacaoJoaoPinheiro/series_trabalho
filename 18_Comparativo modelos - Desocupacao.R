################################################################################
##                COMPARATIVO MODELOS SMOOTH E ESTRUTURAL                     ##
################################################################################

## Para visualizar os gráficos em segunda tela:

dev.new()
dev.new()


### 01 - BELO HORIZONTE ########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/01_mod_bh.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/01_mod_bh.Rdata", envir = env2)

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

sm_ma1_bh<-env1$ma1_bh$ts.signal
sm_ma1_bh<-window(ts.union(ts(sm_ma1_bh, start = 2012, frequency = 4)), start = c(2013,4))
cv_sm_ma1_bh<-env1$ma1_bh$cv.signal
cv_sm_ma1_bh<-window(ts.union(ts(cv_sm_ma1_bh, start = 2012, frequency = 4)), start = c(2013,4))

est_ma1_bh<-env2$ma1_bh$ts.signal
se_est_sinal_bh<-env2$ma1_bh$se.signal
ICinf_sinal <- est_ma1_bh - 1.96 * se_est_sinal_bh
ICinf_sinal <- window(ts.union(ts(ICinf_sinal, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_sinal <- est_ma1_bh + 1.96 * se_est_sinal_bh
ICsup_sinal <- window(ts.union(ts(ICsup_sinal, start = 2012, frequency = 4)), start = c(2013,4))
est_ma1_bh<-window(ts.union(ts(est_ma1_bh, start = 2012, frequency = 4)), start = c(2013,4))
cv_est_ma1_bh<-env2$ma1_bh$cv.signal
cv_est_ma1_bh<-window(ts.union(ts(cv_est_ma1_bh, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_bh, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(45,270))
lines(sm_ma1_bh, col = "blue", lty = 1, lwd = 2)
lines(est_ma1_bh, col = "red", lty = 1, lwd = 2)
lines(ICinf_bh, col = "black", lty = 2)
lines(ICsup_bh, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Smooth",
                             "Sinal da Desocupação MA(1) - Estrutural","IC 95%: signal-based"), 
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_bh*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(0,15))
lines(cv_sm_ma1_bh, col = "blue",lwd=2,lty = 1)
lines(cv_est_ma1_bh, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV MA(1) - Smooth","Sinal CV MA(1) - Estrutural"), 
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("01 - Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ma1_bh, type = "l", col = "red", lwd = 2,
     main = "01-Belo Horizonte",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(45,270))
lines(desoc_bh, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: signal-based"), 
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")


### 02 - COLAR E ENTORNO METROPOLITANO DE BELO HORIZONTE #######################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/02_mod_ent.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/02_mod_ent.Rdata", envir = env2)

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

sm_ma1_ent<-env1$ma1_ent$ts.signal
sm_ma1_ent<-window(ts.union(ts(sm_ma1_ent, start = 2012, frequency = 4)), start = c(2013,4))
cv_sm_ma1_ent<-env1$ma1_ent$cv.signal
cv_sm_ma1_ent<-window(ts.union(ts(cv_sm_ma1_ent, start = 2012, frequency = 4)), start = c(2013,4))

est_ma1_ent<-env2$ma1_ent$ts.signal
se_est_sinal_ent<-env2$ma1_ent$se.signal
ICinf_sinal_ent <- est_ma1_ent - 1.96 * se_est_sinal_ent
ICinf_sinal_ent <- window(ts.union(ts(ICinf_sinal_ent, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_sinal_ent <- est_ma1_ent + 1.96 * se_est_sinal_ent
ICsup_sinal_ent <- window(ts.union(ts(ICsup_sinal_ent, start = 2012, frequency = 4)), start = c(2013,4))
est_ma1_ent<-window(ts.union(ts(est_ma1_ent, start = 2012, frequency = 4)), start = c(2013,4))
cv_est_ma1_ent<-env2$ma1_ent$cv.signal
cv_est_ma1_ent<-window(ts.union(ts(cv_est_ma1_ent, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_ent, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(75,450))
lines(sm_ma1_ent, col = "blue", lty = 1, lwd = 2)
lines(est_ma1_ent, col = "red", lty = 1, lwd = 2)
lines(ICinf_ent, col = "black", lty = 2)
lines(ICsup_ent, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Smooth",
                             "Sinal da Desocupação MA(1) - Estrutural","IC 95%: signal-based"), 
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_ent*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(2,14))
lines(cv_sm_ma1_ent, col = "blue",lwd=2,lty = 1)
lines(cv_est_ma1_ent, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV MA(1) - Smooth","Sinal CV MA(1) - Estrutural"), 
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("02 - Colar e Entorno metropolitano de Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ma1_ent, type = "l", col = "red", lwd = 2,
     main = "02-Colar e Entorno metropolitano de Belo Horizonte",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(80,400))
lines(desoc_ent, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal_ent, col = "red", lty = 2)
lines(ICsup_sinal_ent, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: signal-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")


### 03 - SUL DE MINAS ##########################################################

rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/03_mod_sul.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/03_mod_sul.Rdata", envir = env2)
  
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

sm_arma11_sul<-env1$arma11_sul$ts.signal
sm_arma11_sul<-window(ts.union(ts(sm_arma11_sul, start = 2012, frequency = 4)), start = c(2013,4))
cv_sm_arma11_sul<-env1$arma11_sul$cv.signal
cv_sm_arma11_sul<-window(ts.union(ts(cv_sm_arma11_sul, start = 2012, frequency = 4)), start = c(2013,4))

est_arma11_sul<-env2$arma11_sul$ts.signal
se_est_sinal_sul<-env2$arma11_sul$se.signal
ICinf_sinal_sul <- est_arma11_sul - 1.96 * se_est_sinal_sul
ICinf_sinal_sul <- window(ts.union(ts(ICinf_sinal_sul, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_sinal_sul <- est_arma11_sul + 1.96 * se_est_sinal_sul
ICsup_sinal_sul <- window(ts.union(ts(ICsup_sinal_sul, start = 2012, frequency = 4)), start = c(2013,4))
est_arma11_sul<-window(ts.union(ts(est_arma11_sul, start = 2012, frequency = 4)), start = c(2013,4))
cv_est_arma11_sul<-env2$arma11_sul$cv.signal
cv_est_arma11_sul<-window(ts.union(ts(cv_est_arma11_sul, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_sul, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(20,220))
lines(sm_arma11_sul, col = "blue", lty = 1, lwd = 2)
lines(est_arma11_sul, col = "red", lty = 1, lwd = 2)
lines(ICinf_sul, col = "black", lty = 2)
lines(ICsup_sul, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Smooth",
                             "Sinal da Desocupação MA(1) - Estrutural","IC 95%: signal-based"), 
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_sul*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(5,20))
lines(cv_sm_arma11_sul, col = "blue",lwd=2,lty = 1)
lines(cv_est_arma11_sul, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV ARMA(1,1) - Smooth","Sinal CV ARMA(1,1) - Estrutural"), 
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("03 - Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_arma11_sul, type = "l", col = "red", lwd = 2,
     main = "03-Sul de Minas",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(20,200))
lines(desoc_sul, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal_sul, col = "red", lty = 2)
lines(ICsup_sinal_sul, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural ARMA(1,1)","Desocupação: design-based","IC 95%: model-based"),
       col = c("red","black", "red"),lty = c(1,1,2),lwd = c(2,2,1),bty = "n")


### 04 - TRIÂNGULO MINEIRO #####################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/04_mod_trg.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/04_mod_trg.Rdata", envir = env2)

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

sm_ma1_trg<-env1$ma1_trg$ts.signal
sm_ma1_trg<-window(ts.union(ts(sm_ma1_trg, start = 2012, frequency = 4)), start = c(2013,4))
cv_sm_ma1_trg<-env1$ma1_trg$cv.signal
cv_sm_ma1_trg<-window(ts.union(ts(cv_sm_ma1_trg, start = 2012, frequency = 4)), start = c(2013,4))

est_ma1_trg<-env2$ma1_trg$ts.signal
se_est_sinal_trg<-env2$ma1_trg$se.signal
ICinf_sinal_trg <- est_ma1_trg - 1.96 * se_est_sinal_trg
ICinf_sinal_trg <- window(ts.union(ts(ICinf_sinal_trg, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_sinal_trg <- est_ma1_trg + 1.96 * se_est_sinal_trg
ICsup_sinal_trg <- window(ts.union(ts(ICsup_sinal_trg, start = 2012, frequency = 4)), start = c(2013,4))
est_ma1_trg<-window(ts.union(ts(est_ma1_trg, start = 2012, frequency = 4)), start = c(2013,4))
cv_est_ma1_trg<-env2$ma1_trg$cv.signal
cv_est_ma1_trg<-window(ts.union(ts(cv_est_ma1_trg, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_trg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(35,210))
lines(sm_ma1_trg, col = "blue", lty = 1, lwd = 2)
lines(est_ma1_trg, col = "red", lty = 1, lwd = 2)
lines(ICinf_trg, col = "black", lty = 2)
lines(ICsup_trg, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Smooth",
                             "Sinal da Desocupação MA(1) - Estrutural","IC 95%: signal-based"), 
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_trg*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(4,20))
lines(cv_sm_ma1_trg, col = "blue",lwd=2,lty = 1)
lines(cv_est_ma1_trg, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV MA(1) - Smooth","Sinal CV MA(1) - Estrutural"), 
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("04 - Triângulo Mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ma1_trg, type = "l", col = "red", lwd = 2,
     main = "04-Triângulo Mineiro",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(30,190))
lines(desoc_trg, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal_trg, col = "red", lty = 2)
lines(ICsup_sinal_trg, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: model-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")


### 05 - ZONA DA MATA ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/05_mod_mat.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/05_mod_mat.Rdata", envir = env2)

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

sm_ma1_mat<-env1$ma1_mat$ts.signal
sm_ma1_mat<-window(ts.union(ts(sm_ma1_mat, start = 2012, frequency = 4)), start = c(2013,4))
cv_sm_ma1_mat<-env1$ma1_mat$cv.signal
cv_sm_ma1_mat<-window(ts.union(ts(cv_sm_ma1_mat, start = 2012, frequency = 4)), start = c(2013,4))

est_ma1_mat<-env2$ma1_mat$ts.signal
se_est_sinal_mat<-env2$ma1_mat$se.signal
ICinf_sinal_mat <- est_ma1_mat - 1.96 * se_est_sinal_mat
ICinf_sinal_mat <- window(ts.union(ts(ICinf_sinal_mat, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_sinal_mat <- est_ma1_mat + 1.96 * se_est_sinal_mat
ICsup_sinal_mat <- window(ts.union(ts(ICsup_sinal_mat, start = 2012, frequency = 4)), start = c(2013,4))
est_ma1_mat<-window(ts.union(ts(est_ma1_mat, start = 2012, frequency = 4)), start = c(2013,4))
cv_est_ma1_mat<-env2$ma1_mat$cv.signal
cv_est_ma1_mat<-window(ts.union(ts(cv_est_ma1_mat, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_mat, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(30,200))
lines(sm_ma1_mat, col = "blue", lty = 1, lwd = 2)
lines(est_ma1_mat, col = "red", lty = 1, lwd = 2)
lines(ICinf_mat, col = "black", lty = 2)
lines(ICsup_mat, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Smooth",
                             "Sinal da Desocupação MA(1) - Estrutural","IC 95%: signal-based"), 
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_mat*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(4,20))
lines(cv_sm_ma1_mat, col = "blue",lwd=2,lty = 1)
lines(cv_est_ma1_mat, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV MA(1) - Smooth","Sinal CV MA(1) - Estrutural"), 
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("05 - Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ma1_mat, type = "l", col = "red", lwd = 2,
     main = "05-Zona da Mata",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(30,180))
lines(desoc_mat, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal_mat, col = "red", lty = 2)
lines(ICsup_sinal_mat, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: model-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")

### 06 - NORTE DE MINAS ########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/06_mod_nrt.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/06_mod_nrt.Rdata", envir = env2)

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

sm_ma1_nrt<-env1$ma1_nrt$ts.signal
sm_ma1_nrt<-window(ts.union(ts(sm_ma1_nrt, start = 2012, frequency = 4)), start = c(2013,4))
cv_sm_ma1_nrt<-env1$ma1_nrt$cv.signal
cv_sm_ma1_nrt<-window(ts.union(ts(cv_sm_ma1_nrt, start = 2012, frequency = 4)), start = c(2013,4))

est_ma1_nrt<-env2$ma1_nrt$ts.signal
se_est_sinal_nrt<-env2$ma1_nrt$se.signal
ICinf_sinal_nrt <- est_ma1_nrt - 1.96 * se_est_sinal_nrt
ICinf_sinal_nrt <- window(ts.union(ts(ICinf_sinal_nrt, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_sinal_nrt <- est_ma1_nrt + 1.96 * se_est_sinal_nrt
ICsup_sinal_nrt <- window(ts.union(ts(ICsup_sinal_nrt, start = 2012, frequency = 4)), start = c(2013,4))
est_ma1_nrt<-window(ts.union(ts(est_ma1_nrt, start = 2012, frequency = 4)), start = c(2013,4))
cv_est_ma1_nrt<-env2$ma1_nrt$cv.signal
cv_est_ma1_nrt<-window(ts.union(ts(cv_est_ma1_nrt, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_nrt, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(45,275))
lines(sm_ma1_nrt, col = "blue", lty = 1, lwd = 2)
lines(est_ma1_nrt, col = "red", lty = 1, lwd = 2)
lines(ICinf_nrt, col = "black", lty = 2)
lines(ICsup_nrt, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Smooth",
                             "Sinal da Desocupação MA(1) - Estrutural","IC 95%: signal-based"), 
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_nrt*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(2.5,23))
lines(cv_sm_ma1_nrt, col = "blue",lwd=2,lty = 1)
lines(cv_est_ma1_nrt, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV MA(1) - Smooth","Sinal CV MA(1) - Estrutural"), 
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ma1_nrt, type = "l", col = "red", lwd = 2,
     main = "06-Norte de Minas",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(50,250))
lines(desoc_nrt, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal_nrt, col = "red", lty = 2)
lines(ICsup_sinal_nrt, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: model-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")


## GRÁFICO DA TENDÊNCIA COM IC ESTIMATIVA DIRETA

esttrend_ma1nrt <- env2$ma1_nrt$ts.trend
se_est_trend_nrt<-env2$ma1_nrt$se.trend
ICinf_trend_nrt <- esttrend_ma1nrt - 1.96 * se_est_trend_nrt
ICinf_trend_nrt <- window(ts.union(ts(ICinf_trend_nrt, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_trend_nrt <- esttrend_ma1nrt + 1.96 * se_est_trend_nrt
ICsup_trend_nrt <- window(ts.union(ts(ICsup_trend_nrt, start = 2012, frequency = 4)), start = c(2013,4))
esttrend_ma1nrt <- window(ts.union(ts(esttrend_ma1nrt, start = 2012, frequency = 4)), start = c(2013,4))
cv_esttrend_nrt <- env2$ma1_nrt$cv.trend
cv_esttrend_nrt <- window(ts.union(ts(cv_esttrend_nrt, start = 2012, frequency = 4)), start = c(2013,4))


par(mfrow=c(1,2), mar=c(5,5,3,1), oma=c(0,0,2,0), cex=0.8)
plot(esttrend_ma1nrt, type = "l", col = "red", lwd = 2,
     main = "Tendência",
     xlab = "Ano", ylab = "Total de desocupados (mil pessoas)",
     ylim = c(50,250))
lines(desoc_nrt, col = "black", lty = 1, lwd = 2)
lines(ICinf_trend_nrt, col = "red", lty = 2)
lines(ICsup_trend_nrt, col = "red", lty = 2)
legend("topleft", legend = c("Tendência: model-based","Estimativa direta","IC 95%: tendência"),
       col = c("red","black", "red"), lty = c(1,1, 2), lwd = c(2,2,1), bty = "n")

plot((cv_nrt*100), type = "l", col = "black", lwd = 2,
     main = "Coeficiente de Variação (CV) da tendência",
     xlab = "Ano", ylab = "CV(%)",
     ylim = c(2.5,23))
lines(cv_esttrend_nrt, col = "red", lty = 1, lwd = 2)
legend("topleft", legend = c("CV: estimativa direta","CV: model-based"),
       col = c("black","red"), lty = c(1,1), lwd = c(2,2), bty = "n")
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## Sinal da série

par(mfrow=c(1,2), mar=c(5,5,3,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_nrt, type = "l", col = "black", lwd = 2,
     main = "Sinal",
     xlab = "Ano", ylab = "Total de desocupados (mil pessoas)",
     ylim = c(50,250))
lines(est_ma1_nrt, col = "blue", lty = 1, lwd = 2)
lines(ICinf_sinal_nrt, col = "black", lty = 2)
lines(ICsup_sinal_nrt, col = "black", lty = 2)
legend("topleft", legend = c("Estimativa direta","Sinal: model-based","IC 95%: estimativa direta"),
       col = c("black","blue", "black"), lty = c(1,1, 2), lwd = c(2,2,1), bty = "n")

plot((cv_nrt*100), type = "l", col = "black", lwd = 2,
     main = "Coeficiente de Variação (CV)",
     xlab = "Ano", ylab = "CV(%)",
     ylim = c(2.5,23))
lines(cv_esttrend_nrt, col = "blue", lty = 1, lwd = 2)
legend("topleft", legend = c("CV: estimativa direta","CV: model-based"),
       col = c("black","blue"), lty = c(1,1), lwd = c(2,2), bty = "n")
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## Gráfico 4 plots:

figttrend_nrt<-window(ts.union(ts(env2$ma1_nrt$ts.original, start = 2012, frequency = 4),ts(env2$ma1_nrt$ts.trend, start = 2012, frequency = 4)), start = c(2013, 4))
figsaz_nrt<-window(ts.union(ts(env2$ma1_nrt$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 4))
figirr_nrt<-window(ts.union(ts(env2$ma1_nrt$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 4))
figsample_nrt<-window(ts.union(ts(env2$ma1_nrt$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 4))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figttrend_nrt, plot.type = "single", col = c(1, 2), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottom", legend = c("Desocupação: design-based",
                            "Tendência da desocupação: model-based"),
       lty = c(1, 1), col = c(1, 2), bty = 'n', lwd = c(2))
mtext("Desocupados (mil pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_nrt, plot.type = "single", col = c(1), ylab = "", xlab = "", lty = c(1), lwd = c(2), ylim=c(-20,20))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(1), bty = 'n', lwd = c(2))
mtext("Desocupados (mil pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_nrt, plot.type = "single", col = c(1), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(1), bty = 'n', lwd = c(2))
mtext("Desocupados (mil pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_nrt, plot.type = "single", col = c(1), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(1), bty = 'n', lwd = c(2))
mtext("Desocupados (mil pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## Slope da tendência com IC

estslope_ma1nrt <- env2$ma1_nrt$ts.slope
se_est_slope_nrt <- env2$ma1_nrt$se.slope
ICinf_slope_nrt <- estslope_ma1nrt - 1.96 * se_est_slope_nrt
ICinf_slope_nrt <- window(ts.union(ts(ICinf_slope_nrt, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_slope_nrt <- estslope_ma1nrt + 1.96 * se_est_slope_nrt
ICsup_slope_nrt <- window(ts.union(ts(ICsup_slope_nrt, start = 2012, frequency = 4)), start = c(2013,4))
estslope_ma1nrt <- window(ts.union(ts(estslope_ma1nrt, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(estslope_ma1nrt, type = "l", col = "black", lwd = 2,
     main = "06 - Norte de Minas",
     xlab = "Ano", ylab = "Inclinação da tendência (mil pessoas/trimestre)",
     ylim = c(-20,20))
abline(h = 0, col = "gray40", lwd = 1.5, lty = 3)
lines(ICinf_slope_nrt, col = "black", lty = 2)
lines(ICsup_slope_nrt, col = "black", lty = 2)
legend("topleft", legend = c("Inclinação da tendência: model-based","IC 95%: model-based"),
       col = c("black","black"),lty = c(1, 2),lwd = c(2,1),bty = "n")

### 07 - VALE DO RIO DOCE ######################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/07_mod_val.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/07_mod_val.Rdata", envir = env2)

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

sm_ar1_val<-env1$ar1_vl$ts.signal
sm_ar1_val<-window(ts.union(ts(sm_ar1_val, start = 2012, frequency = 4)), start = c(2013,4))
cv_sm_ar1_val<-env1$ar1_vl$cv.signal
cv_sm_ar1_val<-window(ts.union(ts(cv_sm_ar1_val, start = 2012, frequency = 4)), start = c(2013,4))

est_ar1_val<-env2$ar1_val$ts.signal
se_est_sinal_val<-env2$ar1_val$se.signal
ICinf_sinal_val <- est_ar1_val - 1.96 * se_est_sinal_val
ICinf_sinal_val <- window(ts.union(ts(ICinf_sinal_val, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_sinal_val <- est_ar1_val + 1.96 * se_est_sinal_val
ICsup_sinal_val <- window(ts.union(ts(ICsup_sinal_val, start = 2012, frequency = 4)), start = c(2013,4))
est_ar1_val<-window(ts.union(ts(est_ar1_val, start = 2012, frequency = 4)), start = c(2013,4))
cv_est_ar1_val<-env2$ar1_val$cv.signal
cv_est_ar1_val<-window(ts.union(ts(cv_est_ar1_val, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_val, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(25,225))
lines(sm_ar1_val, col = "blue", lty = 1, lwd = 2)
lines(est_ar1_val, col = "red", lty = 1, lwd = 2)
lines(ICinf_val, col = "black", lty = 2)
lines(ICsup_val, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Smooth",
                             "Sinal da Desocupação MA(1) - Estrutural","IC 95%: signal-based"), 
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_val*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(3,17))
lines(cv_sm_ar1_val, col = "blue",lwd=2,lty = 1)
lines(cv_est_ar1_val, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV AR(1) - Smooth","Sinal CV AR(1) - Estrutural"), 
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("07 - Vale do Rio Doce", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ar1_val, type = "l", col = "red", lwd = 2,
     main = "07-Vale do Rio Doce",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(30,210))
lines(desoc_val, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal_val, col = "red", lty = 2)
lines(ICsup_sinal_val, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural AR(1)","Desocupação: design-based","IC 95%: model-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")


### 08 - CENTRAL ###############################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/08_mod_cen.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/08_mod_cen.Rdata", envir = env2)

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

sm_ma1_cen<-env1$ma1_cen$ts.signal
sm_ma1_cen<-window(ts.union(ts(sm_ma1_cen, start = 2012, frequency = 4)), start = c(2013,4))
cv_sm_ma1_cen<-env1$ma1_cen$cv.signal
cv_sm_ma1_cen<-window(ts.union(ts(cv_sm_ma1_cen, start = 2012, frequency = 4)), start = c(2013,4))

est_ma1_cen<-env2$ma1_cen$ts.signal
se_est_sinal_cen<-env2$ma1_cen$se.signal
ICinf_sinal_cen <- est_ma1_cen - 1.96 * se_est_sinal_cen
ICinf_sinal_cen <- window(ts.union(ts(ICinf_sinal_cen, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_sinal_cen <- est_ma1_cen + 1.96 * se_est_sinal_cen
ICsup_sinal_cen <- window(ts.union(ts(ICsup_sinal_cen, start = 2012, frequency = 4)), start = c(2013,4))
est_ma1_cen<-window(ts.union(ts(est_ma1_cen, start = 2012, frequency = 4)), start = c(2013,4))
cv_est_ma1_cen<-env2$ma1_cen$cv.signal
cv_est_ma1_cen<-window(ts.union(ts(cv_est_ma1_cen, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_cen, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(25,220))
lines(sm_ma1_cen, col = "blue", lty = 1, lwd = 2)
lines(est_ma1_cen, col = "red", lty = 1, lwd = 2)
lines(ICinf_cen, col = "black", lty = 2)
lines(ICsup_cen, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Smooth",
                             "Sinal da Desocupação MA(1) - Estrutural","IC 95%: signal-based"), 
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_cen*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(2,20))
lines(cv_sm_ma1_cen, col = "blue",lwd=2,lty = 1)
lines(cv_est_ma1_cen, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV MA(1) - Smooth","Sinal CV MA(1) - Estrutural"), 
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("08 - Central", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ma1_cen, type = "l", col = "red", lwd = 2,
     main = "08-Central",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(45,270))
lines(desoc_cen, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal_cen, col = "red", lty = 2)
lines(ICsup_sinal_cen, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: model-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")

### 09 - MINAS GERAIS ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/5_smoothdesocup_8reg/09_mod_mg.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/09_mod_mg.Rdata", envir = env2)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
mg<-baseestr8reg$`09 - Minas Gerais`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtmg<-baseal8reg$`09 - Minas Gerais` 
dbmg<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/09_params_mg.RDS")

desoc_mg <- mg$Total.de.desocupados/1000
se_db <- mg$sd_d/1000
cv_mg <- se_db/desoc_mg
ICinf_mg<-desoc_mg-1.96*se_db
ICsup_mg<-desoc_mg+1.96*se_db

desoc_mg <- window(ts.union(ts(desoc_mg, start = 2012, frequency = 4)), start = c(2013,4))
ICinf_mg <- window(ts.union(ts(ICinf_mg, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_mg <- window(ts.union(ts(ICsup_mg, start = 2012, frequency = 4)), start = c(2013,4))
cv_mg <- window(ts.union(ts(cv_mg, start = 2012, frequency = 4)), start = c(2013,4))

sm_ma1_mg<-env1$ma1_mg$ts.signal
sm_ma1_mg<-window(ts.union(ts(sm_ma1_mg, start = 2012, frequency = 4)), start = c(2013,4))
cv_sm_ma1_mg<-env1$ma1_mg$cv.signal
cv_sm_ma1_mg<-window(ts.union(ts(cv_sm_ma1_mg, start = 2012, frequency = 4)), start = c(2013,4))

est_ma1_mg<-env2$ma1_mg$ts.signal
se_est_sinal_mg<-env2$ma1_mg$se.signal
ICinf_sinal_mg <- est_ma1_mg - 1.96 * se_est_sinal_mg
ICinf_sinal_mg <- window(ts.union(ts(ICinf_sinal_mg, start = 2012, frequency = 4)), start = c(2013,4))
ICsup_sinal_mg <- est_ma1_mg + 1.96 * se_est_sinal_mg
ICsup_sinal_mg <- window(ts.union(ts(ICsup_sinal_mg, start = 2012, frequency = 4)), start = c(2013,4))
est_ma1_mg<-window(ts.union(ts(est_ma1_mg, start = 2012, frequency = 4)), start = c(2013,4))
cv_est_ma1_mg<-env2$ma1_mg$cv.signal
cv_est_ma1_mg<-window(ts.union(ts(cv_est_ma1_mg, start = 2012, frequency = 4)), start = c(2013,4))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(desoc_mg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(400,1700))
lines(sm_ma1_mg, col = "blue", lty = 1, lwd = 2)
lines(est_ma1_mg, col = "red", lty = 1, lwd = 2)
lines(ICinf_mg, col = "black", lty = 2)
lines(ICsup_mg, col = "black", lty = 2)
legend("topleft", legend = c("Desocupação: design-based", "Sinal da Desocupação MA(1) - Smooth",
                             "Sinal da Desocupação MA(1) - Estrutural","IC 95%: signal-based"), 
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_mg*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(1.5,6))
lines(cv_sm_ma1_mg, col = "blue",lwd=2,lty = 1)
lines(cv_est_ma1_mg, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV desocupados: design based","Sinal CV MA(1) - Smooth","Sinal CV MA(1) - Estrutural"), 
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("09 - Minas Gerais", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ma1_mg, type = "l", col = "red", lwd = 2,
     main = "09 - Minas Gerais",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(450,1700))
lines(desoc_mg, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal_mg, col = "red", lty = 2)
lines(ICsup_sinal_mg, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: model-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")

