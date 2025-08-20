################################################################################
##          COMPARATIVO MODELOS SMOOTH E ESTRUTURAL - OCUPADOS                ##
################################################################################

## Para visualizar os gráficos em segunda tela:

dev.new()
dev.new()

### 01 - BELO HORIZONTE ########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/7_smoothocup_8reg/01_mod_bh.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/01_mod_bh.Rdata", envir = env2)

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

sm_ar1_bh<-env1$ar1_bh$ts.signal
sm_ar1_bh<-window(ts.union(ts(sm_ar1_bh, start = 2012, frequency = 4)), start = c(2013,3))
cv_sm_ar1_bh<-env1$ar1_bh$cv.signal
cv_sm_ar1_bh<-window(ts.union(ts(cv_sm_ar1_bh, start = 2012, frequency = 4)), start = c(2013,3))

est_ar1_bh<-env2$ar1_bh$ts.signal
se_est_sinal_bh<-env2$ar1_bh$se.signal
ICinf_sinal <- est_ar1_bh - 1.96 * se_est_sinal_bh
ICinf_sinal <- window(ts.union(ts(ICinf_sinal, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_sinal <- est_ar1_bh + 1.96 * se_est_sinal_bh
ICsup_sinal <- window(ts.union(ts(ICsup_sinal, start = 2012, frequency = 4)), start = c(2013,3))
est_ar1_bh<-window(ts.union(ts(est_ar1_bh, start = 2012, frequency = 4)), start = c(2013,3))
cv_est_ar1_bh<-env2$ar1_bh$cv.signal
cv_est_ar1_bh<-window(ts.union(ts(cv_est_ar1_bh, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_bh, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(1000,1600))
lines(sm_ar1_bh, col = "blue", lty = 1, lwd = 2)
lines(est_ar1_bh, col = "red", lty = 1, lwd = 2)
lines(ICinf_bh, col = "black", lty = 2)
lines(ICsup_bh, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação: design-based", "Sinal da Ocupação - Smooth",
                             "Sinal da Ocupação - Estrutural","IC 95%: design-based"), 
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_bh*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(1,3))
lines(cv_sm_ar1_bh, col = "blue",lwd=2,lty = 1)
lines(cv_est_ar1_bh, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV ocupados: design-based","Sinal CV - Smooth","Sinal CV - Estrutural"), 
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("01 - Belo Horizonte (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO IC SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ar1_bh, type = "l", col = "red", lwd = 2,
     main = "01-Belo Horizonte",
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(1100,1500))
lines(ocup_bh, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Ocupação - Estrutural AR(1)","Ocupação: design-based","IC 95%: signal-based"), 
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")



### 02 - COLAR E ENTORNO METROPOLITANO DE BELO HORIZONTE #######################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/7_smoothocup_8reg/02_mod_ent.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/02_mod_ent.Rdata", envir = env2)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
ent<-baseestr8reg$`02-Colar e Entorno metropolitano de BH`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtent<-baseal8reg$`02-Colar e Entorno Metropolitano de BH`
dbent<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/02_params_ent.RDS")

ocup_ent <- (ent$Total.de.ocupados)/1000
se_db <- (ent$sd_o)/1000
cv_ent <- se_db/ocup_ent
ICinf_ent<-ocup_ent-1.96*se_db
ICsup_ent<-ocup_ent+1.96*se_db

ocup_ent <- window(ts.union(ts(ocup_ent, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_ent <- window(ts.union(ts(ICinf_ent, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_ent <- window(ts.union(ts(ICsup_ent, start = 2012, frequency = 4)), start = c(2013,3))
cv_ent <- window(ts.union(ts(cv_ent, start = 2012, frequency = 4)), start = c(2013,3))

sm_ar1_ent<-env1$ar1_ent$ts.signal
sm_ar1_ent<-window(ts.union(ts(sm_ar1_ent, start = 2012, frequency = 4)), start = c(2013,3))
cv_sm_ar1_ent<-env1$ar1_ent$cv.signal
cv_sm_ar1_ent<-window(ts.union(ts(cv_sm_ar1_ent, start = 2012, frequency = 4)), start = c(2013,3))

est_ar1_ent<-env2$ar1_ent$ts.signal
se_est_sinal_ent<-env2$ar1_ent$se.signal
ICinf_sinal <- est_ar1_ent - 1.96 * se_est_sinal_ent
ICinf_sinal <- window(ts.union(ts(ICinf_sinal, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_sinal <- est_ar1_ent + 1.96 * se_est_sinal_ent
ICsup_sinal <- window(ts.union(ts(ICsup_sinal, start = 2012, frequency = 4)), start = c(2013,3))
est_ar1_ent<-window(ts.union(ts(est_ar1_ent, start = 2012, frequency = 4)), start = c(2013,3))
cv_est_ar1_ent<-env2$ar1_ent$cv.signal
cv_est_ar1_ent<-window(ts.union(ts(cv_est_ar1_ent, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_ent, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(1300,2100))
lines(sm_ar1_ent, col = "blue", lty = 1, lwd = 2)
lines(est_ar1_ent, col = "red", lty = 1, lwd = 2)
lines(ICinf_ent, col = "black", lty = 2)
lines(ICsup_ent, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação: design-based", "Sinal da Ocupação - Smooth",
                             "Sinal da Ocupação - Estrutural","IC 95%: design-based"),
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_ent*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(1,3.5))
lines(cv_sm_ar1_ent, col = "blue",lwd=2,lty = 1)
lines(cv_est_ar1_ent, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV ocupados: design-based","Sinal CV - Smooth","Sinal CV - Estrutural"),
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte AR(1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ar1_ent, type = "l", col = "red", lwd = 2,
     main = "02-Colar e Entorno metropolitano de Belo Horizonte",
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(1400,2080))
lines(ocup_ent, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Ocupação - Estrutural AR(1)","Ocupação: design-based","IC 95%: signal-based"), 
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")

### 03 - SUL DE MINAS ##########################################################

rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/7_smoothocup_8reg/03_mod_sul.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/03_mod_sul.Rdata", envir = env2)
  
baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
sul<-baseestr8reg$`03-Sul de Minas`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtsul<-baseal8reg$`03-Sul de Minas` ## Arquivo "cru", saída direta da rotina da base por rotação
dbsul<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/03_params_sul.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

ocup_sul <- sul$Total.de.ocupados/1000
se_db <- sul$sd_o/1000
cv_sul <- se_db/ocup_sul
ICinf_sul<-ocup_sul-1.96*se_db
ICsup_sul<-ocup_sul+1.96*se_db

ocup_sul <- window(ts.union(ts(ocup_sul, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_sul <- window(ts.union(ts(ICinf_sul, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_sul <- window(ts.union(ts(ICsup_sul, start = 2012, frequency = 4)), start = c(2013,3))
cv_sul <- window(ts.union(ts(cv_sul, start = 2012, frequency = 4)), start = c(2013,3))

sm_ar1_sul<-env1$ar1_sul$ts.signal
sm_ar1_sul<-window(ts.union(ts(sm_ar1_sul, start = 2012, frequency = 4)), start = c(2013,3))
cv_sm_ar1_sul<-env1$ar1_sul$cv.signal
cv_sm_ar1_sul<-window(ts.union(ts(cv_sm_ar1_sul, start = 2012, frequency = 4)), start = c(2013,3))

est_ar1_sul<-env2$ar1_sul$ts.signal
se_est_sinal_sul<-env2$ar1_sul$se.signal
ICinf_sinal <- est_ar1_sul - 1.96 * se_est_sinal_sul
ICinf_sinal <- window(ts.union(ts(ICinf_sinal, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_sinal <- est_ar1_sul + 1.96 * se_est_sinal_sul
ICsup_sinal <- window(ts.union(ts(ICsup_sinal, start = 2012, frequency = 4)), start = c(2013,3))
est_ar1_sul<-window(ts.union(ts(est_ar1_sul, start = 2012, frequency = 4)), start = c(2013,3))
cv_est_ar1_sul<-env2$ar1_sul$cv.signal
cv_est_ar1_sul<-window(ts.union(ts(cv_est_ar1_sul, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_sul, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(1000,1650))
lines(sm_ar1_sul, col = "blue", lty = 1, lwd = 2)
lines(est_ar1_sul, col = "red", lty = 1, lwd = 2)
lines(ICinf_sul, col = "black", lty = 2)
lines(ICsup_sul, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação: design-based", "Sinal da Ocupação - Smooth",
                             "Sinal da Ocupação - Estrutural","IC 95%: design-based"),
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_sul*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(1,8))
lines(cv_sm_ar1_sul, col = "blue",lwd=2,lty = 1)
lines(cv_est_ar1_sul, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV ocupados: design-based","Sinal CV - Smooth","Sinal CV - Estrutural"),
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("03 - Sul de Minas AR(1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ar1_sul, type = "l", col = "red", lwd = 2,
     main = "03-Sul de Minas",
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(1080,1550))
lines(ocup_sul, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Ocupação - Estrutural AR(1)","Ocupação: design-based","IC 95%: signal-based"), 
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")

### 04 - TRIÂNGULO MINEIRO #####################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/7_smoothocup_8reg/04_mod_trg.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/04_mod_trg.Rdata", envir = env2)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
trg<-baseestr8reg$`04-Triângulo Mineiro`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dttrg<-baseal8reg$`04-Triângulo Mineiro` 
dbtrg<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/04_params_trg.RDS") 

ocup_trg <- trg$Total.de.ocupados/1000
se_db <- trg$sd_o/1000
cv_trg <- se_db/ocup_trg
ICinf_trg<-ocup_trg-1.96*se_db
ICsup_trg<-ocup_trg+1.96*se_db

ocup_trg <- window(ts.union(ts(ocup_trg, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_trg <- window(ts.union(ts(ICinf_trg, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_trg <- window(ts.union(ts(ICsup_trg, start = 2012, frequency = 4)), start = c(2013,3))
cv_trg <- window(ts.union(ts(cv_trg, start = 2012, frequency = 4)), start = c(2013,3))

sm_ar1_trg<-env1$ar1_trg$ts.signal
sm_ar1_trg<-window(ts.union(ts(sm_ar1_trg, start = 2012, frequency = 4)), start = c(2013,3))
cv_sm_ar1_trg<-env1$ar1_trg$cv.signal
cv_sm_ar1_trg<-window(ts.union(ts(cv_sm_ar1_trg, start = 2012, frequency = 4)), start = c(2013,3))

est_ar1_trg<-env2$ar1_trg$ts.signal
se_est_sinal_trg<-env2$ar1_trg$se.signal
ICinf_sinal <- est_ar1_trg - 1.96 * se_est_sinal_trg
ICinf_sinal <- window(ts.union(ts(ICinf_sinal, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_sinal <- est_ar1_trg + 1.96 * se_est_sinal_trg
ICsup_sinal <- window(ts.union(ts(ICsup_sinal, start = 2012, frequency = 4)), start = c(2013,3))
est_ar1_trg<-window(ts.union(ts(est_ar1_trg, start = 2012, frequency = 4)), start = c(2013,3))
cv_est_ar1_trg<-env2$ar1_trg$cv.signal
cv_est_ar1_trg<-window(ts.union(ts(cv_est_ar1_trg, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_trg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(1000,1850))
lines(sm_ar1_trg, col = "blue", lty = 1, lwd = 2)
lines(est_ar1_trg, col = "red", lty = 1, lwd = 2)
lines(ICinf_trg, col = "black", lty = 2)
lines(ICsup_trg, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação: design-based", "Sinal da Ocupação - Smooth",
                             "Sinal da Ocupação - Estrutural","IC 95%: design-based"),
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_trg*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(2,15))
lines(cv_sm_ar1_trg, col = "blue",lwd=2,lty = 1)
lines(cv_est_ar1_trg, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV ocupados: design-based","Sinal CV - Smooth","Sinal CV - Estrutural"),
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("04 - Triângulo Mineiro AR(1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO SLOPE

#sl_ar1_trg<-mods$`04-Triângulo Mineiro`$slope_estrutural_ar1trg

#se_slope_trg<-ar1_trg$se.slope

#ICinf_slope <- sl_ar1_trg - 1.96 * se_slope_trg
#ICsup_slope <- sl_ar1_trg + 1.96 * se_slope_trg

#ICinf_slope <- window(ts(ICinf_slope,start = 2012, frequency = 4), start = c(2013,3))
#ICsup_slope <- window(ts(ICsup_slope,start = 2012, frequency = 4), start = c(2013,3))

#fig_slope<-window(ts.union(ts(sl_ar1_trg, start = 2012, frequency = 4)), start = c(2013, 3))

#par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
#plot(fig_slope, type = "l", col = "blue", lwd = 2,
#     main = "04-Triangulo Mineiro",
#     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
#     ylim = c(-25, 70))
#lines(ICinf_slope, col = "red", lty = 2)
#lines(ICsup_slope, col = "red", lty = 2)
#legend("topright", legend = c("Inclinação da tendência do total de ocupados", "IC 95%"), col = c("blue", "red"),lty = c(1, 2),lwd = c(2, 1),bty = "n")


## Tentando fazer com a série de ocupação

#win.ocup_trg<-window(ts(ocup_trg,start = 2012, frequency = 4), start = c(2013,3))
#win.trend_trg<-window(ts(ar1_trg$ts.trend,start = 2012, frequency = 4), start = c(2013,3))
#win.ICinf_trg <- window(ts(ICinf_trg,start = 2012, frequency = 4), start = c(2013,3))
#win.ICsup_trg <- window(ts(ICsup_trg,start = 2012, frequency = 4), start = c(2013,3))

#par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
#plot(win.ocup_trg, type = "l", col = "black", lwd = 2,
#     main = "04-Triangulo Mineiro",
#     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
#     ylim = c(1000,1900))
#lines(win.trend_trg, col = "blue",lwd = 2, lty = 1)
#lines(win.ICinf_trg, col = "red", lty = 2)
#lines(win.ICsup_trg, col = "red", lty = 2)
#legend("topright", legend = c("Total de ocupados (milhares de pessoas)","Tendência da ocupação: model", "IC 95%"),
#       col = c("black","blue", "red"),lty = c(1, 1, 2),lwd = c(2, 2, 1),bty = "n")


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ar1_trg, type = "l", col = "red", lwd = 2,
     main = "04-Triângulo Mineiro",
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(600,1680))
lines(ocup_trg, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Ocupação - Estrutural AR(1)","Ocupação: design-based","IC 95%: signal-based"), 
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")

### 05 - ZONA DA MATA ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/7_smoothocup_8reg/05_mod_mat.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/05_mod_mat.Rdata", envir = env2)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
mat<-baseestr8reg$`05-Mata de Minas Gerais`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtmat<-baseal8reg$`05-Mata de Minas Gerais`
dbmat<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/05_params_mat.RDS")

ocup_mat<- mat$Total.de.ocupados/1000
se_db <- mat$sd_o/1000
cv_mat <- se_db/ocup_mat
ICinf_mat<-ocup_mat-1.96*se_db
ICsup_mat<-ocup_mat+1.96*se_db

ocup_mat <- window(ts.union(ts(ocup_mat, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_mat <- window(ts.union(ts(ICinf_mat, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_mat <- window(ts.union(ts(ICsup_mat, start = 2012, frequency = 4)), start = c(2013,3))
cv_mat <- window(ts.union(ts(cv_mat, start = 2012, frequency = 4)), start = c(2013,3))

sm_ar1_mat<-env1$ar1_mat$ts.signal
sm_ar1_mat<-window(ts.union(ts(sm_ar1_mat, start = 2012, frequency = 4)), start = c(2013,3))
cv_sm_ar1_mat<-env1$ar1_mat$cv.signal
cv_sm_ar1_mat<-window(ts.union(ts(cv_sm_ar1_mat, start = 2012, frequency = 4)), start = c(2013,3))

est_ar1_mat<-env2$ar1_mat$ts.signal
se_est_sinal_mat<-env2$ar1_mat$se.signal
ICinf_sinal <- est_ar1_mat - 1.96 * se_est_sinal_mat
ICinf_sinal <- window(ts.union(ts(ICinf_sinal, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_sinal <- est_ar1_mat + 1.96 * se_est_sinal_mat
ICsup_sinal <- window(ts.union(ts(ICsup_sinal, start = 2012, frequency = 4)), start = c(2013,3))
est_ar1_mat<-window(ts.union(ts(est_ar1_mat, start = 2012, frequency = 4)), start = c(2013,3))
cv_est_ar1_mat<-env2$ar1_mat$cv.signal
cv_est_ar1_mat<-window(ts.union(ts(cv_est_ar1_mat, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_mat, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(800,1400))
lines(sm_ar1_mat, col = "blue", lty = 1, lwd = 2)
lines(est_ar1_mat, col = "red", lty = 1, lwd = 2)
lines(ICinf_mat, col = "black", lty = 2)
lines(ICsup_mat, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação: design-based", "Sinal da Ocupação - Smooth",
                             "Sinal da Ocupação - Estrutural","IC 95%: design-based"),
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_mat*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(1.5,7))
lines(cv_sm_ar1_mat, col = "blue",lwd=2,lty = 1)
lines(cv_est_ar1_mat, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV ocupados: design-based","Sinal CV - Smooth","Sinal CV - Estrutural"),
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("05 - Zona da Mata AR(1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ar1_mat, type = "l", col = "red", lwd = 2,
     main = "05-Zona da Mata",
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(880,1300))
lines(ocup_mat, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Ocupação - Estrutural AR(1)","Ocupação: design-based","IC 95%: signal-based"), 
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")

### 06 - NORTE DE MINAS ########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/7_smoothocup_8reg/06_mod_nrt.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/06_mod_nrt.Rdata", envir = env2)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
nrt<-baseestr8reg$`06-Norte de Minas`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtnrt<-baseal8reg$`06-Norte de Minas`
dbnrt<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/06_params_nrt.RDS") 

ocup_nrt <- nrt$Total.de.ocupados/1000
se_db <- nrt$sd_o/1000
cv_nrt <- se_db/ocup_nrt
ICinf_nrt<-ocup_nrt-1.96*se_db
ICsup_nrt<-ocup_nrt+1.96*se_db

ocup_nrt <- window(ts.union(ts(ocup_nrt, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_nrt <- window(ts.union(ts(ICinf_nrt, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_nrt <- window(ts.union(ts(ICsup_nrt, start = 2012, frequency = 4)), start = c(2013,3))
cv_nrt <- window(ts.union(ts(cv_nrt, start = 2012, frequency = 4)), start = c(2013,3))

sm_ar1_nrt<-env1$ar1_nrt$ts.signal
sm_ar1_nrt<-window(ts.union(ts(sm_ar1_nrt, start = 2012, frequency = 4)), start = c(2013,3))
cv_sm_ar1_nrt<-env1$ar1_nrt$cv.signal
cv_sm_ar1_nrt<-window(ts.union(ts(cv_sm_ar1_nrt, start = 2012, frequency = 4)), start = c(2013,3))

est_ar1_nrt<-env2$ar1_nrt$ts.signal
se_est_sinal_nrt<-env2$ar1_nrt$se.signal
ICinf_sinal <- est_ar1_nrt - 1.96 * se_est_sinal_nrt
ICinf_sinal <- window(ts.union(ts(ICinf_sinal, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_sinal <- est_ar1_nrt + 1.96 * se_est_sinal_nrt
ICsup_sinal <- window(ts.union(ts(ICsup_sinal, start = 2012, frequency = 4)), start = c(2013,3))
est_ar1_nrt<-window(ts.union(ts(est_ar1_nrt, start = 2012, frequency = 4)), start = c(2013,3))
cv_est_ar1_nrt<-env2$ar1_nrt$cv.signal
cv_est_ar1_nrt<-window(ts.union(ts(cv_est_ar1_nrt, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_nrt, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(800,1500))
lines(sm_ar1_nrt, col = "blue", lty = 1, lwd = 2)
lines(est_ar1_nrt, col = "red", lty = 1, lwd = 2)
lines(ICinf_nrt, col = "black", lty = 2)
lines(ICsup_nrt, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação: design-based", "Sinal da Ocupação - Smooth",
                             "Sinal da Ocupação - Estrutural","IC 95%: design-based"),
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_nrt*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(2,8))
lines(cv_sm_ar1_nrt, col = "blue",lwd=2,lty = 1)
lines(cv_est_ar1_nrt, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV ocupados: design-based","Sinal CV - Smooth","Sinal CV - Estrutural"),
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("06 - Norte de Minas AR(1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ar1_nrt, type = "l", col = "red", lwd = 2,
     main = "06-Norte de Minas",
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(850,1400))
lines(ocup_nrt, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Ocupação - Estrutural AR(1)","Ocupação: design-based","IC 95%: signal-based"), 
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")

## Gráfico tendência com IC

esttrend_ar1nrt <- env2$ar1_nrt$ts.trend
se_est_trend_nrt<-env2$ar1_nrt$se.trend
ICinf_trend_nrt <- esttrend_ar1nrt - 1.96 * se_est_trend_nrt
ICinf_trend_nrt <- window(ts.union(ts(ICinf_trend_nrt, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_trend_nrt <- esttrend_ar1nrt + 1.96 * se_est_trend_nrt
ICsup_trend_nrt <- window(ts.union(ts(ICsup_trend_nrt, start = 2012, frequency = 4)), start = c(2013,3))
esttrend_ar1nrt <- window(ts.union(ts(esttrend_ar1nrt, start = 2012, frequency = 4)), start = c(2013,3))
cv_esttrend_nrt <- env2$ar1_nrt$cv.trend
cv_esttrend_nrt <- window(ts.union(ts(cv_esttrend_nrt, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,3,1), oma=c(0,0,2,0), cex=0.8)
plot(esttrend_ar1nrt, type = "l", col = "red", lwd = 2,
     main = "Tendência",
     xlab = "Ano", ylab = "Total de ocupados (mil pessoas)",
     ylim = c(900,1500))
lines(ocup_nrt, col = "black", lty = 1, lwd = 2)
lines(ICinf_trend_nrt, col = "red", lty = 2)
lines(ICsup_trend_nrt, col = "red", lty = 2)
legend("topleft", legend = c("Tendência: model-based","Estimativa direta","IC 95%: tendência"),
       col = c("red","black", "red"), lty = c(1,1, 2), lwd = c(2,2,1), bty = "n")

plot((cv_nrt*100), type = "l", col = "black", lwd = 2,
     main = "Coeficiente de Variação (CV) da tendência",
     xlab = "Ano", ylab = "CV(%)",
     ylim = c(2,8))
lines(cv_esttrend_nrt, col = "red", lty = 1, lwd = 2)
legend("topleft", legend = c("CV: estimativa direta","CV: model-based"),
       col = c("black","red"), lty = c(1,1), lwd = c(2,2), bty = "n")
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## Sinal da série

par(mfrow=c(1,2), mar=c(5,5,3,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_nrt, type = "l", col = "black", lwd = 2,
     main = "Sinal",
     xlab = "Ano", ylab = "Total de ocupados (mil pessoas)",
     ylim = c(900,1500))
lines(est_ar1_nrt, col = "blue", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "black", lty = 2)
lines(ICsup_sinal, col = "black", lty = 2)
legend("topleft", legend = c("Estimativa direta","Sinal: model-based","IC 95%: estimativa direta"),
       col = c("black","blue", "black"), lty = c(1,1, 2), lwd = c(2,2,1), bty = "n")

plot((cv_nrt*100), type = "l", col = "black", lwd = 2,
     main = "Coeficiente de Variação (CV)",
     xlab = "Ano", ylab = "CV(%)",
     ylim = c(2,8))
lines(cv_est_ar1_nrt, col = "blue", lty = 1, lwd = 2)
legend("topleft", legend = c("CV: estimativa direta","CV: model-based"),
       col = c("black","blue"), lty = c(1,1), lwd = c(2,2), bty = "n")
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## Gráfico 4 plots:

figttrend_nrt<-window(ts.union(ts(env2$ar1_nrt$ts.original, start = 2012, frequency = 4),ts(env2$ar1_nrt$ts.trend, start = 2012, frequency = 4)), start = c(2013, 3))
figsaz_nrt<-window(ts.union(ts(env2$ar1_nrt$ts.seasonal, start = 2012, frequency = 4)), start = c(2013, 3))
figirr_nrt<-window(ts.union(ts(env2$ar1_nrt$ts.irregular, start = 2012, frequency = 4)), start = c(2013, 3))
figsample_nrt<-window(ts.union(ts(env2$ar1_nrt$ts.sampling_error, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow = c(2, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
plot(figttrend_nrt, plot.type = "single", col = c(1, 2), ylab = "", xlab = "", lty = c(1, 1), lwd = c(2))
legend("bottomleft", legend = c("Ocupação: design-based",
                            "Tendência da ocupação: model-based"),
       lty = c(1, 1), col = c(1, 2), bty = 'n', lwd = c(2))
mtext("Ocupados (mil pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsaz_nrt, plot.type = "single", col = c(1), ylab = "", xlab = "", lty = c(1), lwd = c(2), ylim=c(-60,40))
legend("bottomleft", legend = c("Sazonalidade"),
       lty = c(1), col = c(1), bty = 'n', lwd = c(2))
mtext("Ocupados (mil pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figirr_nrt, plot.type = "single", col = c(1), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Termo irregular"),
       lty = c(1), col = c(1), bty = 'n', lwd = c(2))
mtext("Ocupados (mil pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

plot(figsample_nrt, plot.type = "single", col = c(1), ylab = "", xlab = "", lty = c(1), lwd = c(2))
legend("bottomleft", legend = c("Erro amostral"),
       lty = c(1), col = c(1), bty = 'n', lwd = c(2))
mtext("Ocupados (mil pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)

## Slope da tendência com IC

estslope_ar1nrt <- env2$ar1_nrt$ts.slope
se_est_slope_nrt <- env2$ar1_nrt$se.slope
ICinf_slope_nrt <- estslope_ar1nrt - 1.96 * se_est_slope_nrt
ICinf_slope_nrt <- window(ts.union(ts(ICinf_slope_nrt, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_slope_nrt <- estslope_ar1nrt + 1.96 * se_est_slope_nrt
ICsup_slope_nrt <- window(ts.union(ts(ICsup_slope_nrt, start = 2012, frequency = 4)), start = c(2013,3))
estslope_ar1nrt <- window(ts.union(ts(estslope_ar1nrt, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(estslope_ar1nrt, type = "l", col = "black", lwd = 2,
     main = "06 - Norte de Minas",
     xlab = "Ano", ylab = "Inclinação da tendência (mil pessoas/trimestre)",
     ylim = c(-90,60))
abline(h = 0, col = "gray40", lwd = 1.5, lty = 3)
lines(ICinf_slope_nrt, col = "black", lty = 2)
lines(ICsup_slope_nrt, col = "black", lty = 2)
legend("topleft", legend = c("Inclinação da tendência: model-based","IC 95%: model-based"),
       col = c("black","black"),lty = c(1, 2),lwd = c(2,1),bty = "n")

### 07 - VALE DO RIO DOCE ######################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/7_smoothocup_8reg/07_mod_val.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/07_mod_val.Rdata", envir = env2)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
vl<-baseestr8reg$`07-Vale do Rio Doce`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtvl<-baseal8reg$`07-Vale do Rio Doce`
dbvl<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/07_params_rio.RDS") 

ocup_val <- vl$Total.de.ocupados/1000
se_db <- vl$sd_o/1000
cv_val <- se_db/ocup_val
ICinf_val<-ocup_val-1.96*se_db
ICsup_val<-ocup_val+1.96*se_db

ocup_val <- window(ts.union(ts(ocup_val, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_val <- window(ts.union(ts(ICinf_val, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_val <- window(ts.union(ts(ICsup_val, start = 2012, frequency = 4)), start = c(2013,3))
cv_val <- window(ts.union(ts(cv_val, start = 2012, frequency = 4)), start = c(2013,3))

sm_ar1_val<-env1$ar1_vl$ts.signal
sm_ar1_val<-window(ts.union(ts(sm_ar1_val, start = 2012, frequency = 4)), start = c(2013,3))
cv_sm_ar1_val<-env1$ar1_vl$cv.signal
cv_sm_ar1_val<-window(ts.union(ts(cv_sm_ar1_val, start = 2012, frequency = 4)), start = c(2013,3))

est_ar1_val<-env2$ar1_val$ts.signal
se_est_sinal_val<-env2$ar1_val$se.signal
ICinf_sinal <- est_ar1_val - 1.96 * se_est_sinal_val
ICinf_sinal <- window(ts.union(ts(ICinf_sinal, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_sinal <- est_ar1_val + 1.96 * se_est_sinal_val
ICsup_sinal <- window(ts.union(ts(ICsup_sinal, start = 2012, frequency = 4)), start = c(2013,3))
est_ar1_val<-window(ts.union(ts(est_ar1_val, start = 2012, frequency = 4)), start = c(2013,3))
cv_est_ar1_val<-env2$ar1_val$cv.signal
cv_est_ar1_val<-window(ts.union(ts(cv_est_ar1_val, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_val, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(700,1300))
lines(sm_ar1_val, col = "blue", lty = 1, lwd = 2)
lines(est_ar1_val, col = "red", lty = 1, lwd = 2)
lines(ICinf_val, col = "black", lty = 2)
lines(ICsup_val, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação: design-based", "Sinal da Ocupação - Smooth",
                             "Sinal da Ocupação - Estrutural","IC 95%: design-based"),
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_val*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(2,8))
lines(cv_sm_ar1_val, col = "blue",lwd=2,lty = 1)
lines(cv_est_ar1_val, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV ocupados: design-based","Sinal CV - Smooth","Sinal CV - Estrutural"),
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("07 - Vale do Rio Doce (AR1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ar1_val, type = "l", col = "red", lwd = 2,
     main = "07-Vale do Rio Doce",
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(600,1150))
lines(ocup_val, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Ocupação - Estrutural AR(1)","Ocupação: design-based","IC 95%: signal-based"), 
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")


### 08 - CENTRAL ###############################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/7_smoothocup_8reg/08_mod_cen.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/08_mod_cen.Rdata", envir = env2)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
cen<-baseestr8reg$`08-Central`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtcen<-baseal8reg$`08-Central`
dbcen<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/08_params_cen.RDS") 

ocup_cen <- cen$Total.de.ocupados/1000
se_db <- cen$sd_o/1000
cv_cen <- se_db/ocup_cen
ICinf_cen<-ocup_cen-1.96*se_db
ICsup_cen<-ocup_cen+1.96*se_db

ocup_cen <- window(ts.union(ts(ocup_cen, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_cen <- window(ts.union(ts(ICinf_cen, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_cen <- window(ts.union(ts(ICsup_cen, start = 2012, frequency = 4)), start = c(2013,3))
cv_cen <- window(ts.union(ts(cv_cen, start = 2012, frequency = 4)), start = c(2013,3))

sm_ar1_cen<-env1$ar1_cen$ts.signal
sm_ar1_cen<-window(ts.union(ts(sm_ar1_cen, start = 2012, frequency = 4)), start = c(2013,3))
cv_sm_ar1_cen<-env1$ar1_cen$cv.signal
cv_sm_ar1_cen<-window(ts.union(ts(cv_sm_ar1_cen, start = 2012, frequency = 4)), start = c(2013,3))

est_ar1_cen<-env2$ar1_cen$ts.signal
se_est_sinal_cen<-env2$ar1_cen$se.signal
ICinf_sinal <- est_ar1_cen - 1.96 * se_est_sinal_cen
ICinf_sinal <- window(ts.union(ts(ICinf_sinal, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_sinal <- est_ar1_cen + 1.96 * se_est_sinal_cen
ICsup_sinal <- window(ts.union(ts(ICsup_sinal, start = 2012, frequency = 4)), start = c(2013,3))
est_ar1_cen<-window(ts.union(ts(est_ar1_cen, start = 2012, frequency = 4)), start = c(2013,3))
cv_est_ar1_cen<-env2$ar1_cen$cv.signal
cv_est_ar1_cen<-window(ts.union(ts(cv_est_ar1_cen, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_cen, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(900,1700))
lines(sm_ar1_cen, col = "blue", lty = 1, lwd = 2)
lines(est_ar1_cen, col = "red", lty = 1, lwd = 2)
lines(ICinf_cen, col = "black", lty = 2)
lines(ICsup_cen, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação: design-based", "Sinal da Ocupação - Smooth",
                             "Sinal da Ocupação - Estrutural","IC 95%: design-based"),
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_cen*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(0,7))
lines(cv_sm_ar1_cen, col = "blue",lwd=2,lty = 1)
lines(cv_est_ar1_cen, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV ocupados: design-based","Sinal CV - Smooth","Sinal CV - Estrutural"),
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("08 - Central AR(1)", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ar1_cen, type = "l", col = "red", lwd = 2,
     main = "08-Central",
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(1000,1500))
lines(ocup_cen, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Ocupação - Estrutural AR(1)","Ocupação: design-based","IC 95%: signal-based"), 
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")

### 09 - MINAS GERAIS ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/7_smoothocup_8reg/09_mod_mg.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/09_mod_mg.Rdata", envir = env2)

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
mg<-baseestr8reg$`09 - Minas Gerais`
baseal8reg<- readRDS("C:/FJP2425/Programacao/data/basealinhada_8reg.RDS")
dtmg<-baseal8reg$`09 - Minas Gerais` 
dbmg<-readRDS("C:/FJP2425/Programacao/data/pseudoerros_8reg/09_params_mg.RDS")

ocup_mg <- mg$Total.de.ocupados/1000
se_db <- mg$sd_o/1000
cv_mg <- se_db/ocup_mg
ICinf_mg<-ocup_mg-1.96*se_db
ICsup_mg<-ocup_mg+1.96*se_db

ocup_mg <- window(ts.union(ts(ocup_mg, start = 2012, frequency = 4)), start = c(2013,3))
ICinf_mg <- window(ts.union(ts(ICinf_mg, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_mg <- window(ts.union(ts(ICsup_mg, start = 2012, frequency = 4)), start = c(2013,3))
cv_mg <- window(ts.union(ts(cv_mg, start = 2012, frequency = 4)), start = c(2013,3))

sm_ar1_mg<-env1$ar1_mg$ts.signal
sm_ar1_mg<-window(ts.union(ts(sm_ar1_mg, start = 2012, frequency = 4)), start = c(2013,3))
cv_sm_ar1_mg<-env1$ar1_mg$cv.signal
cv_sm_ar1_mg<-window(ts.union(ts(cv_sm_ar1_mg, start = 2012, frequency = 4)), start = c(2013,3))

est_ar1_mg<-env2$ar1_mg$ts.signal
se_est_sinal_mg<-env2$ar1_mg$se.signal
ICinf_sinal <- est_ar1_mg - 1.96 * se_est_sinal_mg
ICinf_sinal <- window(ts.union(ts(ICinf_sinal, start = 2012, frequency = 4)), start = c(2013,3))
ICsup_sinal <- est_ar1_mg + 1.96 * se_est_sinal_mg
ICsup_sinal <- window(ts.union(ts(ICsup_sinal, start = 2012, frequency = 4)), start = c(2013,3))
est_ar1_mg<-window(ts.union(ts(est_ar1_mg, start = 2012, frequency = 4)), start = c(2013,3))
cv_est_ar1_mg<-env2$ar1_mg$cv.signal
cv_est_ar1_mg<-window(ts.union(ts(cv_est_ar1_mg, start = 2012, frequency = 4)), start = c(2013,3))

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
plot(ocup_mg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(8000,11500))
lines(sm_ar1_mg, col = "blue", lty = 1, lwd = 2)
lines(est_ar1_mg, col = "red", lty = 1, lwd = 2)
lines(ICinf_mg, col = "black", lty = 2)
lines(ICsup_mg, col = "black", lty = 2)
legend("topleft", legend = c("Ocupação: design-based", "Sinal da Ocupação - Smooth",
                             "Sinal da Ocupação - Estrutural","IC 95%: design-based"),
       col = c("black","blue", "red","black"),lty = c(1,1,1,2),lwd = c(2,2,2,1),bty = "n", cex=0.8)

plot((cv_mg*100), type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(0,3))
lines(cv_sm_ar1_mg, col = "blue",lwd=2,lty = 1)
lines(cv_est_ar1_mg, col = "red",lwd=2, lty = 1)
legend("topleft", legend = c("CV ocupados: design-based","Sinal CV - Smooth","Sinal CV - Estrutural"),
       col = c("black","blue", "red"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)
mtext("09 - Minas Gerais", side = 3, outer = TRUE, line = 0.5)


### GRÁFICO COM IC DO SINAL

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(est_ar1_mg, type = "l", col = "red", lwd = 2,
     main = "09 - Minas Gerais",
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)",
     ylim = c(9000,11150))
lines(ocup_mg, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Ocupação - Estrutural AR(1)","Ocupação: design-based","IC 95%: signal-based"), 
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")
