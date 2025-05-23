################################################################################
##                COMPARATIVO MODELOS SMOOTH E ESTRUTURAL                     ##
################################################################################

## Para visualizar os gráficos em segunda tela:

dev.new()
dev.new()


### 01 - BELO HORIZONTE ########################################################
rm(list = ls())

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/01_mod_bh.Rdata")

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

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")

sm_ma1_bh<-mods$`01-Belo Horizonte`$sinal_smooth_ma1bh
cv_sm_ma1_bh<-mods$`01-Belo Horizonte`$cv_sinal_smooth_ma1bh
est_ma1_bh<-mods$`01-Belo Horizonte`$sinal_estrutural_ma1bh
cv_est_ma1_bh<-mods$`01-Belo Horizonte`$cv_sinal_estrutural_ma1bh

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_bh <- window(ts.union(
  ts(desoc_bh, start = 2012, frequency = 4),
  ts(sm_ma1_bh, start = 2012, frequency = 4),
  ts(est_ma1_bh, start = 2012, frequency = 4),
  ts(ICinf_bh,start = 2012, frequency = 4),
  ts(ICsup_bh,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_bh, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("bottom", legend = c("Desocupação: design-based",
                  "Sinal da Desocupação - Smooth MA(1)",
                  "Sinal da Desocupação - Estrutural MA(1)"),lty = c(1, 1, 1),col = c(1, 4, 2),bty = 'n',lwd = c(2, 2, 2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_bh.cv <- window(ts.union(
  ts((cv_bh*100), start = 2012, frequency = 4),
  ts(cv_sm_ma1_bh, start = 2012, frequency = 4),
  ts(cv_est_ma1_bh, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_bh.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados - Smooth MA(1)",
                             "Sinal CV desocupados - Estrutural MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("01 - Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

se_sinal_bh<-ma1_bh$se.signal

ICinf_sinal <- est_ma1_bh - 1.96 * se_sinal_bh
ICsup_sinal <- est_ma1_bh + 1.96 * se_sinal_bh

ICinf_sinal <- window(ts(ICinf_sinal,start = 2012, frequency = 4), start = c(2013,3))
ICsup_sinal <- window(ts(ICsup_sinal,start = 2012, frequency = 4), start = c(2013,3))

fig_sinal<-window(ts.union(ts(est_ma1_bh, start = 2012, frequency = 4)), start = c(2013, 3))
fig_estimativa<-window(ts.union(ts(desoc_bh, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(fig_sinal, type = "l", col = "red", lwd = 2,
     main = "01-Belo Horizonte",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(45,270))
lines(fig_estimativa, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: signal-based"), 
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")


### 02 - COLAR E ENTORNO METROPOLITANO DE BELO HORIZONTE #######################
rm(list = ls())

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/02_mod_ent.Rdata")

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

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")

sm_ma1_ent<-mods$`02-Colar e Entorno Metropolitano de BH`$sinal_smooth_ma1ent
cv_sm_ma1_ent<-mods$`02-Colar e Entorno Metropolitano de BH`$cv_sinal_smooth_ma1ent
est_ma1_ent<-mods$`02-Colar e Entorno Metropolitano de BH`$sinal_estrutural_ma1ent
cv_est_ma1_ent<-mods$`02-Colar e Entorno Metropolitano de BH`$cv_sinal_estrutural_ma1ent

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_ent <- window(ts.union(
  ts(desoc_ent, start = 2012, frequency = 4),
  ts(sm_ma1_ent, start = 2012, frequency = 4),
  ts(est_ma1_ent, start = 2012, frequency = 4),
  ts(ICinf_ent,start = 2012, frequency = 4),
  ts(ICsup_ent,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_ent, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da Desocupação - Smooth MA(1)",
                            "Sinal da Desocupação - Estrutural MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ent.cv <- window(ts.union(
  ts((cv_ent) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ma1_ent, start = 2012, frequency = 4),
  ts(cv_est_ma1_ent, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_ent.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados - Smooth MA(1)",
                             "Sinal CV desocupados - Estrutural MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("02 - Colar e Entorno Metropolitano de Belo Horizonte", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

se_sinal_ent<-ma1_ent$se.signal

ICinf_sinal <- est_ma1_ent - 1.96 * se_sinal_ent
ICsup_sinal <- est_ma1_ent + 1.96 * se_sinal_ent

ICinf_sinal <- window(ts(ICinf_sinal,start = 2012, frequency = 4), start = c(2013,3))
ICsup_sinal <- window(ts(ICsup_sinal,start = 2012, frequency = 4), start = c(2013,3))

fig_sinal<-window(ts.union(ts(est_ma1_ent, start = 2012, frequency = 4)), start = c(2013, 3))
fig_estimativa<-window(ts.union(ts(desoc_ent, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(fig_sinal, type = "l", col = "red", lwd = 2,
     main = "02-Colar e Entorno metropolitano de Belo Horizonte",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(80,400))
lines(fig_estimativa, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: signal-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")


### 03 - SUL DE MINAS ##########################################################

rm(list = ls())

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/03_mod_sul.Rdata")

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")
  
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

sm_arma11_sul<-mods$`03-Sul de Minas`$sinal_smooth_arma11sul
cv_sm_arma11_sul<-mods$`03-Sul de Minas`$cv_sinal_smooth_arma11sul
est_arma11_sul<-mods$`03-Sul de Minas`$sinal_estrutural_arma11sul
cv_est_arma11_sul<-mods$`03-Sul de Minas`$cv_sinal_estrutural_arma11sul

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_sul <- window(ts.union(
  ts(desoc_sul, start = 2012, frequency = 4),
  ts(sm_arma11_sul, start = 2012, frequency = 4),
  ts(est_arma11_sul, start = 2012, frequency = 4),
  ts(ICinf_sul,start = 2012, frequency = 4),
  ts(ICsup_sul,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_sul, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("topleft", legend = c("Desocupação: design-based",
                             "Sinal desocupados - Smooth ARMA(1,1)",
                             "Sinal desocupados - Estrutural ARMA(1,1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_sul.cv <- window(ts.union(
  ts((cv_sul) * 100, start = 2012, frequency = 4),
  ts(cv_sm_arma11_sul, start = 2012, frequency = 4),
  ts(cv_est_arma11_sul, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_sul.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados - Smooth ARMA(1,1)",
                             "Sinal CV desocupados - Estrutural ARMA(1,1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("03 - Sul de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

se_sinal_sul<-arma11_sul$se.signal

ICinf_sinal <- est_arma11_sul - 1.96 * se_sinal_sul
ICsup_sinal <- est_arma11_sul + 1.96 * se_sinal_sul

ICinf_sinal <- window(ts(ICinf_sinal,start = 2012, frequency = 4), start = c(2013,3))
ICsup_sinal <- window(ts(ICsup_sinal,start = 2012, frequency = 4), start = c(2013,3))

fig_sinal<-window(ts.union(ts(est_arma11_sul, start = 2012, frequency = 4)), start = c(2013, 3))
fig_estimativa<-window(ts.union(ts(desoc_sul, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(fig_sinal, type = "l", col = "red", lwd = 2,
     main = "03-Sul de Minas",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(20,200))
lines(fig_estimativa, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: model-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")


### 04 - TRIÂNGULO MINEIRO #####################################################
rm(list = ls())

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/04_mod_trg.Rdata")

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")

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

sm_ma1_trg<-mods$`04-Triângulo Mineiro`$sinal_smooth_ma1trg
cv_sm_ma1_trg<-mods$`04-Triângulo Mineiro`$cv_sinal_smooth_ma1trg
est_ma1_trg<-mods$`04-Triângulo Mineiro`$sinal_estrutural_ma1trg
cv_est_ma1_trg<-mods$`04-Triângulo Mineiro`$cv_sinal_estrutural_ma1trg

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_trg <- window(ts.union(
  ts(desoc_trg, start = 2012, frequency = 4),
  ts(sm_ma1_trg, start = 2012, frequency = 4),
  ts(est_ma1_trg, start = 2012, frequency = 4),
  ts(ICinf_trg,start = 2012, frequency = 4),
  ts(ICsup_trg,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_trg, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da Desocupação - Smooth MA(1)",
                            "Sinal da Desocupação - Estrutural MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_trg.cv <- window(ts.union(
  ts((cv_trg) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ma1_trg, start = 2012, frequency = 4),
  ts(cv_est_ma1_trg, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_trg.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados - Smooth MA(1)",
                             "Sinal CV desocupados - Estrutural MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("04 - Triângulo Mineiro", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

se_sinal_trg<-ma1_trg$se.signal

ICinf_sinal <- est_ma1_trg - 1.96 * se_sinal_trg
ICsup_sinal <- est_ma1_trg + 1.96 * se_sinal_trg

ICinf_sinal <- window(ts(ICinf_sinal,start = 2012, frequency = 4), start = c(2013,3))
ICsup_sinal <- window(ts(ICsup_sinal,start = 2012, frequency = 4), start = c(2013,3))

fig_sinal<-window(ts.union(ts(est_ma1_trg, start = 2012, frequency = 4)), start = c(2013, 3))
fig_estimativa<-window(ts.union(ts(desoc_trg, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(fig_sinal, type = "l", col = "red", lwd = 2,
     main = "04-Triângulo Mineiro",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(30,190))
lines(fig_estimativa, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: model-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")


### 05 - ZONA DA MATA ##########################################################
rm(list = ls())

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/05_mod_mat.Rdata")

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")

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

sm_ma1_mat<-mods$`05-Zona da Mata`$sinal_smooth_ma1mat
cv_sm_ma1_mat<-mods$`05-Zona da Mata`$cv_sinal_smooth_ma1mat
est_ma1_mat<-mods$`05-Zona da Mata`$sinal_estrutural_ma1mat
cv_est_ma1_mat<-mods$`05-Zona da Mata`$cv_sinal_estrutural_ma1mat

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_mat <- window(ts.union(
  ts(desoc_mat, start = 2012, frequency = 4),
  ts(sm_ma1_mat, start = 2012, frequency = 4),
  ts(est_ma1_mat, start = 2012, frequency = 4),
  ts(ICinf_mat,start = 2012, frequency = 4),
  ts(ICsup_mat,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_mat, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da Desocupação - Smooth MA(1)",
                            "Sinal da Desocupação - Estrutural MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_mat.cv <- window(ts.union(
  ts((cv_mat) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ma1_mat, start = 2012, frequency = 4),
  ts(cv_est_ma1_mat, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_mat.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados - Smooth MA(1)",
                             "Sinal CV desocupados - Estrutural MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("05 - Zona da Mata", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

se_sinal_mat<-ma1_mat$se.signal

ICinf_sinal <- est_ma1_mat - 1.96 * se_sinal_mat
ICsup_sinal <- est_ma1_mat + 1.96 * se_sinal_mat

ICinf_sinal <- window(ts(ICinf_sinal,start = 2012, frequency = 4), start = c(2013,3))
ICsup_sinal <- window(ts(ICsup_sinal,start = 2012, frequency = 4), start = c(2013,3))

fig_sinal<-window(ts.union(ts(est_ma1_mat, start = 2012, frequency = 4)), start = c(2013, 3))
fig_estimativa<-window(ts.union(ts(desoc_mat, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(fig_sinal, type = "l", col = "red", lwd = 2,
     main = "05-Zona da Mata",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(30,180))
lines(fig_estimativa, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: model-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")

### 06 - NORTE DE MINAS ########################################################
rm(list = ls())

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/06_mod_nrt.Rdata")

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")

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

sm_ma1_nrt<-mods$`06-Norte de Minas`$sinal_smooth_ma1nrt
cv_sm_ma1_nrt<-mods$`06-Norte de Minas`$cv_sinal_smooth_ma1nrt
est_ma1_nrt<-mods$`06-Norte de Minas`$sinal_estrutural_ma1nrt
cv_est_ma1_nrt<-mods$`06-Norte de Minas`$cv_sinal_estrutural_ma1nrt

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_nrt <- window(ts.union(
  ts(desoc_nrt, start = 2012, frequency = 4),
  ts(sm_ma1_nrt, start = 2012, frequency = 4),
  ts(est_ma1_nrt, start = 2012, frequency = 4),
  ts(ICinf_nrt,start = 2012, frequency = 4),
  ts(ICsup_nrt,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_nrt, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da Desocupação - Smooth MA(1)",
                            "Sinal da Desocupação - Estrutural MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_nrt.cv <- window(ts.union(
  ts((cv_nrt) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ma1_nrt, start = 2012, frequency = 4),
  ts(cv_est_ma1_nrt, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_nrt.cv, plot.type = "single", col = c(1, 4, 2),lty = c(1, 1, 1),lwd = c(2, 2, 2),ylab = "", xlab = "")
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados - Smooth MA(1)",
                             "Sinal CV desocupados - Estrutural MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("06 - Norte de Minas", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

se_sinal_nrt<-ma1_nrt$se.signal

ICinf_sinal <- est_ma1_nrt - 1.96 * se_sinal_nrt
ICsup_sinal <- est_ma1_nrt + 1.96 * se_sinal_nrt

ICinf_sinal <- window(ts(ICinf_sinal,start = 2012, frequency = 4), start = c(2013,3))
ICsup_sinal <- window(ts(ICsup_sinal,start = 2012, frequency = 4), start = c(2013,3))

fig_sinal<-window(ts.union(ts(est_ma1_nrt, start = 2012, frequency = 4)), start = c(2013, 3))
fig_estimativa<-window(ts.union(ts(desoc_nrt, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(fig_sinal, type = "l", col = "red", lwd = 2,
     main = "06-Norte de Minas",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(50,250))
lines(fig_estimativa, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: model-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")

### 07 - VALE DO RIO DOCE ######################################################
rm(list = ls())

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/07_mod_val.Rdata")

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")

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

sm_ar1_val<-mods$`07-Vale do Rio Doce`$sinal_smooth_ar1vl
cv_sm_ar1_val<-mods$`07-Vale do Rio Doce`$cv_sinal_smooth_ar1vl
est_ar1_val<-mods$`07-Vale do Rio Doce`$sinal_estrutural_ar1vl
cv_est_ar1_val<-mods$`07-Vale do Rio Doce`$cv_sinal_estrutural_ar1vl

par(mfrow = c(1, 2), mar = c(5, 5, 1, 1), oma = c(0, 0, 2, 0), cex = 0.8)
fig_ar1 <- window(ts.union(
  ts(desoc_val, start = 2012, frequency = 4),
  ts(sm_ar1_val, start = 2012, frequency = 4),
  ts(est_ar1_val, start = 2012, frequency = 4),
  ts(ICinf_val,start = 2012, frequency = 4),
  ts(ICsup_val,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_ar1, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da Desocupação - Smooth AR(1)",
                            "Sinal da Desocupação - Estrutural AR(1)"),
       lty = c(1,1,1), col = c(1, 4,2), bty = 'n', lwd = c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_ar1.cv <- window(ts.union(
  ts((cv_val) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ar1_val, start = 2012, frequency = 4),
  ts(cv_est_ar1_val, start = 2012, frequency = 4)), start = c(2013, 3))
plot(fig_ar1.cv, plot.type = "single", col = c(1, 4,2), ylab = "", xlab = "", lty = c(1, 1,1), lwd = c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados - Smooth AR(1)",
                             "Sinal CV desocupados - Estrutural AR(1)"),
       lty = c(1, 1,1), col = c(1, 4,2), bty = 'n', lwd = c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("07 - Vale do Rio Doce", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

se_sinal_val<-ar1_val$se.signal

ICinf_sinal <- est_ar1_val - 1.96 * se_sinal_val
ICsup_sinal <- est_ar1_val + 1.96 * se_sinal_val

ICinf_sinal <- window(ts(ICinf_sinal,start = 2012, frequency = 4), start = c(2013,3))
ICsup_sinal <- window(ts(ICsup_sinal,start = 2012, frequency = 4), start = c(2013,3))

fig_sinal<-window(ts.union(ts(est_ar1_val, start = 2012, frequency = 4)), start = c(2013, 3))
fig_estimativa<-window(ts.union(ts(desoc_val, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(fig_sinal, type = "l", col = "red", lwd = 2,
     main = "07-Vale do Rio Doce",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(30,210))
lines(fig_estimativa, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural AR(1)","Desocupação: design-based","IC 95%: model-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")


### 08 - CENTRAL ###############################################################
rm(list = ls())

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/08_mod_cen.Rdata")

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")

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

sm_ma1_cen<-mods$`08-Central`$sinal_smooth_ma1cen
cv_sm_ma1_cen<-mods$`08-Central`$cv_sinal_smooth_ma1cen
est_ma1_cen<-mods$`08-Central`$sinal_estrutural_ma1cen
cv_est_ma1_cen<-mods$`08-Central`$cv_sinal_estrutural_ma1cen

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_cen <- window(ts.union(
  ts(desoc_cen, start = 2012, frequency = 4),
  ts(sm_ma1_cen, start = 2012, frequency = 4),
  ts(est_ma1_cen, start = 2012, frequency = 4),
  ts(ICinf_cen,start = 2012, frequency = 4),
  ts(ICsup_cen,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_cen, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da Desocupação - Smooth MA(1)",
                            "Sinal da Desocupação - Estrutural MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_cen.cv <- window(ts.union(
  ts((cv_cen) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ma1_cen, start = 2012, frequency = 4),
  ts(cv_est_ma1_cen, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_cen.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados - Smooth MA(1)",
                             "Sinal CV desocupados - Estrutural MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("08 - Central", side = 3, outer = TRUE, line = 0.5, font = 2, cex = 1.2)


### GRÁFICO COM IC DO SINAL

se_sinal_cen<-ma1_cen$se.signal

ICinf_sinal <- est_ma1_cen - 1.96 * se_sinal_cen
ICsup_sinal <- est_ma1_cen + 1.96 * se_sinal_cen

ICinf_sinal <- window(ts(ICinf_sinal,start = 2012, frequency = 4), start = c(2013,3))
ICsup_sinal <- window(ts(ICsup_sinal,start = 2012, frequency = 4), start = c(2013,3))

fig_sinal<-window(ts.union(ts(est_ma1_cen, start = 2012, frequency = 4)), start = c(2013, 3))
fig_estimativa<-window(ts.union(ts(desoc_cen, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(fig_sinal, type = "l", col = "red", lwd = 2,
     main = "08-Central",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(45,270))
lines(fig_estimativa, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: model-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")

### 09 - MINAS GERAIS ##########################################################
rm(list = ls())

load("D:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/09_mod_mg.Rdata")

mods<-readRDS("D:/FJP2425/Programacao/data/RDS de modelos/result_mods_deso.rds")

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

sm_ma1_mg<-mods$`09 - Minas Gerais`$sinal_smooth_ma1mg
cv_sm_ma1_mg<-mods$`09 - Minas Gerais`$cv_sinal_smooth_ma1mg
est_ma1_mg<-mods$`09 - Minas Gerais`$sinal_estrutural_ma1mg
cv_est_ma1_mg<-mods$`09 - Minas Gerais`$cv_sinal_estrutural_ma1mg

par(mfrow=c(1,2), mar=c(5,5,1,1), oma=c(0,0,2,0), cex=0.8)
fig_mg <- window(ts.union(
  ts(desoc_mg, start = 2012, frequency = 4),
  ts(sm_ma1_mg, start = 2012, frequency = 4),
  ts(est_ma1_mg, start = 2012, frequency = 4),
  ts(ICinf_mg,start = 2012, frequency = 4),
  ts(ICsup_mg,start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_mg, plot.type = "single", col = c(1, 4, 2, 1, 1),lty = c(1, 1, 1, 2, 2),lwd = c(2, 2, 2, 1, 1),ylab = "", xlab = "")
legend("bottom", legend = c("Desocupação: design-based",
                            "Sinal da Desocupação - Smooth MA(1)",
                            "Sinal da Desocupação - Estrutural MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("Total de desocupados (milhares de pessoas)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)

fig_mg.cv <- window(ts.union(
  ts((cv_mg) * 100, start = 2012, frequency = 4),
  ts(cv_sm_ma1_mg, start = 2012, frequency = 4),
  ts(cv_est_ma1_mg, start = 2012, frequency = 4)
), start=c(2013,3))
plot(fig_mg.cv, plot.type = "single", col = c(1,4,2), ylab="", xlab="", lty = c(1,1,1), lwd=c(2))
legend("topleft", legend = c("CV desocupados: design-based",
                             "Sinal CV desocupados - Smooth MA(1)",
                             "Sinal CV desocupados - Estrutural MA(1)"),
       lty = c(1,1,1), col = c(1,4,2), bty = 'n', lwd=c(2))
mtext("CV (%)", side = 2, line = 3)
mtext("Ano", side = 1, line = 3)
mtext("09 - Minas Gerais", side = 3, outer = TRUE, line = 0.5)


### GRÁFICO COM IC DO SINAL

se_sinal_mg<-ma1_mg$se.signal

ICinf_sinal <- est_ma1_mg - 1.96 * se_sinal_mg
ICsup_sinal <- est_ma1_mg + 1.96 * se_sinal_mg

ICinf_sinal <- window(ts(ICinf_sinal,start = 2012, frequency = 4), start = c(2013,3))
ICsup_sinal <- window(ts(ICsup_sinal,start = 2012, frequency = 4), start = c(2013,3))

fig_sinal<-window(ts.union(ts(est_ma1_mg, start = 2012, frequency = 4)), start = c(2013, 3))
fig_estimativa<-window(ts.union(ts(desoc_mg, start = 2012, frequency = 4)), start = c(2013, 3))

par(mfrow=c(1,1), mar=c(4,4,2,1), oma=c(0,0,2,0), cex=1)
plot(fig_sinal, type = "l", col = "red", lwd = 2,
     main = "09 - Minas Gerais",
     xlab = "Ano", ylab = "Total de desocupados (milhares de pessoas)",
     ylim = c(450,1700))
lines(fig_estimativa, col = "black", lty = 1, lwd = 2)
lines(ICinf_sinal, col = "red", lty = 2)
lines(ICsup_sinal, col = "red", lty = 2)
legend("topleft", legend = c("Sinal da Desocupação - Estrutural MA(1)","Desocupação: design-based","IC 95%: model-based"),
       col = c("red","black", "red"),lty = c(1,1, 2),lwd = c(2,2,1),bty = "n")

