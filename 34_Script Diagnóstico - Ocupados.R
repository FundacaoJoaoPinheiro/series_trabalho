################################################################################
##       SCRIPT PARA DIAGNÓSTICO DOS MODELOS UNI E MULT - OCUPADOS            ##
################################################################################

## Para visualizar os gráficos em segunda tela (se necessário):

dev.new()
dev.new()

rm(list=ls())
gc()
options(scipen=999)

## Para visualizar os gráficos em segunda tela (se necessário):

dev.new()
dev.new()

## Uploads dos dados:

env1<-new.env()
env2<-new.env()
env3<-new.env()
env4<-new.env()
env5<-new.env()
env6<-new.env()
env7<-new.env()
env8<-new.env()
env9<-new.env()
env10<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/01_mod_bh.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/02_mod_ent.Rdata", envir = env2)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/03_mod_sul.Rdata", envir = env3)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/04_mod_trg.Rdata", envir = env4)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/05_mod_mat.Rdata", envir = env5)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/06_mod_nrt.Rdata", envir = env6)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/07_mod_val.Rdata", envir = env7)
load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/08_mod_cen.Rdata", envir = env8)
load("C:/FJP2425/Programacao/data/Rdatas/13_multivariado_semcorrelacao - ocup_8reg/estimados/01_mod_semcorr.Rdata", envir = env9)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/estimados/01_mod_comcorr.Rdata",envir = env10)

### Teste Razão Verossimilhança entre s/corr e c/corr ##########################

source("data/funcoes/30_teste_razvero.R")

mod_semcorr <- env9$modelo_mult_sem_corr
mod_comcorr <- env10$modelo_mult

teste_razvero(mod_semcorr,mod_comcorr)


### Matriz de correlação (todas as regiões) ####################################

params <- mod_comcorr[["fit"]][["par"]]
regioes <- c("BH","ENT","SUL","TRG","MAT","NRT","VAL","CEN")
n <- length(regioes)

cor_mat <- matrix(NA, n, n, dimnames = list(regioes, regioes))
diag(cor_mat) <- 1

# Os parametros de corr começam no 41 e vão até o 68
k <- 41
for(i in 1:(n-1)){
  for(j in (i+1):n){
    cor_ij <- tanh(params[k])
    cor_mat[j,i] <- cor_ij
    k <- k + 1
  }
}

print(round(cor_mat, 4))


### Diferença relativa média do erro padrão ##########################################

source("data/funcoes/31_calculo_rrse.R")

## BH

rrse_bh <- calcula_rrse(window(ts(mod_comcorr$se.original_1, start = 2012, frequency = 4),start=c(2013,3)),
                        window(ts(env1$ar1_bh$se.signal, start = 2012, frequency = 4),start=c(2013,3)),
                        window(ts(mod_comcorr$se.signal_1, start = 2012, frequency = 4),start=c(2013,3)))

## ENT

rrse_ent <- calcula_rrse(window(ts(mod_comcorr$se.original_2, start = 2012, frequency = 4),start=c(2013,3)),
                         window(ts(env2$ar1_ent$se.signal, start = 2012, frequency = 4),start=c(2013,3)),
                         window(ts(mod_comcorr$se.signal_2, start = 2012, frequency = 4),start=c(2013,3)))

## SUL

rrse_sul <- calcula_rrse(window(ts(mod_comcorr$se.original_3, start = 2012, frequency = 4),start=c(2013,3)),
                         window(ts(env3$ar1_sul$se.signal, start = 2012, frequency = 4),start=c(2013,3)),
                         window(ts(mod_comcorr$se.signal_3, start = 2012, frequency = 4),start=c(2013,3)))

## TRG

rrse_trg <- calcula_rrse(window(ts(mod_comcorr$se.original_4, start = 2012, frequency = 4),start=c(2013,3)),
                         window(ts(env4$ar1_trg$se.signal, start = 2012, frequency = 4),start=c(2013,3)),
                         window(ts(mod_comcorr$se.signal_4, start = 2012, frequency = 4),start=c(2013,3)))

## MAT

rrse_mat <- calcula_rrse(window(ts(mod_comcorr$se.original_5, start = 2012, frequency = 4),start=c(2013,3)),
                         window(ts(env5$ar1_mat$se.signal, start = 2012, frequency = 4),start=c(2013,3)),
                         window(ts(mod_comcorr$se.signal_5, start = 2012, frequency = 4),start=c(2013,3)))

## NRT

rrse_nrt <- calcula_rrse(window(ts(mod_comcorr$se.original_6, start = 2012, frequency = 4),start=c(2013,3)),
                         window(ts(env6$ar1_nrt$se.signal, start = 2012, frequency = 4),start=c(2013,3)),
                         window(ts(mod_comcorr$se.signal_6, start = 2012, frequency = 4),start=c(2013,3)))

## VAL

rrse_val <- calcula_rrse(window(ts(mod_comcorr$se.original_7, start = 2012, frequency = 4),start=c(2013,3)),
                         window(ts(env7$ar1_val$se.signal, start = 2012, frequency = 4),start=c(2013,3)),
                         window(ts(mod_comcorr$se.signal_7, start = 2012, frequency = 4),start=c(2013,3)))

## CEN

rrse_cen <- calcula_rrse(window(ts(mod_comcorr$se.original_8, start = 2012, frequency = 4),start=c(2013,3)),
                         window(ts(env8$ar1_cen$se.signal, start = 2012, frequency = 4),start=c(2013,3)),
                         window(ts(mod_comcorr$se.signal_8, start = 2012, frequency = 4),start=c(2013,3)))

## Resultados agrupados

lista_rrse <- list(BH  = rrse_bh,ENT = rrse_ent,SUL = rrse_sul,TRG = rrse_trg,MAT = rrse_mat,
                   NRT = rrse_nrt,VAL = rrse_val,CEN = rrse_cen)

df_rrse <- do.call(rbind, lista_rrse)

colnames(df_rrse) <- c("Univariado", "Multivariado")

df_rrse


### Vício relativo  ############################################################

source("data/funcoes/32_calculo_vicio.R")

## BH

vicio_bh <- calcula_vicio(window(ts(mod_comcorr$ts.original_1, start = 2012, frequency = 4),start=c(2013,3)),
                          window(ts(env1$ar1_bh$ts.signal, start = 2012, frequency = 4),start=c(2013,3)),
                          window(ts(mod_comcorr$ts.signal_1, start = 2012, frequency = 4),start=c(2013,3)))

## ENT

vicio_ent <- calcula_vicio(window(ts(mod_comcorr$ts.original_2, start = 2012, frequency = 4),start=c(2013,3)),
                           window(ts(env2$ar1_ent$ts.signal, start = 2012, frequency = 4),start=c(2013,3)),
                           window(ts(mod_comcorr$ts.signal_2, start = 2012, frequency = 4),start=c(2013,3)))

## SUL

vicio_sul <- calcula_vicio(window(ts(mod_comcorr$ts.original_3, start = 2012, frequency = 4),start=c(2013,3)),
                           window(ts(env3$ar1_sul$ts.signal, start = 2012, frequency = 4),start=c(2013,3)),
                           window(ts(mod_comcorr$ts.signal_3, start = 2012, frequency = 4),start=c(2013,3)))

## TRG

vicio_trg <- calcula_vicio(window(ts(mod_comcorr$ts.original_4, start = 2012, frequency = 4),start=c(2013,3)),
                           window(ts(env4$ar1_trg$ts.signal, start = 2012, frequency = 4),start=c(2013,3)),
                           window(ts(mod_comcorr$ts.signal_4, start = 2012, frequency = 4),start=c(2013,3)))

## MAT

vicio_mat <- calcula_vicio(window(ts(mod_comcorr$ts.original_5, start = 2012, frequency = 4),start=c(2013,3)),
                           window(ts(env5$ar1_mat$ts.signal, start = 2012, frequency = 4),start=c(2013,3)),
                           window(ts(mod_comcorr$ts.signal_5, start = 2012, frequency = 4),start=c(2013,3)))

## NRT

vicio_nrt <- calcula_vicio(window(ts(mod_comcorr$ts.original_6, start = 2012, frequency = 4),start=c(2013,3)),
                           window(ts(env6$ar1_nrt$ts.signal, start = 2012, frequency = 4),start=c(2013,3)),
                           window(ts(mod_comcorr$ts.signal_6, start = 2012, frequency = 4),start=c(2013,3)))

## VAL

vicio_val <- calcula_vicio(window(ts(mod_comcorr$ts.original_7, start = 2012, frequency = 4),start=c(2013,3)),
                           window(ts(env7$ar1_val$ts.signal, start = 2012, frequency = 4),start=c(2013,3)),
                           window(ts(mod_comcorr$ts.signal_7, start = 2012, frequency = 4),start=c(2013,3)))

## CEN

vicio_cen <- calcula_vicio(window(ts(mod_comcorr$ts.original_8, start = 2012, frequency = 4),start=c(2013,3)),
                           window(ts(env8$ar1_cen$ts.signal, start = 2012, frequency = 4),start=c(2013,3)),
                           window(ts(mod_comcorr$ts.signal_8, start = 2012, frequency = 4),start=c(2013,3)))

## Resultados agrupados

lista_vicio <- list(BH  = (vicio_bh)*100,ENT = (vicio_ent)*100,SUL = (vicio_sul)*100,TRG = (vicio_trg)*100,
                    MAT = (vicio_mat)*100, NRT = (vicio_nrt)*100,VAL = (vicio_val)*100,CEN = (vicio_cen)*100)

df_vicio <- do.call(rbind, lista_vicio)

colnames(df_vicio) <- c("Univariado", "Multivariado")

df_vicio


### Minas Gerais  ##############################################################

baseestr8reg <- readRDS("C:/FJP2425/Programacao/data/baseestr8reg.RDS")
ocup_mg <- baseestr8reg$`09 - Minas Gerais`$Total.de.ocupados
ocup_mg <- window(ts.union(ts(ocup_mg, start = 2012, frequency = 4)), start = c(2013,3))
ocup_mg <- ocup_mg/1000

## Sinal mg univariado

sinalmg_uni <- 
  env1$ar1_bh$ts.signal+
  env2$ar1_ent$ts.signal+
  env3$ar1_sul$ts.signal+
  env4$ar1_trg$ts.signal+
  env5$ar1_mat$ts.signal+
  env6$ar1_nrt$ts.signal+
  env7$ar1_val$ts.signal+
  env8$ar1_cen$ts.signal

sinalmg_uni <- window(ts(sinalmg_uni, start = 2012, frequency = 4),start=c(2013,3))

## Sinal mg multivariado com correlação

sinalmg_mult <- 
  mod_comcorr$ts.signal_1+
  mod_comcorr$ts.signal_2+
  mod_comcorr$ts.signal_3+
  mod_comcorr$ts.signal_4+
  mod_comcorr$ts.signal_5+
  mod_comcorr$ts.signal_6+
  mod_comcorr$ts.signal_7+
  mod_comcorr$ts.signal_8

sinalmg_mult <- window(ts(sinalmg_mult, start = 2012, frequency = 4),start=c(2013,3))

## Gráfico

plot(ocup_mg, type = "l", col = "black", lwd = 2,
     xlab = "Ano", ylab = "Total de ocupados (milhares de pessoas)", ylim=c(8500,12000))
lines(sinalmg_uni, col = "blue", lty = 1, lwd = 2)
lines(sinalmg_mult, col= "red", lty = 1, lwd = 2)
legend("topleft", legend = c("Estimativa direta", "Sinal - Mod. Univariado","Sinal - Mod. Multivariado"),
       col = c("black","blue","green"),lty = c(1,1,1),lwd = c(2,2,2),bty = "n", cex=0.8)

