################################################################################
##      SCRIPT PARA DIAGNÓSTICO DOS MODELOS UNI E MULT - DESOCUPADOS          ##
################################################################################

## Para visualizar os gráficos em segunda tela (se necessário):

dev.new()
dev.new()

### Teste Razão Verossimilhança entre s/corr e c/corr ##########################



### Matriz de correlação (todas as regiões) ####################################

load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/estimados/01_mod_comcorr.Rdata")




### 01 - BELO HORIZONTE ########################################################
rm(list = ls())
gc()

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/01_mod_bh.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### 02 - COLAR E ENTORNO METROPOLITANO DE BELO HORIZONTE #######################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/02_mod_ent.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### 03 - SUL DE MINAS ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/03_mod_sul.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### 04 - TRIÂNGULO MINEIRO #####################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/04_mod_trg.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### 05 - ZONA DA MATA ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/05_mod_mat.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### 06 - NORTE DE MINAS ########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/06_mod_nrt.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### 07 - VALE DO RIO DOCE ######################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/07_mod_val.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### 08 - CENTRAL ###############################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/6_estruturaldesocup_8reg/08_mod_cen.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/12_multivariado_comcorr - desoc_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### Comparativo com o total MG #################################################



