################################################################################
##       SCRIPT PARA DIAGNÓSTICO DOS MODELOS UNI E MULT - OCUPADOS            ##
################################################################################

## Para visualizar os gráficos em segunda tela (se necessário):

dev.new()
dev.new()

### 01 - BELO HORIZONTE ########################################################
rm(list = ls())
gc()

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/01_mod_bh.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### 02 - COLAR E ENTORNO METROPOLITANO DE BELO HORIZONTE #######################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/02_mod_ent.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### 03 - SUL DE MINAS ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/03_mod_sul.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### 04 - TRIÂNGULO MINEIRO #####################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/04_mod_trg.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### 05 - ZONA DA MATA ##########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/05_mod_mat.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### 06 - NORTE DE MINAS ########################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/06_mod_nrt.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### 07 - VALE DO RIO DOCE ######################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/07_mod_val.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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


### 08 - CENTRAL ###############################################################
rm(list = ls())

env1<-new.env()
env2<-new.env()

load("C:/FJP2425/Programacao/data/Rdatas/8_estruturalocup_8reg/08_mod_cen.Rdata", envir = env1)
load("C:/FJP2425/Programacao/data/Rdatas/14_multivariado_comcorrelacao - ocup_8reg/estimados/01_mod_comcorr.Rdata",envir = env2)

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