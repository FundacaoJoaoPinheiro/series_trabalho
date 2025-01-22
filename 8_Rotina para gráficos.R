################################################################################
##                          SCRIPT PARA GRÁFICOS                              ##
################################################################################

## O objetivo desta rotina é servir de base para elaborar gráficos de diversas variáveis
#   e servir de consulta para trabalhos posteriores;

## Para visualizar os gráficos em segunda tela:

dev.new()
dev.new()

## Gráficos para os coeficientes de variação da base por estratos:

dados<-readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")

### DADOS TRIMESTRAIS ##########################################################

## Transformando os dados em série temporal:

# BH:

{cvbh_o<-ts(dados$`01-Belo Horizonte`$CV.ocupados, start=c(2012,1), frequency = 4)
cvbh_d<-ts(dados$`01-Belo Horizonte`$CV.desocupados, start=c(2012,1), frequency = 4)
cvbh_tx<-ts(dados$`01-Belo Horizonte`$CV.taxa, start=c(2012,1), frequency = 4)

# Entorno

cvent_o<-ts(dados$`02-Entorno metropolitano de BH`$CV.ocupados, start=c(2012,1), frequency = 4)
cvent_d<-ts(dados$`02-Entorno metropolitano de BH`$CV.desocupados, start=c(2012,1), frequency = 4)
cvent_tx<-ts(dados$`02-Entorno metropolitano de BH`$CV.taxa, start=c(2012,1), frequency = 4)

# Colar:

cvcol_o<-ts(dados$`03-Colar metropolitano de BH`$CV.ocupados, start=c(2012,1), frequency = 4)
cvcol_d<-ts(dados$`03-Colar metropolitano de BH`$CV.desocupados, start=c(2012,1), frequency = 4)
cvcol_tx<-ts(dados$`03-Colar metropolitano de BH`$CV.taxa, start=c(2012,1), frequency = 4)

# RIDE:

cvrid_o<-ts(dados$`04-RIDE de Brasília em Minas`$CV.ocupados, start=c(2012,1), frequency = 4)
cvrid_d<-ts(dados$`04-RIDE de Brasília em Minas`$CV.desocupados, start=c(2012,1), frequency = 4)
cvrid_tx<-ts(dados$`04-RIDE de Brasília em Minas`$CV.taxa, start=c(2012,1), frequency = 4)

# Sul:

cvsul_o<-ts(dados$`05-Sul de Minas`$CV.ocupados, start=c(2012,1), frequency = 4)
cvsul_d<-ts(dados$`05-Sul de Minas`$CV.desocupados, start=c(2012,1), frequency = 4)
cvsul_tx<-ts(dados$`05-Sul de Minas`$CV.taxa, start=c(2012,1), frequency = 4)

# Triângulo:

cvtrg_o<-ts(dados$`06-Triângulo Mineiro`$CV.ocupados, start=c(2012,1), frequency = 4)
cvtrg_d<-ts(dados$`06-Triângulo Mineiro`$CV.desocupados, start=c(2012,1), frequency = 4)
cvtrg_tx<-ts(dados$`06-Triângulo Mineiro`$CV.taxa, start=c(2012,1), frequency = 4)

# Mata:

cvmat_o<-ts(dados$`07-Mata de Minas Gerais`$CV.ocupados, start=c(2012,1), frequency = 4)
cvmat_d<-ts(dados$`07-Mata de Minas Gerais`$CV.desocupados, start=c(2012,1), frequency = 4)
cvmat_tx<-ts(dados$`07-Mata de Minas Gerais`$CV.taxa, start=c(2012,1), frequency = 4)

# Norte:

cvnrt_o<-ts(dados$`08-Norte de Minas`$CV.ocupados, start=c(2012,1), frequency = 4)
cvnrt_d<-ts(dados$`08-Norte de Minas`$CV.desocupados, start=c(2012,1), frequency = 4)
cvnrt_tx<-ts(dados$`08-Norte de Minas`$CV.taxa, start=c(2012,1), frequency = 4)

# Vale:

cvvl_o<-ts(dados$`09-Vale do Rio Doce`$CV.ocupados, start=c(2012,1), frequency = 4)
cvvl_d<-ts(dados$`09-Vale do Rio Doce`$CV.desocupados, start=c(2012,1), frequency = 4)
cvvl_tx<-ts(dados$`09-Vale do Rio Doce`$CV.taxa, start=c(2012,1), frequency = 4)

# Central:

cvcen_o<-ts(dados$`10-Central`$CV.ocupados, start=c(2012,1), frequency = 4)
cvcen_d<-ts(dados$`10-Central`$CV.desocupados, start=c(2012,1), frequency = 4)
cvcen_tx<-ts(dados$`10-Central`$CV.taxa, start=c(2012,1), frequency = 4)

# MG:

cvmg_o<-ts(dados$`11 - Minas Gerais`$CV.ocupados, start=c(2012,1), frequency = 4)
cvmg_d<-ts(dados$`11 - Minas Gerais`$CV.desocupados, start=c(2012,1), frequency = 4)
cvmg_tx<-ts(dados$`11 - Minas Gerais`$CV.taxa, start=c(2012,1), frequency = 4)}


### GRÁFICOS ###################################################################

# Individuais:

par(mfrow=c(1,1))

plot.ts(cvbh_o,col = "black", xlab="Ano", ylab="CV (%)",main="01 - Belo Horizonte",lwd=2)

# Duplas:

# Ocupada

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((cvbh_o), col = "black", xlab="Ano", ylab="Ocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvbh_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("1 - Belo Horizonte", outer=TRUE, line =-2,cex=1.5,font=2)}

# Desocupada:


# Taxa de desemprego:












