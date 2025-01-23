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

anual<-readRDS("D:/FJP2425/Programacao/data/baseestanual.RDS")

### DADOS TRIMESTRAIS ##########################################################

## Transformando os dados em série temporal:

# BH:

{t_cvbh_o<-ts(dados$`01-Belo Horizonte`$CV.ocupados, start=c(2012,1), frequency = 4)
t_cvbh_d<-ts(dados$`01-Belo Horizonte`$CV.desocupados, start=c(2012,1), frequency = 4)
t_cvbh_tx<-ts(dados$`01-Belo Horizonte`$CV.taxa, start=c(2012,1), frequency = 4)

# Entorno

t_cvent_o<-ts(dados$`02-Entorno metropolitano de BH`$CV.ocupados, start=c(2012,1), frequency = 4)
t_cvent_d<-ts(dados$`02-Entorno metropolitano de BH`$CV.desocupados, start=c(2012,1), frequency = 4)
t_cvent_tx<-ts(dados$`02-Entorno metropolitano de BH`$CV.taxa, start=c(2012,1), frequency = 4)

# Colar:

t_cvcol_o<-ts(dados$`03-Colar metropolitano de BH`$CV.ocupados, start=c(2012,1), frequency = 4)
t_cvcol_d<-ts(dados$`03-Colar metropolitano de BH`$CV.desocupados, start=c(2012,1), frequency = 4)
t_cvcol_tx<-ts(dados$`03-Colar metropolitano de BH`$CV.taxa, start=c(2012,1), frequency = 4)

# RIDE:

t_cvrid_o<-ts(dados$`04-RIDE de Brasília em Minas`$CV.ocupados, start=c(2012,1), frequency = 4)
t_cvrid_d<-ts(dados$`04-RIDE de Brasília em Minas`$CV.desocupados, start=c(2012,1), frequency = 4)
t_cvrid_tx<-ts(dados$`04-RIDE de Brasília em Minas`$CV.taxa, start=c(2012,1), frequency = 4)

# Sul:

t_cvsul_o<-ts(dados$`05-Sul de Minas`$CV.ocupados, start=c(2012,1), frequency = 4)
t_cvsul_d<-ts(dados$`05-Sul de Minas`$CV.desocupados, start=c(2012,1), frequency = 4)
t_cvsul_tx<-ts(dados$`05-Sul de Minas`$CV.taxa, start=c(2012,1), frequency = 4)

# Triângulo:

t_cvtrg_o<-ts(dados$`06-Triângulo Mineiro`$CV.ocupados, start=c(2012,1), frequency = 4)
t_cvtrg_d<-ts(dados$`06-Triângulo Mineiro`$CV.desocupados, start=c(2012,1), frequency = 4)
t_cvtrg_tx<-ts(dados$`06-Triângulo Mineiro`$CV.taxa, start=c(2012,1), frequency = 4)

# Mata:

t_cvmat_o<-ts(dados$`07-Mata de Minas Gerais`$CV.ocupados, start=c(2012,1), frequency = 4)
t_cvmat_d<-ts(dados$`07-Mata de Minas Gerais`$CV.desocupados, start=c(2012,1), frequency = 4)
t_cvmat_tx<-ts(dados$`07-Mata de Minas Gerais`$CV.taxa, start=c(2012,1), frequency = 4)

# Norte:

t_cvnrt_o<-ts(dados$`08-Norte de Minas`$CV.ocupados, start=c(2012,1), frequency = 4)
t_cvnrt_d<-ts(dados$`08-Norte de Minas`$CV.desocupados, start=c(2012,1), frequency = 4)
t_cvnrt_tx<-ts(dados$`08-Norte de Minas`$CV.taxa, start=c(2012,1), frequency = 4)

# Vale:

t_cvvl_o<-ts(dados$`09-Vale do Rio Doce`$CV.ocupados, start=c(2012,1), frequency = 4)
t_cvvl_d<-ts(dados$`09-Vale do Rio Doce`$CV.desocupados, start=c(2012,1), frequency = 4)
t_cvvl_tx<-ts(dados$`09-Vale do Rio Doce`$CV.taxa, start=c(2012,1), frequency = 4)

# Central:

t_cvcen_o<-ts(dados$`10-Central`$CV.ocupados, start=c(2012,1), frequency = 4)
t_cvcen_d<-ts(dados$`10-Central`$CV.desocupados, start=c(2012,1), frequency = 4)
t_cvcen_tx<-ts(dados$`10-Central`$CV.taxa, start=c(2012,1), frequency = 4)

# MG:

t_cvmg_o<-ts(dados$`11 - Minas Gerais`$CV.ocupados, start=c(2012,1), frequency = 4)
t_cvmg_d<-ts(dados$`11 - Minas Gerais`$CV.desocupados, start=c(2012,1), frequency = 4)
t_cvmg_tx<-ts(dados$`11 - Minas Gerais`$CV.taxa, start=c(2012,1), frequency = 4)}


### DADOS ANUAIS ###############################################################

# BH:

{a_cvbh_o<-ts(anual$`01-Belo Horizonte`$cv, start=c(2012), frequency = 1)
a_cvbh_d<-ts(anual$`01-Belo Horizonte`$cv.1, start=c(2012), frequency = 1)
a_cvbh_tx<-ts(anual$`01-Belo Horizonte`$cv.6, start=c(2012), frequency = 1)

# Entorno

a_cvent_o<-ts(anual$`02-Entorno metropolitano de BH`$cv, start=c(2012), frequency = 1)
a_cvent_d<-ts(anual$`02-Entorno metropolitano de BH`$cv.1, start=c(2012), frequency = 1)
a_cvent_tx<-ts(anual$`02-Entorno metropolitano de BH`$cv.6, start=c(2012), frequency = 1)

# Colar:

a_cvcol_o<-ts(anual$`03-Colar metropolitano de BH`$cv, start=c(2012), frequency = 1)
a_cvcol_d<-ts(anual$`03-Colar metropolitano de BH`$cv.1, start=c(2012), frequency = 1)
a_cvcol_tx<-ts(anual$`03-Colar metropolitano de BH`$cv.6, start=c(2012), frequency = 1)

# RIDE:

a_cvrid_o<-ts(anual$`04-RIDE de Brasília em Minas`$cv, start=c(2012), frequency = 1)
a_cvrid_d<-ts(anual$`04-RIDE de Brasília em Minas`$cv.1, start=c(2012), frequency = 1)
a_cvrid_tx<-ts(anual$`04-RIDE de Brasília em Minas`$cv.6, start=c(2012), frequency = 1)

# Sul:

a_cvsul_o<-ts(anual$`05-Sul de Minas`$cv, start=c(2012), frequency = 1)
a_cvsul_d<-ts(anual$`05-Sul de Minas`$cv.1, start=c(2012), frequency = 1)
a_cvsul_tx<-ts(anual$`05-Sul de Minas`$cv.6, start=c(2012), frequency = 1)

# Triângulo:

a_cvtrg_o<-ts(anual$`06-Triângulo Mineiro`$cv, start=c(2012), frequency = 1)
a_cvtrg_d<-ts(anual$`06-Triângulo Mineiro`$cv.1, start=c(2012), frequency = 1)
a_cvtrg_tx<-ts(anual$`06-Triângulo Mineiro`$cv.6, start=c(2012), frequency = 1)

# Mata:

a_cvmat_o<-ts(anual$`07-Mata de Minas Gerais`$cv, start=c(2012), frequency = 1)
a_cvmat_d<-ts(anual$`07-Mata de Minas Gerais`$cv.1, start=c(2012), frequency = 1)
a_cvmat_tx<-ts(anual$`07-Mata de Minas Gerais`$cv.6, start=c(2012), frequency = 1)

# Norte:

a_cvnrt_o<-ts(anual$`08-Norte de Minas`$cv, start=c(2012), frequency = 1)
a_cvnrt_d<-ts(anual$`08-Norte de Minas`$cv.1, start=c(2012), frequency = 1)
a_cvnrt_tx<-ts(anual$`08-Norte de Minas`$cv.6, start=c(2012), frequency = 1)

# Vale:

a_cvval_o<-ts(anual$`09-Vale do Rio Doce`$cv, start=c(2012), frequency = 1)
a_cvval_d<-ts(anual$`09-Vale do Rio Doce`$cv.1, start=c(2012), frequency = 1)
a_cvval_tx<-ts(anual$`09-Vale do Rio Doce`$cv.6, start=c(2012), frequency = 1)

# Central:

a_cvcen_o<-ts(anual$`10-Central`$cv, start=c(2012), frequency = 1)
a_cvcen_d<-ts(anual$`10-Central`$cv.1, start=c(2012), frequency = 1)
a_cvcen_tx<-ts(anual$`10-Central`$cv.6, start=c(2012), frequency = 1)

# MG:

a_cvmg_o<-ts(anual$`11 - Minas Gerais`$cv, start=c(2012), frequency = 1)
a_cvmg_d<-ts(anual$`11 - Minas Gerais`$cv.1, start=c(2012), frequency = 1)
a_cvmg_tx<-ts(anual$`11 - Minas Gerais`$cv.6, start=c(2012), frequency = 1)}


### GRÁFICOS ###################################################################

# Individuais:

par(mfrow=c(1,1))

plot.ts(t_cvbh_o,col = "black", xlab="Ano", ylab="CV(%)",main="01 - Belo Horizonte",lwd=2)

plot.ts(a_cvbh_o*100,col = "black", xlab="Ano", ylab="CV(%)",main="01 - Belo Horizonte",lwd=2)

# Duplas:

# Ocupada

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvbh_o*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvbh_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("1 - Belo Horizonte (Total de Ocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvent_o*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvent_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("2 - Entorno de Belo Horizonte (Total de Ocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvcol_o*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvcol_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("3 - Colar Metropolitano de Belo Horizonte (Total de Ocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvrid_o*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvrid_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("4 - RIDE de Brasília em Minas (Total de Ocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvsul_o*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvsul_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("5 - Sul de Minas (Total de Ocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvtrg_o*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvtrg_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("6 - Triângulo Mineiro (Total de Ocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvmat_o*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvmat_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("7 - Zona da Mata (Total de Ocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvbh_o*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvbh_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("8 - Norte de Minas (Total de Ocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvnrt_o*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvnrt_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("9 - Norte de Minas (Total de Ocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvcen_o*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvcen_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("10 - Central (Total de Ocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvmg_o), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvmg_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("11 - Minas Gerais (Total de Ocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}


# Desocupada:

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvbh_d*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvbh_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("1 - Belo Horizonte (Total de Desocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvent_d*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvent_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("2 - Entorno de Belo Horizonte (Total de Desocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvcol_d*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvcol_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("3 - Colar Metropolitano de Belo Horizonte (Total de Desocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvrid_d*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvrid_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("4 - RIDE de Brasília em Minas (Total de Desocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvsul_d*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvsul_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("5 - Sul de Minas (Total de Desocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvtrg_d*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvtrg_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("6 - Triângulo Mineiro (Total de Desocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvmat_d*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvmat_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("7 - Zona da Mata (Total de Desocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvbh_d*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvbh_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("8 - Norte de Minas (Total de Desocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvnrt_d*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvnrt_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("9 - Norte de Minas (Total de Desocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvcen_d*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvcen_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("10 - Central (Total de Desocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvmg_d), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvmg_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("11 - Minas Gerais (Total de Desocupados)", outer=TRUE, line =-2,cex=1.5,font=2)}


# Taxa de desemprego:


{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvbh_tx*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvbh_tx,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("1 - Belo Horizonte (Taxa de Desocupação)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvent_tx*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvent_tx,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("2 - Entorno de Belo Horizonte (Taxa de Desocupação)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvcol_tx*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvcol_tx,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("3 - Colar Metropolitano de Belo Horizonte (Taxa de Desocupação)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvrid_tx*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvrid_tx,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("4 - RIDE de Brasília em Minas (Taxa de Desocupação)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvsul_tx*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvsul_tx,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("5 - Sul de Minas (Taxa de Desocupação)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvtrg_tx*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvtrg_tx,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("6 - Triângulo Mineiro (Taxa de Desocupação)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvmat_tx*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvmat_tx,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("7 - Zona da Mata (Taxa de Desocupação)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvbh_tx*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvbh_tx,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("8 - Norte de Minas (Taxa de Desocupação)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvnrt_tx*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvnrt_tx,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("9 - Norte de Minas (Taxa de Desocupação)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvcen_tx*100), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvcen_tx,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("10 - Central (Taxa de Desocupação)", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((a_cvmg_tx), col = "black", xlab="Ano", ylab="CV(%)",main="",lwd=2)
  plot.ts(t_cvmg_tx,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("11 - Minas Gerais (Taxa de Desocupação)", outer=TRUE, line =-2,cex=1.5,font=2)}










