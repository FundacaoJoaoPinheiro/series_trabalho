################################################################################
##          CRIANDO OS DFS PARA OS 10 ESTRATOS - 1T2012 AO 3T2024             ##
################################################################################


# Diretório principal

setwd("C:/FJP2425/Programacao")

library(PNADcIBGE)
library(survey)
library(tidyverse)

dev.new()
dev.new()

## Lendo todos os arquivos .RDS
  ## Lembrete: o caminho deles está dentro do diretório principal

arquivos <- list.files("data/estimativas", pattern = "\\.RDS$", full.names = TRUE)

pnadcrds <- lapply(arquivos, readRDS)

str(pnadcrds) # para conferir o objeto


# Diagnóstico do upload:

dimensao <- sapply(pnadcrds, dim)

dimensao_df <- data.frame(t(dimensao))
colnames(dimensao_df) <- c("Linhas", "Colunas")
print(dimensao_df)


View(pnadcrds[[26]])  # Possível conferir os dados com o site do IBGE


#### Criando uma coluna de trimestres para os gráficos posteriores (utilizar somente se necessário)
    ## Caso necessário, facilita a plotagem

#trimestres <- seq(from = as.Date("2012-01-01"), to = as.Date("2024-09-01"), by="quarter")

#tri_format <- paste(format(trimestres, "%Y"), 
#                   "Q", 
#                   ceiling(as.numeric(format(trimestres, "%m")) / 3), 
#                   sep = "")

#length(tri_format) ## Verificando se deu certo


################################################################################
## CRIANDO OS 11 DFS

## Minas Gerais (linha: 11)

# Estimativas diretas

mg<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                     2,15,28,41,
                                     3,16,29,42,
                                     4,17,30,43,
                                     5,18,31,44,
                                     6,19,32,45,
                                     7,20,33,46,
                                     8,21,34,47,
                                     9,22,35,48,
                                     10,23,36,49,
                                     11,24,37,50,
                                     12,25,38,51,
                                     13,26,39,52)], function(df) {
                                           data.frame(
                                             "Período" = as.character(df[11, 8]),
                                             "Total de ocupados" = as.numeric(df[11,2]),
                                             "sd_o" = as.numeric(df[11,3]),
                                             "Total de desocupados" = as.numeric(df[11, 4]),
                                             "sd_d" = as.numeric(df[11,5]),
                                             "Taxa de desocupação" = as.numeric(df[11, 6]),
                                             "sd_txd" = as.numeric(df[11,7])
                                           )
                                         }))
#View(mg)

## Calculando os coeficientes de variação e os adicionando ao Data Frame:

## Exemplo script Caio:

#    y_1 <- base$desocupada/1000
#    se_db_1 <- base$se_d/1000
#    cv_db_1 <- se_db_1/y_1


## CV para as três variáveis
  ## Incluindo esses valores no data frame

mg <- cbind(
  mg[, 1:3],
  `CV.ocupados` = with(mg, `sd_o` / `Total.de.ocupados` * 100),
  mg[, 4:5],  
  `CV.desocupados` = with(mg, `sd_d` / `Total.de.desocupados` * 100),
  mg[, 6:7],
  `CV.taxa` = with(mg, `sd_txd` / `Taxa.de.desocupação` * 100)
)

#View(mg)

## Gráficos:

  ## Transformando em série temporal para facilitar o plot

{mg_oc<-ts(mg$Total.de.ocupados,start=c(2012,1),frequency = 4)
cvmg_o<-ts(mg$CV.ocupados,start= c(2012,1),frequency=4)

mg_d<-ts(mg$Total.de.desocupados,start=c(2012,1),frequency = 4)
cvmg_d<-ts(mg$CV.desocupadosstart=c(2012,1),frequency = 4)

mg_txd<-ts(mg$Taxa.de.desocupação,start=c(2012,1),frequency = 4)
cvmg_txd<-ts(mg$CV.taxa,start=c(2012,1),frequency = 4)
}

  ## Realizando os plots (por variável)

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
plot.ts((mg_oc/1000), col = "black", xlab="Ano", ylab="Ocupados (mil pessoas)",main="",lwd=2)
plot.ts(cvmg_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
mtext("Total de Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((mg_d/1000), col = "black", xlab="Ano", ylab="Desocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvmg_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("Total de Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((mg_txd*100), col = "black", xlab="Ano", ylab="Taxa de desocupação (%)",main="",lwd=2)
  plot.ts(cvmg_txd,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("Total de Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}


################################
### Belo Horizonte (linha: 1)

bh<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                     2,15,28,41,
                                     3,16,29,42,
                                     4,17,30,43,
                                     5,18,31,44,
                                     6,19,32,45,
                                     7,20,33,46,
                                     8,21,34,47,
                                     9,22,35,48,
                                     10,23,36,49,
                                     11,24,37,50,
                                     12,25,38,51,
                                     13,26,39,52)], function(df) {
                                       data.frame(
                                         "Período" = as.character(df[1, 8]),
                                         "Total de ocupados" = as.numeric(df[1,2]),
                                         "sd_o" = as.numeric(df[1,3]),
                                         "Total de desocupados" = as.numeric(df[1, 4]),
                                         "sd_d" = as.numeric(df[1,5]),
                                         "Taxa de desocupação" = as.numeric(df[1, 6]),
                                         "sd_txd" = as.numeric(df[1,7])
                                       )
                                   }))

#View(bh)

## Calculando CV:

bh <- cbind(
  bh[, 1:3],`CV.ocupados` = with(bh, `sd_o` / `Total.de.ocupados` * 100),
  bh[, 4:5],`CV.desocupados` = with(bh, `sd_d` / `Total.de.desocupados` * 100),
  bh[, 6:7],`CV.taxa` = with(bh, `sd_txd` / `Taxa.de.desocupação` * 100)
)

#View(bh)


## Gráficos:

# Transformando em série temporal para facilitar o plot

{bh_oc<-ts(bh$Total.de.ocupados,start=c(2012,1),frequency = 4)
cvbh_o<-ts(bh$CV.ocupados,start= c(2012,1),frequency=4)

bh_d<-ts(bh$Total.de.desocupados,start=c(2012,1),frequency = 4)
cvbh_d<-ts(bh$CV.desocupados,start=c(2012,1),frequency = 4)

bh_txd<-ts(bh$Taxa.de.desocupação,start=c(2012,1),frequency = 4)
cvbh_txd<-ts(bh$CV.taxa,start=c(2012,1),frequency = 4)}


# Realizando os plots (por variável)

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((bh_oc/1000), col = "black", xlab="Ano", ylab="Ocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvbh_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("1 - Belo Horizonte", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((bh_d/1000), col = "black", xlab="Ano", ylab="Desocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvbh_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("1 - Belo Horizonte", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((bh_txd*100), col = "black", xlab="Ano", ylab="Taxa de desocupação (%)",main="",lwd=2)
  plot.ts(cvbh_txd,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("1 - Belo Horizonte", outer=TRUE, line =-2,cex=1.5,font=2)}



######################################
### Entorno Metropolitano de BH (linha: 2)

entornobh<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                            2,15,28,41,
                                            3,16,29,42,
                                            4,17,30,43,
                                            5,18,31,44,
                                            6,19,32,45,
                                            7,20,33,46,
                                            8,21,34,47,
                                            9,22,35,48,
                                            10,23,36,49,
                                            11,24,37,50,
                                            12,25,38,51,
                                            13,26,39,52)], function(df) {
                                              data.frame(
                                                "Período" = as.character(df[2, 8]),
                                                "Total de ocupados" = as.numeric(df[2,2]),
                                                "sd_o" = as.numeric(df[2,3]),
                                                "Total de desocupados" = as.numeric(df[2, 4]),
                                                "sd_d" = as.numeric(df[2,5]),
                                                "Taxa de desocupação" = as.numeric(df[2, 6]),
                                                "sd_txd" = as.numeric(df[2,7])
                                              )
                                            }))

#View(entornobh)


## Calculando CV:

entornobh <- cbind(
  entornobh[, 1:3],`CV.ocupados` = with(entornobh, `sd_o` / `Total.de.ocupados` * 100),
  entornobh[, 4:5],`CV.desocupados` = with(entornobh, `sd_d` / `Total.de.desocupados` * 100),
  entornobh[, 6:7],`CV.taxa` = with(entornobh, `sd_txd` / `Taxa.de.desocupação` * 100)
)


#View(entornobh)


## Gráficos:

# Transformando em série temporal para facilitar o plot

{entornobh_oc<-ts(entornobh$Total.de.ocupados,start=c(2012,1),frequency = 4)
  cventornobh_o<-ts(entornobh$CV.ocupados,start= c(2012,1),frequency=4)
  
  entornobh_d<-ts(entornobh$Total.de.desocupados,start=c(2012,1),frequency = 4)
  cventornobh_d<-ts(entornobh$CV.desocupados,start=c(2012,1),frequency = 4)
  
  entornobh_txd<-ts(entornobh$Taxa.de.desocupação,start=c(2012,1),frequency = 4)
  cventornobh_txd<-ts(entornobh$CV.taxa,start=c(2012,1),frequency = 4)}


# Realizando os plots (por variável)

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((entornobh_oc/1000), col = "black", xlab="Ano", ylab="Ocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cventornobh_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("2 - Entorno Metropolitano de BH", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((entornobh_d/1000), col = "black", xlab="Ano", ylab="Desocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cventornobh_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("2 - Entorno Metropolitano de BH", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((entornobh_txd*100), col = "black", xlab="Ano", ylab="Taxa de desocupação (%)",main="",lwd=2)
  plot.ts(cventornobh_txd,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("2 - Entorno Metropolitano de BH", outer=TRUE, line =-2,cex=1.5,font=2)}



########################################
### Colar Metropolitano de BH (linha: 3)

colarbh<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                          2,15,28,41,
                                          3,16,29,42,
                                          4,17,30,43,
                                          5,18,31,44,
                                          6,19,32,45,
                                          7,20,33,46,
                                          8,21,34,47,
                                          9,22,35,48,
                                          10,23,36,49,
                                          11,24,37,50,
                                          12,25,38,51,
                                          13,26,39,52)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[3, 8]),
                                              "Total de ocupados" = as.numeric(df[3,2]),
                                              "sd_o" = as.numeric(df[3,3]),
                                              "Total de desocupados" = as.numeric(df[3, 4]),
                                              "sd_d" = as.numeric(df[3,5]),
                                              "Taxa de desocupação" = as.numeric(df[3, 6]),
                                              "sd_txd" = as.numeric(df[3,7])
                                            )
                                          }))

#View(colarbh)


## Calculando CV:

colarbh <- cbind(
  colarbh[, 1:3],`CV.ocupados` = with(colarbh, `sd_o` / `Total.de.ocupados` * 100),
  colarbh[, 4:5],`CV.desocupados` = with(colarbh, `sd_d` / `Total.de.desocupados` * 100),
  colarbh[, 6:7],`CV.taxa` = with(colarbh, `sd_txd` / `Taxa.de.desocupação` * 100)
)


#View(colarbh)


## Gráficos:

# Transformando em série temporal para facilitar o plot

{colarbh_oc<-ts(colarbh$Total.de.ocupados,start=c(2012,1),frequency = 4)
  cvcolarbh_o<-ts(colarbh$CV.ocupados,start= c(2012,1),frequency=4)
  
  colarbh_d<-ts(colarbh$Total.de.desocupados,start=c(2012,1),frequency = 4)
  cvcolarbh_d<-ts(colarbh$CV.desocupados,start=c(2012,1),frequency = 4)
  
  colarbh_txd<-ts(colarbh$Taxa.de.desocupação,start=c(2012,1),frequency = 4)
  cvcolarbh_txd<-ts(colarbh$CV.taxa,start=c(2012,1),frequency = 4)}


# Realizando os plots (por variável)

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((colarbh_oc/1000), col = "black", xlab="Ano", ylab="Ocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvcolarbh_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("3 - Colar Metropolitano de BH", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((colarbh_d/1000), col = "black", xlab="Ano", ylab="Desocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvcolarbh_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("3 - Colar Metropolitano de BH", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((colarbh_txd*100), col = "black", xlab="Ano", ylab="Taxa de desocupação (%)",main="",lwd=2)
  plot.ts(cvcolarbh_txd,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("3 - Colar Metropolitano de BH", outer=TRUE, line =-2,cex=1.5,font=2)}



########################################
### RIDE de Brasília em Minas (linha: 4)

RIDE<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                       2,15,28,41,
                                       3,16,29,42,
                                       4,17,30,43,
                                       5,18,31,44,
                                       6,19,32,45,
                                       7,20,33,46,
                                       8,21,34,47,
                                       9,22,35,48,
                                       10,23,36,49,
                                       11,24,37,50,
                                       12,25,38,51,
                                       13,26,39,52)], function(df) {
                                         data.frame(
                                           "Período" = as.character(df[4, 8]),
                                           "Total de ocupados" = as.numeric(df[4,2]),
                                           "sd_o" = as.numeric(df[4,3]),
                                           "Total de desocupados" = as.numeric(df[4, 4]),
                                           "sd_d" = as.numeric(df[4,5]),
                                           "Taxa de desocupação" = as.numeric(df[4, 6]),
                                           "sd_txd" = as.numeric(df[4,7])
                                         )
                                       }))

#View(RIDE)


## Calculando CV:

RIDE <- cbind(
  RIDE[, 1:3],`CV.ocupados` = with(RIDE, `sd_o` / `Total.de.ocupados` * 100),
  RIDE[, 4:5],`CV.desocupados` = with(RIDE, `sd_d` / `Total.de.desocupados` * 100),
  RIDE[, 6:7],`CV.taxa` = with(RIDE, `sd_txd` / `Taxa.de.desocupação` * 100)
)


#View(RIDE)


## Gráficos:

# Transformando em série temporal para facilitar o plot

{RIDE_oc<-ts(RIDE$Total.de.ocupados,start=c(2012,1),frequency = 4)
  cvRIDE_o<-ts(RIDE$CV.ocupados,start= c(2012,1),frequency=4)
  
  RIDE_d<-ts(RIDE$Total.de.desocupados,start=c(2012,1),frequency = 4)
  cvRIDE_d<-ts(RIDE$CV.desocupados,start=c(2012,1),frequency = 4)
  
  RIDE_txd<-ts(RIDE$Taxa.de.desocupação,start=c(2012,1),frequency = 4)
  cvRIDE_txd<-ts(RIDE$CV.taxa,start=c(2012,1),frequency = 4)}


# Realizando os plots (por variável)

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((RIDE_oc/1000), col = "black", xlab="Ano", ylab="Ocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvRIDE_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("4 - RIDE de Brasília em Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((RIDE_d/1000), col = "black", xlab="Ano", ylab="Desocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvRIDE_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("4 - RIDE de Brasília em Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((RIDE_txd*100), col = "black", xlab="Ano", ylab="Taxa de desocupação (%)",main="",lwd=2)
  plot.ts(cvRIDE_txd,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("4 - RIDE de Brasília em Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}



#####################################
### Sul de Minas (linha: 5)

sulmg<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                           2,15,28,41,
                                           3,16,29,42,
                                           4,17,30,43,
                                           5,18,31,44,
                                           6,19,32,45,
                                           7,20,33,46,
                                           8,21,34,47,
                                           9,22,35,48,
                                           10,23,36,49,
                                           11,24,37,50,
                                           12,25,38,51,
                                           13,26,39,52)], function(df) {
                                             data.frame(
                                               "Período" = as.character(df[5, 8]),
                                               "Total de ocupados" = as.numeric(df[5,2]),
                                               "sd_o" = as.numeric(df[5,3]),
                                               "Total de desocupados" = as.numeric(df[5, 4]),
                                               "sd_d" = as.numeric(df[5,5]),
                                               "Taxa de desocupação" = as.numeric(df[5, 6]),
                                               "sd_txd" = as.numeric(df[5,7])
                                             )
                                           }))

#View(sulmg)


## Calculando CV:

sulmg <- cbind(
  sulmg[, 1:3],`CV.ocupados` = with(sulmg, `sd_o` / `Total.de.ocupados` * 100),
  sulmg[, 4:5],`CV.desocupados` = with(sulmg, `sd_d` / `Total.de.desocupados` * 100),
  sulmg[, 6:7],`CV.taxa` = with(sulmg, `sd_txd` / `Taxa.de.desocupação` * 100)
)


#View(sulmg)


## Gráficos:

# Transformando em série temporal para facilitar o plot

{sulmg_oc<-ts(sulmg$Total.de.ocupados,start=c(2012,1),frequency = 4)
  cvsulmg_o<-ts(sulmg$CV.ocupados,start= c(2012,1),frequency=4)
  
  sulmg_d<-ts(sulmg$Total.de.desocupados,start=c(2012,1),frequency = 4)
  cvsulmg_d<-ts(sulmg$CV.desocupados,start=c(2012,1),frequency = 4)
  
  sulmg_txd<-ts(sulmg$Taxa.de.desocupação,start=c(2012,1),frequency = 4)
  cvsulmg_txd<-ts(sulmg$CV.taxa,start=c(2012,1),frequency = 4)}


# Realizando os plots (por variável)

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((sulmg_oc/1000), col = "black", xlab="Ano", ylab="Ocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvsulmg_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("5 - Sul de Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((sulmg_d/1000), col = "black", xlab="Ano", ylab="Desocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvsulmg_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("5 - Sul de Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((sulmg_txd*100), col = "black", xlab="Ano", ylab="Taxa de desocupação (%)",main="",lwd=2)
  plot.ts(cvsulmg_txd,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("5 - Sul de Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}



########################################################
### Triângulo Mineiro (linha: 6)

trng<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                            2,15,28,41,
                                            3,16,29,42,
                                            4,17,30,43,
                                            5,18,31,44,
                                            6,19,32,45,
                                            7,20,33,46,
                                            8,21,34,47,
                                            9,22,35,48,
                                            10,23,36,49,
                                            11,24,37,50,
                                            12,25,38,51,
                                            13,26,39,52)], function(df) {
                                              data.frame(
                                                "Período" = as.character(df[6, 8]),
                                                "Total de ocupados" = as.numeric(df[6,2]),
                                                "sd_o" = as.numeric(df[6,3]),
                                                "Total de desocupados" = as.numeric(df[6, 4]),
                                                "sd_d" = as.numeric(df[6,5]),
                                                "Taxa de desocupação" = as.numeric(df[6, 6]),
                                                "sd_txd" = as.numeric(df[6,7])
                                              )
                                            }))

#View(trng)

## Calculando CV:

trng <- cbind(
  trng[, 1:3],`CV.ocupados` = with(trng, `sd_o` / `Total.de.ocupados` * 100),
  trng[, 4:5],`CV.desocupados` = with(trng, `sd_d` / `Total.de.desocupados` * 100),
  trng[, 6:7],`CV.taxa` = with(trng, `sd_txd` / `Taxa.de.desocupação` * 100)
)


#View(trng)

## Gráficos:

# Transformando em série temporal para facilitar o plot

{trng_oc<-ts(trng$Total.de.ocupados,start=c(2012,1),frequency = 4)
  cvtrngmg_o<-ts(trng$CV.ocupados,start= c(2012,1),frequency=4)
  
  trng_d<-ts(trng$Total.de.desocupados,start=c(2012,1),frequency = 4)
  cvtrng_d<-ts(trng$CV.desocupados,start=c(2012,1),frequency = 4)
  
  trng_txd<-ts(trng$Taxa.de.desocupação,start=c(2012,1),frequency = 4)
  cvtrng_txd<-ts(trng$CV.taxa,start=c(2012,1),frequency = 4)}


# Realizando os plots (por variável)

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((trng_oc/1000), col = "black", xlab="Ano", ylab="Ocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvtrngmg_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("6 - Triângulo Mineiro", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((trng_d/1000), col = "black", xlab="Ano", ylab="Desocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvtrng_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("6 - Triângulo Mineiro", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((trng_txd*100), col = "black", xlab="Ano", ylab="Taxa de desocupação (%)",main="",lwd=2)
  plot.ts(cvtrng_txd,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("6 - Triângulo Mineiro", outer=TRUE, line =-2,cex=1.5,font=2)}



###############################################################
### Mata de Minas Gerais (linha: 7)

zonamata<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                           2,15,28,41,
                                           3,16,29,42,
                                           4,17,30,43,
                                           5,18,31,44,
                                           6,19,32,45,
                                           7,20,33,46,
                                           8,21,34,47,
                                           9,22,35,48,
                                           10,23,36,49,
                                           11,24,37,50,
                                           12,25,38,51,
                                           13,26,39,52)], function(df) {
                                             data.frame(
                                               "Período" = as.character(df[7, 8]),
                                               "Total de ocupados" = as.numeric(df[7,2]),
                                               "sd_o" = as.numeric(df[7,3]),
                                               "Total de desocupados" = as.numeric(df[7, 4]),
                                               "sd_d" = as.numeric(df[7,5]),
                                               "Taxa de desocupação" = as.numeric(df[7, 6]),
                                               "sd_txd" = as.numeric(df[7,7])
                                             )
                                           }))

#View(zonamata)


## Calculando CV:

zonamata <- cbind(
  zonamata[, 1:3],`CV.ocupados` = with(zonamata, `sd_o` / `Total.de.ocupados` * 100),
  zonamata[, 4:5],`CV.desocupados` = with(zonamata, `sd_d` / `Total.de.desocupados` * 100),
  zonamata[, 6:7],`CV.taxa` = with(zonamata, `sd_txd` / `Taxa.de.desocupação` * 100)
)


#View(zonamata)

## Gráficos:

# Transformando em série temporal para facilitar o plot

{zonamata_oc<-ts(zonamata$Total.de.ocupados,start=c(2012,1),frequency = 4)
  cvzonamata_o<-ts(zonamata$CV.ocupados,start= c(2012,1),frequency=4)
  
  zonamata_d<-ts(zonamata$Total.de.desocupados,start=c(2012,1),frequency = 4)
  cvzonamata_d<-ts(zonamata$CV.desocupados,start=c(2012,1),frequency = 4)
  
  zonamata_txd<-ts(zonamata$Taxa.de.desocupação,start=c(2012,1),frequency = 4)
  cvzonamata_txd<-ts(zonamata$CV.taxa,start=c(2012,1),frequency = 4)}


# Realizando os plots (por variável)

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((zonamata_oc/1000), col = "black", xlab="Ano", ylab="Ocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvzonamata_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("7 - Zona da Mata", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((zonamata_d/1000), col = "black", xlab="Ano", ylab="Desocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvzonamata_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("7 - Zona da Mata", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((zonamata_txd*100), col = "black", xlab="Ano", ylab="Taxa de desocupação (%)",main="",lwd=2)
  plot.ts(cvzonamata_txd,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("7 - Zona da Mata", outer=TRUE, line =-2,cex=1.5,font=2)}



############################################################
### Norte de Minas (Linha: 8)

nortemg<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                             2,15,28,41,
                                             3,16,29,42,
                                             4,17,30,43,
                                             5,18,31,44,
                                             6,19,32,45,
                                             7,20,33,46,
                                             8,21,34,47,
                                             9,22,35,48,
                                             10,23,36,49,
                                             11,24,37,50,
                                             12,25,38,51,
                                             13,26,39,52)], function(df) {
                                               data.frame(
                                                 "Período" = as.character(df[8, 8]),
                                                 "Total de ocupados" = as.numeric(df[8,2]),
                                                 "sd_o" = as.numeric(df[8,3]),
                                                 "Total de desocupados" = as.numeric(df[8, 4]),
                                                 "sd_d" = as.numeric(df[8,5]),
                                                 "Taxa de desocupação" = as.numeric(df[8, 6]),
                                                 "sd_txd" = as.numeric(df[8,7])
                                               )
                                             }))

#View(nortemg)


## Calculando CV:

nortemg <- cbind(
  nortemg[, 1:3],`CV.ocupados` = with(nortemg, `sd_o` / `Total.de.ocupados` * 100),
  nortemg[, 4:5],`CV.desocupados` = with(nortemg, `sd_d` / `Total.de.desocupados` * 100),
  nortemg[, 6:7],`CV.taxa` = with(nortemg, `sd_txd` / `Taxa.de.desocupação` * 100)
)


#View(nortemg)

## Gráficos:

# Transformando em série temporal para facilitar o plot

{nortemg_oc<-ts(nortemg$Total.de.ocupados,start=c(2012,1),frequency = 4)
  cvnortemg_o<-ts(nortemg$CV.ocupados,start= c(2012,1),frequency=4)
  
  nortemg_d<-ts(nortemg$Total.de.desocupados,start=c(2012,1),frequency = 4)
  cvnortemg_d<-ts(nortemg$CV.desocupados,start=c(2012,1),frequency = 4)
  
  nortemg_txd<-ts(nortemg$Taxa.de.desocupação,start=c(2012,1),frequency = 4)
  cvnortemg_txd<-ts(nortemg$CV.taxa,start=c(2012,1),frequency = 4)}


# Realizando os plots (por variável)

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((nortemg_oc/1000), col = "black", xlab="Ano", ylab="Ocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvnortemg_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("8 - Norte de Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((nortemg_d/1000), col = "black", xlab="Ano", ylab="Desocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvnortemg_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("8 - Norte de Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((nortemg_txd*100), col = "black", xlab="Ano", ylab="Taxa de desocupação (%)",main="",lwd=2)
  plot.ts(cvnortemg_txd,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("8 - Norte de Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}



######################################################
### Vale do Rio Doce (linha: 9)

riodoce<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                          2,15,28,41,
                                          3,16,29,42,
                                          4,17,30,43,
                                          5,18,31,44,
                                          6,19,32,45,
                                          7,20,33,46,
                                          8,21,34,47,
                                          9,22,35,48,
                                          10,23,36,49,
                                          11,24,37,50,
                                          12,25,38,51,
                                          13,26,39,52)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[9, 8]),
                                              "Total de ocupados" = as.numeric(df[9,2]),
                                              "sd_o" = as.numeric(df[9,3]),
                                              "Total de desocupados" = as.numeric(df[9, 4]),
                                              "sd_d" = as.numeric(df[9,5]),
                                              "Taxa de desocupação" = as.numeric(df[9, 6]),
                                              "sd_txd" = as.numeric(df[9,7])
                                            )
                                          }))

#View(riodoce)


## Calculando CV:

riodoce <- cbind(
  riodoce[, 1:3],`CV.ocupados` = with(riodoce, `sd_o` / `Total.de.ocupados` * 100),
  riodoce[, 4:5],`CV.desocupados` = with(riodoce, `sd_d` / `Total.de.desocupados` * 100),
  riodoce[, 6:7],`CV.taxa` = with(riodoce, `sd_txd` / `Taxa.de.desocupação` * 100)
)


#View(riodoce)

## Gráficos:

# Transformando em série temporal para facilitar o plot

{riodoce_oc<-ts(riodoce$Total.de.ocupados,start=c(2012,1),frequency = 4)
  cvriodoce_o<-ts(riodoce$CV.ocupados,start= c(2012,1),frequency=4)
  
  riodoce_d<-ts(riodoce$Total.de.desocupados,start=c(2012,1),frequency = 4)
  cvriodoce_d<-ts(riodoce$CV.desocupados,start=c(2012,1),frequency = 4)
  
  riodoce_txd<-ts(riodoce$Taxa.de.desocupação,start=c(2012,1),frequency = 4)
  cvriodoce_txd<-ts(riodoce$CV.taxa,start=c(2012,1),frequency = 4)}


# Realizando os plots (por variável)

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((riodoce_oc/1000), col = "black", xlab="Ano", ylab="Ocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvriodoce_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("9 - Vale do Rio Doce", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((riodoce_d/1000), col = "black", xlab="Ano", ylab="Desocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvriodoce_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("9 - Vale do Rio Doce", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((riodoce_txd*100), col = "black", xlab="Ano", ylab="Taxa de desocupação (%)",main="",lwd=2)
  plot.ts(cvriodoce_txd,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("9 - Vale do Rio Doce", outer=TRUE, line =-2,cex=1.5,font=2)}



########################################################
### Central de Minas (linha: 10):

central<-do.call(rbind, lapply(pnadcrds[c(1,14,27,40,
                                          2,15,28,41,
                                          3,16,29,42,
                                          4,17,30,43,
                                          5,18,31,44,
                                          6,19,32,45,
                                          7,20,33,46,
                                          8,21,34,47,
                                          9,22,35,48,
                                          10,23,36,49,
                                          11,24,37,50,
                                          12,25,38,51,
                                          13,26,39,52)], function(df) {
                                            data.frame(
                                              "Período" = as.character(df[10, 8]),
                                              "Total de ocupados" = as.numeric(df[10,2]),
                                              "sd_o" = as.numeric(df[10,3]),
                                              "Total de desocupados" = as.numeric(df[10, 4]),
                                              "sd_d" = as.numeric(df[10,5]),
                                              "Taxa de desocupação" = as.numeric(df[10, 6]),
                                              "sd_txd" = as.numeric(df[10,7])
                                            )
                                          }))

#View(central)


## Calculando CV:

central <- cbind(
  central[, 1:3],`CV.ocupados` = with(central, `sd_o` / `Total.de.ocupados` * 100),
  central[, 4:5],`CV.desocupados` = with(central, `sd_d` / `Total.de.desocupados` * 100),
  central[, 6:7],`CV.taxa` = with(central, `sd_txd` / `Taxa.de.desocupação` * 100)
)


#View(central)

## Gráficos:

# Transformando em série temporal para facilitar o plot

{central_oc<-ts(central$Total.de.ocupados,start=c(2012,1),frequency = 4)
  cvcentral_o<-ts(central$CV.ocupados,start= c(2012,1),frequency=4)
  
  central_d<-ts(central$Total.de.desocupados,start=c(2012,1),frequency = 4)
  cvcentral_d<-ts(central$CV.desocupados,start=c(2012,1),frequency = 4)
  
  central_txd<-ts(central$Taxa.de.desocupação,start=c(2012,1),frequency = 4)
  cvcentral_txd<-ts(central$CV.taxa,start=c(2012,1),frequency = 4)}


# Realizando os plots (por variável)

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((central_oc/1000), col = "black", xlab="Ano", ylab="Ocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvcentral_o,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("10 - Central de Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((central_d/1000), col = "black", xlab="Ano", ylab="Desocupados (mil pessoas)",main="",lwd=2)
  plot.ts(cvcentral_d,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("10 - Central de Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}

{par(mfrow=c(1,2),oma = c(2, 2, 3, 2))
  plot.ts((central_txd*100), col = "black", xlab="Ano", ylab="Taxa de desocupação (%)",main="",lwd=2)
  plot.ts(cvcentral_txd,col = "blue", xlab="Ano", ylab="CV(%)",main="", lwd=2)
  mtext("10 - Central de Minas Gerais", outer=TRUE, line =-2,cex=1.5,font=2)}



################################################################################
### PLOTS GERAIS E OUTRAS AVALIAÇÕES


## Plot de todos os CVs:

# Ocupados:

par(mfrow=c(1,1))

{seriescvoc<-cbind(cvbh_o,cventornobh_o,cvcolarbh_o,cvRIDE_o,cvsulmg_o,cvtrngmg_o,cvzonamata_o,cvnortemg_o,cvriodoce_o,cvcentral_o)
colnames(seriescvoc)<-c("BH", "Entorno BH", "Colar BH", "RIDE", "Sul", "Triângulo", "Mata", "Norte MG", "Rio doce", "Central" )
}

{cores <- c("black", "blue", "red", "green", "purple", "orange", "brown", "pink", "cyan", "yellow")
plot.ts(seriescvoc,plot.type = "single",main="Coeficiente de variação (%) para o total de ocupados",
        col=cores,
        xlab="Ano",ylab="CV(%)",lwd=2)
  abline(h = 15, col = "black", lty = 2, lwd = 2)
  legend("topleft",legend=colnames(seriescvoc),col=cores,lwd = 1,cex=0.8)
}

# Desocupados:

{seriescvd<-cbind(cvbh_d,cventornobh_d,cvcolarbh_d,cvRIDE_d,cvsulmg_d,cvtrng_d,cvzonamata_d,cvnortemg_d,cvriodoce_d,cvcentral_d)
  colnames(seriescvd)<-c("BH", "Entorno BH", "Colar BH", "RIDE", "Sul", "Triângulo", "Mata", "Norte MG", "Rio doce", "Central" )
}

{cores <- c("black", "blue", "red", "green", "purple", "orange", "brown", "pink", "cyan", "yellow")
  plot.ts(seriescvd,plot.type = "single",main="Coeficiente de variação (%) para o total de desocupados",
          col=cores,
          xlab="Ano",ylab="CV(%)",lwd=2)
  abline(h = 15, col = "black", lty = 2, lwd = 2)
  legend("topleft",legend=colnames(seriescvoc),col=cores,lwd = 1,cex=0.8)
}

# Taxa de Desocupação:

{seriescvtxd<-cbind(cvbh_txd,cventornobh_txd,cvcolarbh_txd,cvRIDE_txd,cvsulmg_txd,cvtrng_txd,cvzonamata_txd,cvnortemg_txd,cvriodoce_txd,cvcentral_d)
  colnames(seriescvtxd)<-c("BH", "Entorno BH", "Colar BH", "RIDE", "Sul", "Triângulo", "Mata", "Norte MG", "Rio doce", "Central" )
}

{cores <- c("black", "blue", "red", "green", "purple", "orange", "brown", "pink", "cyan", "yellow")
  plot.ts(seriescvtxd,plot.type = "single",main="Coeficiente de variação (%) para a taxa de desocupação",
          col=cores,
          xlab="Ano",ylab="CV(%)",lwd=2)
  abline(h = 15, col = "black", lty = 2, lwd = 2)
  legend("topleft",legend=colnames(seriescvoc),col=cores,lwd = 1,cex=0.8)
}


## CV médio para períodos selecionados

## Toda a amostra

cvmedmg <- colMeans(mg[,c(4,7,10)])
print(cvmedmg)

cvmedbh<-colMeans(bh[,c(4,7,10)])
print(cvmedbh)

estratos<-list(bh,entornobh,colarbh,RIDE,sulmg,trng,zonamata,nortemg,riodoce,central)
cvmedestratos <- lapply(estratos, function(df) colMeans(df[, c(4, 7, 10), drop = FALSE], na.rm = TRUE))
print(cvmedestratos)


## Últimos quatro anos:

cvmedmg4 <- colMeans(mg[33:51,c(4,7,10)])
print(cvmedmg4)

cvmedestratos4 <- lapply(estratos, function(df) {
  colMeans(df[33:51, c(4, 7, 10), drop = FALSE], na.rm = TRUE)
})
print(cvmedestratos4)


## Últimos dois anos:

cvmedmg2 <- colMeans(mg[45:51,c(4,7,10)])
print(cvmedmg2)

cvmedestratos2 <- lapply(estratos, function(df) {
  colMeans(df[45:51, c(4, 7, 10), drop = FALSE], na.rm = TRUE)
})
print(cvmedestratos2)


### SALVANDO ARQUIVO LISTA #####################################################

 baseestr0424<-list("01-Belo Horizonte"=bh,"02-Entorno metropolitano de BH"=entornobh,"03-Colar metropolitano de BH"=colarbh,
                   "04-RIDE de Brasília em Minas"=RIDE, "05-Sul de Minas"=sulmg, "06-Triângulo Mineiro"=trng,
                   "07-Mata de Minas Gerais"=zonamata, "08-Norte de Minas"=nortemg, "09-Vale do Rio Doce"=riodoce,
                   "10-Central"=central, "11 - Minas Gerais"=mg)

 saveRDS(baseestr0424,file = "C:/FJP2425/Programacao/data/baseestr0424.rds")
 baseestr0424 <- readRDS("C:/FJP2425/Programacao/data/baseestr0424.RDS")
