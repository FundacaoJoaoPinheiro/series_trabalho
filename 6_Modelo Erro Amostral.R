################################################################################
##                          MODELO ERRO AMOSTRAL                              ##
################################################################################

options(scipen=999)

## Leitura da base de dados:

base <- readRDS("data/baseMG_k.RDS")

## Utilizando conjunto de funções já prontas para o script

source("data/funcoes/01_funcoes_pseudo_erro.R")


################################################################################
### BELO HORIZONTE

dbbh<-base[["01-Belo Horizonte"]]

## Definindo variáveis adicionais

colnames(dbbh)
t = c(1:nrow(dbbh))
lags = 24
T = nrow(dbbh)
K = (ncol(dbbh)-1)/2






