################################################################################
##                           ESTUDO DO PACOTE DLM                             ##
################################################################################

library(dlm)
library(tidyverse)

################################################################################
## MODELO POLINOMIAL DE PRIMEIRA ORDEM:

## Modelo de random walk + ruído
  ## Modelo é constante
    ## Únicos parâmetros do modelo são as matrizes de variância de observação (V) e de evolução (W)

first<-dlm(FF = 1, V = 0.8, GG = 1, W = 0.1, m0 = 0, C0 = 100)

# ou

first<-dlmModPoly(order = 1, dV = 0.8, dW = 0.1, C0 = 100)


## Considerando um modelo de segunda ordem

myMod <- dlmModPoly() ## Esse aqui é o código que define o modelo genérico

FF(myMod)         ## Coeficiente de y_t

GG(myMod)         ## Coeficiente de theta_t

W(myMod)          ## Matriz de variância

m0(myMod)

V(myMod) <- 0.8   ## Matriz de variância


## Somando DLMs para formar um modelo mais complexo:
  # Ex: posso "somar" um componente de tendência estocástica

myMod <- dlmModPoly() + dlmModSeas(4)

myMod               # Para observar as matrizes


## Exemplo para 2 séries
  ## Tendência linear estocástica, sazonalidade trimestral e ambas séries possuem ruído

model2<- dlmModPoly(dV = 0.2, dW = c(0, 0.5)) %+%
  (dlmModSeas(4, dV = 0, dW = c(0, 0, 0.35)) %+% 
       dlmModPoly(1, dV = 0.1, dW = 0.03))

model2


################################################################################
### Modelo tempo variante

## Aqui, pelo menos F, V, G ou W tem que variar no tempo (_t)
  ## Aqui, adiciona-se ao modelo a matriz numérica X -> entrada dos dados
    ## As demais passam a ser representadas com um J na frente ("JFF")

u<- rnorm(25)
myMod <- dlmModReg(u, dV = 14.5)
myMod$JFF
head(myMod$X)


################################################################################
### Estimação via máxima verossimilhança
  
  ## Função primária para utilizar a estimação: dlmMLE
    ## Essencial para parâmetros desconhecidos

buildFun <- function(x) {
  dlmModPoly(1, dV = exp(x[1]), dW = exp(x[2]))
}

fit <- dlmMLE(Nile, parm = c(0,0), build = buildFun) # (0,0) é a definição de um ponto arbitrário

fit$convergence
dlmNile <- buildFun(fit$par)
V(dlmNile)
W(dlmNile)

StructTS(Nile, "level") ## Autor compara as variâncias com a estimação via "StructTS"


  ## Supondo que eu queira capturar a mudança de nível do rio em 1899
    ## Posso fazer isso inflando a variância do sistema em 1899 utilizando um multiplicador > 1

buildFun <- function(x) {
  m <- dlmModPoly(1, dV = exp(x[1]))
  m$JW <- matrix(1)
  m$X <- matrix(exp(x[2]), nc = 1, nr = length(Nile)) ## Pelo nc e o nr eu defino a dimensão de uma matriz
  j <- which(time(Nile) == 1899)
  m$X[j,1] <- m$X[j,1] * (1 + exp(x[3]))              ## Aqui o autor faz o choque no modelo
  return(m)
  }

fit <- dlmMLE(Nile, parm = c(0,0,0), build = buildFun)
fit$conv
dlmNileJump <- buildFun(fit$par)
V(dlmNileJump)
dlmNileJump$X[c(1, which(time(Nile) == 1899)), 1]



################################################################################
### Filtering, smoothing and forecasting

  # m_0 e C_0 representam a média e a variância da distribuição filtrada

####### Filteragem
  ## dlmFilter
    ## Retorna a filtragem de n+1 observações

nileJumpFilt <- dlmFilter(Nile, dlmNileJump)
plot(Nile, type = 'o', col = "seagreen")
lines(dropFirst(nileJumpFilt$m), type = 'o', pch = 20, col = "brown")

# As variâncias resultantes podem ser utilizadas, por exemplo, para computar
#   os intervalos de probabilidade da filtragem
    # Principal cmd que faz isso: dlmSvd2var

 attach(nileJumpFilt)
 v <- unlist(dlmSvd2var(U.C, D.C))
 pl <- dropFirst(m) + qnorm(0.05, sd = sqrt(v[-1]))
 pu <- dropFirst(m) + qnorm(0.95, sd = sqrt(v[-1]))
 detach()
 lines(pl, lty = 2, col = "brown")
 lines(pu, lty = 2, col = "brown")


######## Suavização:
 # Além de alimentar o modelo com um objeto resultante da filtragem, posso utilizar um vetor ou matriz
 
nileJumpSmooth <- dlmSmooth(nileJumpFilt)
plot(Nile, type = 'o', col = "seagreen")
attach(nileJumpSmooth)
lines(dropFirst(s), type = 'o', pch = 20, col = "brown")
v <- unlist(dlmSvd2var(U.S, D.S))
pl <- dropFirst(s) + qnorm(0.05, sd = sqrt(v[-1]))
pu <- dropFirst(s) + qnorm(0.95, sd = sqrt(v[-1]))
detach()
lines(pl, lty = 2, col = "brown")
lines(pu, lty = 2, col = "brown")


# 2 exemplo: consumo de gás no Reino Unido
  # Em escala logaritmica
  # Trimestral, LLT

# Primeiro: estimo as variâncias desconhecidas via ML

lGas <- log(UKgas)
dlmGas <- dlmModPoly() + dlmModSeas(4)

buildFun <- function(x) {
diag(W(dlmGas))[2:3] <- exp(x[1:2])
V(dlmGas) <- exp(x[3])
return(dlmGas)
}

(fit <- dlmMLE(lGas, parm = rep(0, 3), build = buildFun))$conv

dlmGas <- buildFun(fit$par)
drop(V(dlmGas))

diag(W(dlmGas))[2:3]

# Segundo: obtendo os componentes suavizados

gasSmooth <- dlmSmooth(lGas, mod = dlmGas)
x <- cbind(lGas, dropFirst(gasSmooth$s[,c(1,3)]))
colnames(x) <- c("Gas", "Trend", "Seasonal")
plot(x, type = 'o', main = "UK Gas Consumption") 


########## Previsão:
# dlmForecast

gasFilt <- dlmFilter(lGas, mod = dlmGas)
gasFore <- dlmForecast(gasFilt, nAhead = 20) # Aqui temos "a" (previsão dos estados futuros) e "R" (respectiva matrix var-cov associada)
sqrtR <- sapply(gasFore$R, function(x) sqrt(x[1,1])) # Extrai os dp das previsões associadas ao primeiro estado latente
pl <- gasFore$a[,1] + qnorm(0.05, sd = sqrtR) # pl e pu são os limites do intervalo de previsão
pu <- gasFore$a[,1] + qnorm(0.95, sd = sqrtR)
x <- ts.union(window(lGas, start = c(1982, 1)), # Aqui estou combinando os resultados em um ts
                 window(gasSmooth$s[,1], start = c(1982, 1)),
                 gasFore$a[,1], pl, pu)
plot(x, plot.type = "single", type = 'o', pch = c(1, 0, 20, 3, 3),
        col = c("darkgrey", "darkgrey", "brown", "yellow", "yellow"),
        ylab = "Log gas consumption")
legend("bottomright", legend = c("Observed",
                                    "Smoothed (deseasonalized)",
                                    "Forecasted level", "90% probability limit"),
          bty = 'n', pch = c(1, 0, 20, 3, 3), lty = 1,
          col = c("darkgrey", "darkgrey", "brown", "yellow", "yellow"))



### TESTE - TAXA DE DESOCUPAÇÃO BH #############################################

# Dados:

baseestr0324 <- readRDS("D:/FJP2425/Programacao/data/baseestr0324.RDS")
bh<-baseestr0324$`01-Belo Horizonte`
baseal0324 <- readRDS("D:/FJP2425/Programacao/data/basealinhada0324.RDS")
dtbh<-baseal0324$`01-Belo Horizonte` ## Arquivo "cru", saída direta da rotina da base por rotação
dbbh<-readRDS("D:/FJP2425/Programacao/data/pseudoerros/01_params_bh.RDS") ## Arquivo retirado da rotina de elaboração dos pseudo erros

## Definindo variáveis e inputs:

y <- bh$Total.de.desocupados
se_db <- bh$sd_d
cv_db <- bh$CV.desocupados
par_ar_erro <- dbbh$parerro_d

# Em ordem: total de ocupados; total de desocupados; taxa de desocupação

{bh_oc<-ts(bh$Total.de.ocupados[1:51],start=c(2012,1),frequency = 4)
  cvbh_o<-ts(bh$CV.ocupados[1:51],start= c(2012,1),frequency=4)
  
  bh_d<-ts(bh$Total.de.desocupados[1:51],start=c(2012,1),frequency = 4)
  cvbh_d<-ts(bh$CV.desocupados[1:51],start=c(2012,1),frequency = 4)
  
  bh_txd<-ts(bh$Taxa.de.desocupação[1:51],start=c(2012,1),frequency = 4)
  cvbh_txd<-ts(bh$CV.taxa[1:51],start=c(2012,1),frequency = 4)}

txbh<-bh_txd*100 # Para evitar possíveis problemas de escala


##### Filtragem e suavização para txd de bh

## Filtragem

# Link útil: https://statisticssu.github.io/STM/tutorial/statespace/statespace.html
# Primeiro Tenho que definir as seguintes matrizes:
  # F -> matriz de observação -> relaciona o vetor de estado ao y_t -> na eq. é o coeficiente de theta_t
    # Define como o estado contribui na observação
  # G -> matriz de transição do estado -> na eq. é o coeficiente de theta_t-1
    # Define a dinâmica do sistema
  # V -> variância do ruído de observação (v_t)
    # Erros nas observações
  # W -> variância do ruído do processo (w_t)
    # Erros na evolução dos estados

  # Atentar-se à dimensão dessas matrizes


# Estimando as variâncias de V e W via MLE
  # No link consultado, o autor troca o "X" utilizado no vignette por "param"  
  # Importante colocar o exp para garantir que as variâncias sejam positivas
  
modelBuild <- function(param) {
  dlm(FF = 1, V = exp(param[1]), GG = 1, W = exp(param[2]), m0 = 1000, C0 = 1000^2)
}

fit <- dlmMLE(txbh, parm = c(0,0), build = modelBuild)
exp(fit$par)
sqrt(exp(fit$par))

model_mle = dlm(FF = 1, V = exp(fit$par[1]), GG = 1, W = exp(fit$par[2]), m0 = 1000, C0 = 1000^2)
bhFilter <- dlmFilter(txbh, model_mle)
plot(txbh, type = 'l', col = "steelblue", lwd = 1.5)
lines(dropFirst(bhFilter$m), type = 'l', col = "orange", lwd = 1.5)
legend("topleft", legend = c("Observado", "Filtrado"), lwd = 1.5, lty = 1, 
       col = c("steelblue", "orange"))


## Suavização:

model_mle = dlm(FF = 1, V = exp(fit$par[1]), GG = 1, W = exp(fit$par[2]), m0 = 1000, C0 = 1000^2)

bhSmooth<- dlmSmooth(txbh,model_mle)

plot(txbh, type = 'l', col = "steelblue", lwd = 1.5)
lines(dropFirst(bhSmooth$s), type = 'l', col = "red", lwd = 1.5)
legend("topleft", legend = c("Observed","Smoothed"), lty = 1, lwd = 1.5, col = c("steelblue", "red"))


## Teste: local trend (order = 2) com componente sazonal trimestral (dlmModSeas(4))
  # Aviso: seguindo o exemplo do vignette com UKGas
  
  # Dificuldade: entender como o autor escreveu os valores dentro do buidFun

dlmtxdbh<- dlmModPoly(order = 2)+dlmModSeas(4)

View(dlmtxdbh)

buildFun <- function(x) {
  diag(W(dlmtxdbh))[2:3] <- exp(x[1:2])
  V(dlmtxdbh) <- exp(x[3])
  return(dlmtxdbh)
}

(fit <- dlmMLE(txbh, parm = rep(0, 3), build = buildFun))$conv  ## Verificar se convergiu para 0

dlmtxdbh <- buildFun(fit$par)
drop(V(dlmtxdbh))

diag(W(dlmtxdbh))[2:3]

txbhSmooth <- dlmSmooth(txbh, mod = dlmtxdbh)
x <- cbind(txbh, dropFirst(txbhSmooth$s[,c(1,3)]))
colnames(x) <- c("Taxa de Desocupação", "Tendência", "Sazonalidade")
plot(x, type = 'o', main = "Taxa de Despocupação de Belo Horizonte") 

plot.ts(txbh,col="black", lwd = 2, xlab = "Ano", ylab = "Taxa de Desocupação (%)", main = "01 - Belo Horizonte")
lines(x[,2], col = "red", lwd = 2)
legend("topleft", legend = c("Taxa de Desocupação - BH", "Tendência"), col = c("black", "red"), lwd = 2)



## Testando a implementação do modelo com o erro amostral (passo a passo)
  ## Se baseando em scripts anteriores

bh_sdtxd<-ts(bh$sd_txd[1:51],start=c(2012,1),frequency = 4)

  ## Modelo Estrutural:
    ## dlmModPoly(2) indica que é ordem = 2 (LLT), Sazonalidade no formato trigonométrico

dlm2<-dlmModPoly(2) + dlmModTrig(4) +dlmModReg(bh_sdtxd,addInt = FALSE)

  ## No script base, temos a variável "se_db", o que seria essa variável?
    ## Momentâneamente, substituí pelo erro padrão da taxa de desemprego

buildFun<-function(x) {
  dlm2$GG[6,6] <- par_ar_erro     # Onde obter?                                        
  W = matrix(0,6,6)
  W[1, 1] <- exp(params[1])
  W[2, 2] <- exp(params[2])
  W[3, 3] <- exp(params[3])
  W[6, 6] <- 1
  dlm2$W <- W
  V =  exp(params[4])
  dlm2$V <- V 
  return(dlm2)
}

## Estimando os hiperparâmetros (via MLE)

  # Ref: modelo$fit <- dlmMLE(y, modelo$initial,modelo$fn, hessian=T,control = list(maxit = 10^8))


(fit <- dlmMLE(txbh,modelo$initial,modelo$fn,hessian=T,control = list(maxit = 10^8), build = buildFun))$conv

  

### ESTIMAÇÃO BAYESIANA ########################################################

##### Foward filtering and backward sampling
  # É o nome dado ao próprio algoritmo que faz a estimação

plot(Nile, type = 'o', col = "seagreen")
nileFilt <- dlmFilter(Nile, dlmNile)
for (i in 1:10) # 10 simulated "true" levels
   lines(dropFirst(dlmBSample(nileFilt)), col = "brown")


###### Adaptive rejection Metropolis sampling

lmixnorm <- function(x, weights, means, sds) {
   log(crossprod(weights, exp(-0.5 * ((x - means) / sds)^2
                                - log(sds))))
}

y <- arms(0, myldens = lmixnorm,
             indFunc = function(x,...) (x > (-100)) * (x < 100),
             n = 5000, weights = c(1, 3, 2),
             means = c(-10, 0, 10), sds = c(7, 5, 2))
summary(y)

library(MASS)
truehist(y, prob = TRUE, ylim = c(0, 0.08), bty = 'o')
curve(colSums(c(1, 3, 2) / 6 *
                   dnorm(matrix(x, 3, length(x), TRUE),
                          mean = c(-10, 0, 10), sd = c(7, 5, 2))),
         add = TRUE)
legend(-25, 0.07, "True density", lty = 1, bty = 'n')


##### Gibbs sampling exemple

  ## Variâncias desconhecidas
  ## Modelo por vezes referenciado como: "d-inversed gamma model"

  ## Gasolina UK
  ## trend + seasonal + noise model
  ## Sugestão: conferir o estado não observado no arqv
  ## Rodo o sampler com base nas condicionais

outGibbs <- dlmGibbsDIG(lGas, dlmModPoly(2) + dlmModSeas(4),
                         a.y = 1, b.y = 1000, a.theta = 1,
                         b.theta = 1000,
                         n.sample = 1100, ind = c(2, 3),
                         save.states = FALSE)

burn <- 100
attach(outGibbs)
dV <- dV[-(1:burn)]
dW <- dW[-(1:burn), ]
detach()
par(mfrow=c(2,3), mar=c(3.1,2.1,2.1,1.1))
plot(dV, type = 'l', xlab = "", ylab = "",
        main = expression(sigma^2))
plot(dW[ , 1], type = 'l', xlab = "", ylab = "",
      main = expression(sigma[beta]^2))
plot(dW[ , 2], type = 'l', xlab = "", ylab = "",
        main = expression(sigma[s]^2))
use <- length(dV) - burn
from <- 0.05 * use
at <- pretty(c(0, use), n = 3); at <- at[at >= from]
plot(ergMean(dV, from), type = 'l', xaxt = 'n',
      xlab = "", ylab = "")
axis(1, at = at - from, labels = format(at))
plot(ergMean(dW[ , 1], from), type = 'l', xaxt = 'n',
xlab = "", ylab = "")
axis(1, at = at - from, labels = format(at))
plot(ergMean(dW[ , 2], from), type = 'l', xaxt = 'n',
    xlab = "", ylab = "")
axis(1, at = at - from, labels = format(at))




