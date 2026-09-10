################################################################################
## 23_multivariado_final.R   — PASSO 4 do pipeline de revisão
##
## Modelo multivariado (SUTSE) para os 8 estratos, com:
##   - a ESPECIFICAÇÃO FINAL do erro amostral, INDIVIDUAL por estrato (passo 2),
##     o que exige blocos de estado de tamanhos diferentes — AR(1), AR(2),
##     AR(3), MA(1) e MA(4) convivem no mesmo modelo;
##   - Var(ẽ) = 1 por estrato, com a inovação derivada por Lyapunov;
##   - covariância dos distúrbios das inclinações por fator de Cholesky
##     (Σ_R = L L'), positiva-semidefinida por construção (issue #11).
##
## LAYOUT DOS ESTADOS
##   1– 8  nível          9–16  inclinação      17–24  trig cos H1
##  25–32  trig sin H1   33–40  trig H2         41+    blocos ARMA por estrato,
##                                                     concatenados, r_i = max(p_i, q_i+1)
##
## PARÂMETROS (60): 8 nível + 8 sazonal + 8 irregular + 36 Cholesky.
## A variância da inovação do erro amostral NÃO é estimada — vem da restrição.
################################################################################

suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)
source(file.path(RAIZ, "rotinas", "_id_funcoes.R"))

INDICADOR <- Sys.getenv("INDICADOR", "desocupados")
BURN  <- 8
P     <- 8
MAXIT <- as.integer(Sys.getenv("MAXIT", "600"))
SAIDA <- file.path(RAIZ, "outputs", "multivariado_final")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

uni <- readRDS(file.path(RAIZ, "outputs", "univariado_final",
                         paste0("modelos_", INDICADOR, ".rds")))
COD <- names(uni)

Y   <- sapply(COD, function(k) uni[[k]]$serie$y)
SE  <- sapply(COD, function(k) uni[[k]]$serie$se)
ROT <- sapply(COD, function(k) uni[[k]]$rotulo)
PRC <- sapply(COD, function(k) uni[[k]]$processo)
BL  <- lapply(COD, function(k) blocos(uni[[k]]$phi, uni[[k]]$theta))
R_I <- sapply(BL, function(b) b$r)
S2  <- sapply(BL, function(b) 1 / P_est(b$G, b$v, 1)[1,1])   # Var(ẽ)=1

INI  <- 40 + c(0, cumsum(R_I)[-P])      # posição inicial do bloco ARMA de cada estrato
NS   <- 40 + sum(R_I)
N_CH <- P*(P+1)/2
IDX  <- list(nivel = 1:8, saz = 9:16, irreg = 17:24, chol = 25:60)
B_NIVEL <- 1:8; B_INCL <- 9:16; B_SAZ1 <- 17:24; B_SAZ2 <- 25:32; B_SAZ3 <- 33:40

cat("Indicador:", INDICADOR, "| estados:", NS, "| parâmetros: 60\n")
cat("processos:", paste(COD, PRC, sep="="), "\n")
cat("r_i:", R_I, "| inovações derivadas:", round(S2, 4), "\n\n")

################################################################################
## estrutura fixa (não depende dos parâmetros): FF, JFF, X e GG
################################################################################
mb <- dlmModPoly(2) + dlmModTrig(4)          # 5 estados por série
GG_str <- mb$GG %x% diag(P)                  # 40 x 40

FF <- matrix(0, P, NS)
for (i in 1:P) {
  FF[i, i]        <- 1                       # nível
  FF[i, 16 + i]   <- 1                       # trig cos H1
  FF[i, 32 + i]   <- 1                       # trig H2
  FF[i, INI[i]+1] <- 1                       # erro amostral (escalado por X)
}
JFF <- matrix(0, P, NS)
for (i in 1:P) JFF[i, INI[i]+1] <- i         # coluna i de X multiplica esse estado

GG <- matrix(0, NS, NS)
GG[1:40, 1:40] <- GG_str
for (i in 1:P) {
  ix <- (INI[i]+1):(INI[i]+R_I[i])
  GG[ix, ix] <- BL[[i]]$G
}

################################################################################
monta_mv <- function(pp) {
  m <- dlm(FF = FF, V = diag(exp(pp[IDX$irreg]), P), GG = GG,
           W = diag(1e-10, NS), m0 = rep(0, NS), C0 = diag(1e6, NS),
           JFF = JFF, X = SE)
  W <- matrix(0, NS, NS)
  W[cbind(B_NIVEL, B_NIVEL)] <- exp(pp[IDX$nivel])
  ## distúrbio sazonal nos TRÊS estados trigonométricos de cada estrato
  ## (issue #8): cos H1 (17-24), sen H1 (25-32) e Nyquist H2 (33-40)
  for (bl in list(B_SAZ1, B_SAZ2, B_SAZ3)) W[cbind(bl, bl)] <- exp(pp[IDX$saz])

  ## bloco 8x8 das inclinações por Cholesky
  L <- matrix(0, P, P); pc <- pp[IDX$chol]; k <- 1
  for (j in 1:P) for (i in j:P) { L[i,j] <- if (i==j) exp(pc[k]) else pc[k]; k <- k+1 }
  W[B_INCL, B_INCL] <- L %*% t(L)

  ## blocos do erro amostral: W_i = s2_i * v_i v_i' (forma de Harvey)
  for (i in 1:P) {
    ix <- (INI[i]+1):(INI[i]+R_I[i])
    W[ix, ix] <- S2[i] * (BL[[i]]$v %*% t(BL[[i]]$v))
  }
  d <- diag(W); diag(W)[d == 0] <- 1e-10
  m$W <- W
  m
}

objetivo <- function(pp) {
  ll <- try(dlmLL(Y, monta_mv(pp)), silent = TRUE)
  if (inherits(ll, "try-error") || !is.finite(ll)) return(1e10)
  ll
}

################################################################################
## valores iniciais a partir dos univariados finais
################################################################################
hp  <- sapply(COD, function(k) uni[[k]]$corrigido$hp)   # 5 x 8
flr <- function(v) log(pmax(v, 1e-8))
p0 <- numeric(60)
p0[IDX$nivel] <- flr(hp[1, ])
p0[IDX$saz]   <- flr(hp[3, ])
p0[IDX$irreg] <- flr(hp[4, ])
esc <- median(sqrt(pmax(hp[2, ], 1e-8)))
pc0 <- numeric(N_CH); k <- 1
for (j in 1:P) for (i in j:P) {
  pc0[k] <- if (i==j) log(sqrt(max(hp[2,i], 1e-8))) else 0.3*esc
  k <- k+1
}
p0[IDX$chol] <- pc0

## PARTIDA QUENTE: retoma da solução já gravada. Serve quando a rodada anterior
## terminou por limite de iterações (conv = 1) -- converge rápido porque já está
## perto do ótimo, ao contrário da partida fria.
if (Sys.getenv("QUENTE", "0") == "1") {
  arq <- file.path(SAIDA, paste0("multivariado_", INDICADOR, ".rds"))
  stopifnot(file.exists(arq))
  ant <- readRDS(arq)
  p0  <- ant$fit$par
  cat("partida quente: logLik anterior =", round(-ant$fit$value, 3),
      "| conv anterior =", ant$fit$convergence, "\n")
}

LIM <- 20
p0  <- pmin(pmax(p0, -LIM+1), LIM-1)
cat("otimizando...\n"); flush.console()
t0 <- Sys.time()
fit <- optim(p0, objetivo, method = "L-BFGS-B",
             lower = rep(-LIM, 60), upper = rep(LIM, 60),
             control = list(maxit = MAXIT))
cat("conv =", fit$convergence, "| logLik =", round(-fit$value, 3),
    "|", round(as.numeric(difftime(Sys.time(), t0, units="mins")), 1), "min\n")

## VERIFICACAO DO OTIMO (issue #2). A Hessiana de 60 parametros custaria alguns
## milhares de avaliacoes da verossimilhanca -- e, como se verificou no
## univariado, diferencas finitas devolvem curvatura espuria nas direcoes
## planas. Usa-se um teste de perturbacao coordenada, que responde a pergunta
## pratica: nenhuma perturbacao pequena pode melhorar a funcao objetivo.
## Dois cuidados no criterio, ambos aprendidos por medicao:
##
## 1. TOLERANCIA. Contar como "melhora" qualquer reducao acima de 1e-8 e
##    apertado demais para a precisao com que dlmLL avalia a verossimilhanca.
##    Com esse limiar o teste nunca converge: numa rodada da taxa, um reinicio
##    custou 56 min para ganhar 0,113 de log-verossimilhanca -- 17 vezes abaixo
##    do limiar de 1,92 de um teste a 5% -- e o contador ainda SUBIU, de 100
##    para 103. Usa-se tol = 0,01, conservador (190 vezes menor que 1,92) mas
##    acima do ruido numerico.
##
## 2. PASSO. Deslocamentos de 0,5 nao testam minimalidade LOCAL; sondam uma
##    vizinhanca larga e, em direcoes planas, quase sempre encontram algo
##    ligeiramente menor. Ficam so 0,01 e 0,1.
## Alem de contar, a funcao DEVOLVE o melhor ponto encontrado. Isso e essencial:
## reiniciar o L-BFGS-B a partir do MESMO ponto reproduz a mesma trajetoria e nao
## sai do lugar -- observado na pratica, com logLik e contagem identicas apos o
## reinicio. O reinicio so avanca se partir do ponto perturbado que ja e melhor.
perturba <- function(par, hs = c(0.01, 0.1), tol = 0.01) {
  base <- objetivo(par); mel <- 0; n <- 0
  melhor_v <- base; melhor_p <- NULL
  for (j in seq_along(par)) for (s in c(-hs, hs)) {
    p <- par; p[j] <- p[j] + s
    v <- objetivo(p); n <- n + 1
    if (!is.finite(v)) next
    if (v < base - tol) mel <- mel + 1
    if (v < melhor_v) { melhor_v <- v; melhor_p <- p }
  }
  list(melhora = mel, total = n, ganho = base - melhor_v, par = melhor_p)
}

## Se o teste falhar, o ponto NAO e minimo local -- tipicamente porque o
## L-BFGS-B abortou na busca linear (codigo 52). Reotimiza-se entao a partir do
## proprio ponto, que costuma bastar. O limite de 3 reinicios evita loop.
MAX_REINICIO <- 5
for (tent in 0:MAX_REINICIO) {
  pt <- perturba(fit$par)
  if (pt$melhora == 0) {
    cat("teste de perturbacao: 0 de ", pt$total,
        " perturbacoes melhoram (otimo local OK)\n", sep = "")
    break
  }
  if (tent == MAX_REINICIO || is.null(pt$par)) {
    cat("teste de perturbacao: ", pt$melhora, " de ", pt$total,
        " melhoram (ganho maximo ", format(pt$ganho, digits = 4),
        ")  <<< ATENCAO: nao e minimo local apos ", tent, " reinicios\n", sep = "")
    break
  }
  cat("teste de perturbacao: ", pt$melhora, " de ", pt$total,
      " melhoram (ganho maximo ", format(pt$ganho, digits = 4),
      ") -- reinicio ", tent + 1, " a partir do ponto perturbado\n", sep = "")
  flush.console()
  t1 <- Sys.time()
  fit <- optim(pt$par, objetivo, method = "L-BFGS-B",
               lower = rep(-LIM, 60), upper = rep(LIM, 60),
               control = list(maxit = MAXIT))
  cat("  conv =", fit$convergence, "| logLik =", round(-fit$value, 3),
      "|", round(as.numeric(difftime(Sys.time(), t1, units = "mins")), 1), "min\n")
}
cat("\n")

################################################################################
mod <- monta_mv(fit$par)
Sigma_R <- mod$W[B_INCL, B_INCL]
dR <- sqrt(diag(Sigma_R)); Corr_R <- Sigma_R / outer(dR, dR)
ev <- eigen(Sigma_R, symmetric = TRUE, only.values = TRUE)$values
tol <- 1e-8 * max(abs(ev))

cat("############ COVARIÂNCIA DAS INCLINAÇÕES ############\n")
cat("sigma2_R:", paste(round(diag(Sigma_R), 4), collapse = "  "), "\n")
cat("autovalores:", paste(format(ev, digits = 4), collapse = "  "), "\n")
cat("positiva-semidefinida?", all(ev > -tol), "| posto numérico:", sum(ev > tol), "de", P, "\n\n")
print(round(Corr_R, 4))

flt <- dlmFilter(Y, mod)
mse <- dlmSvd2var(flt$U.C, flt$D.C)
est <- dropFirst(flt$m)
ix  <- (BURN+1):nrow(Y)

des <- do.call(rbind, lapply(1:P, function(i) {
  se_tr <- dropFirst(sapply(mse, function(x) sqrt(x[B_NIVEL[i], B_NIVEL[i]])))
  sinal <- est[, B_NIVEL[i]] + est[, B_SAZ1[i]] + est[, B_SAZ3[i]]
  data.frame(estrato = ROT[i], processo = PRC[i], sigma2_R = round(diag(Sigma_R)[i], 4),
             rrse = round(mean((SE[ix,i] - se_tr[ix])/SE[ix,i])*100, 2),
             vicio = round(sum(sinal[ix] - Y[ix,i])/sum(Y[ix,i])*100, 2),
             stringsAsFactors = FALSE)
}))
cat("\n############ DESEMPENHO —", INDICADOR, "############\n")
print(des, row.names = FALSE)
cat("\nganho médio:", round(mean(des$rrse), 2), "% | univariado:",
    round(mean(sapply(COD, function(k) uni[[k]]$rrse)), 2), "%\n")

saveRDS(list(fit = fit, mod = mod, Sigma_R = Sigma_R, Corr_R = Corr_R,
             autovalores = ev, desempenho = des, rotulos = ROT, processos = PRC,
             perturbacao = pt, INI = INI, R_I = R_I),
        file.path(SAIDA, paste0("multivariado_", INDICADOR, ".rds")))
write.csv(des, file.path(SAIDA, paste0("desempenho_", INDICADOR, ".csv")),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(round(Corr_R, 4), file.path(SAIDA, paste0("correlacao_", INDICADOR, ".csv")),
          fileEncoding = "UTF-8")
cat("\nGravado em", SAIDA, "\n")
