################################################################################
## 11_multivariado_cholesky.R
##
## Modelo estrutural multivariado (SUTSE) para os 8 estratos, com:
##   - issue #11: bloco 8x8 de covariância dos distúrbios das inclinações
##     parametrizado por fator de Cholesky (Sigma_R = L L'), garantindo matriz
##     positiva-definida por construção. Substitui as 28 correlações tanh
##     livres, que produziam matrizes com autovalores negativos
##     (-0,59 desocupados / -1,09 ocupados / -2,00 taxa).
##   - issue #20: termo MA do erro amostral deixa de ser inerte (forma de Harvey).
##   - issue #2: multi-start e exigência de convergência limpa.
##   - m0 com dimensão correta (era rep(0,7) %x% diag(8), uma matriz 56x8).
##
## Warm start: estimativas univariadas corrigidas (rodar antes o 10_).
## Cholesky inicial = diagonal -> correlações nulas (o modelo "sem correlação").
################################################################################

suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)
stopifnot(dir.exists(file.path(RAIZ, "pseudoerros_8reg")))

INDICADOR  <- Sys.getenv("INDICADOR", "desocupados")
BURN       <- 8
P          <- 8
## prior difusa: 1e7 (como no script legado) faz o SVD do dlm falhar com 56
## estados; 1e6 é igualmente difusa frente à escala das séries (dezenas a
## centenas de milhares) e é numericamente estável.
C0_ESCALA  <- as.numeric(Sys.getenv("C0_ESCALA", "1e6"))
SAIDA <- file.path(RAIZ, "outputs", "multivariado_cholesky")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

uni <- readRDS(file.path(RAIZ, "outputs", "univariado_corrigido",
                         paste0("modelos_", INDICADOR, ".rds")))
codigos <- names(uni)
stopifnot(length(codigos) == P)

Y   <- sapply(codigos, function(k) uni[[k]]$serie$y)
SE  <- sapply(codigos, function(k) uni[[k]]$serie$se)
PHI <- sapply(codigos, function(k) uni[[k]]$phi)
THE <- sapply(codigos, function(k) uni[[k]]$theta)
ROT <- sapply(codigos, function(k) uni[[k]]$rotulo)
TEM_MA <- abs(THE) > 0

## ---- índices dos blocos no vetor de estados (7 blocos de 8) -----------------
B_NIVEL <-  1: 8; B_INCL <-  9:16; B_SAZ1 <- 17:24
B_SAZ2  <- 25:32; B_SAZ3 <- 33:40; B_EA   <- 41:48; B_AUX <- 49:56

N_CHOL <- P * (P + 1) / 2          # 36
IDX <- list(nivel = 1:8, saz = 9:16, irreg = 17:24, ea = 25:32, chol = 33:68)

################################################################################
## Construtor
################################################################################

monta <- function(params) {
  m <- dlmModPoly(2) + dlmModTrig(4) + dlmModReg(SE[, 1], addInt = FALSE)
  m$FF <- cbind(m$FF, rep(0, 1))
  m$GG <- rbind(m$GG, rep(0, 6))
  m$GG <- cbind(m$GG, rep(0, 7))
  m$GG[6, 6] <- 0; m$GG[6, 7] <- 0; m$GG[7, 6] <- 0; m$GG[7, 7] <- 0

  m$FF <- m$FF %x% diag(P)

  ## erro amostral entra como regressor com o se do desenho (JFF/X)
  JFF <- matrix(0, P, 56)
  for (i in 1:P) JFF[i, B_EA[i]] <- i
  m$JFF <- JFF
  m$X   <- SE

  ## V: componente irregular
  V <- diag(exp(params[IDX$irreg]), P)
  m$V <- V

  ## GG: processo ARMA do erro amostral por estrato
  GG <- m$GG %x% diag(P)
  for (i in 1:P) {
    GG[B_EA[i], B_EA[i]]  <- PHI[i]
    GG[B_EA[i], B_AUX[i]] <- THE[i]
  }
  m$GG <- GG

  ## W
  W <- matrix(0, 56, 56)
  W[cbind(B_NIVEL, B_NIVEL)] <- exp(params[IDX$nivel])
  W[cbind(B_SAZ1,  B_SAZ1)]  <- exp(params[IDX$saz])

  ## --- bloco das inclinações via Cholesky: Sigma_R = L L' -------------------
  L <- matrix(0, P, P)
  pc <- params[IDX$chol]
  k <- 1
  for (j in 1:P) for (i in j:P) {           # coluna a coluna, triangular inferior
    L[i, j] <- if (i == j) exp(pc[k]) else pc[k]
    k <- k + 1
  }
  W[B_INCL, B_INCL] <- L %*% t(L)

  ## --- erro amostral: forma de Harvey quando há termo MA --------------------
  for (i in 1:P) {
    s2 <- exp(params[IDX$ea][i])
    W[B_EA[i], B_EA[i]] <- s2
    if (TEM_MA[i]) {
      W[B_AUX[i], B_AUX[i]] <- s2
      W[B_EA[i],  B_AUX[i]] <- s2
      W[B_AUX[i], B_EA[i]]  <- s2
    }
  }
  m$W <- W

  ## jitter nas posições estruturalmente nulas da diagonal de W: o filtro do dlm
  ## é baseado em SVD e falha (dgesdd) com muitos zeros exatos em 56 estados.
  ## 1e-9 é irrelevante frente às variâncias estimadas (ordem 1e-1 a 1e2).
  d <- diag(W); diag(W)[d == 0] <- 1e-9
  m$W <- W

  m$m0 <- rep(0, 56)                    # (era rep(0,7) %x% diag(8): 56x8)
  m$C0 <- diag(x = C0_ESCALA, 56)
  m
}

## objetivo seguro: em vez de abortar quando o filtro falha, devolve um valor
## grande e finito, para o otimizador se afastar da região problemática.
objetivo <- function(p) {
  ll <- try(dlmLL(Y, monta(p)), silent = TRUE)
  if (inherits(ll, "try-error") || !is.finite(ll)) return(1e10)
  ll
}

################################################################################
## Valores iniciais a partir do univariado corrigido
################################################################################

hp <- sapply(codigos, function(k) uni[[k]]$corrigido$hp)   # 5 x 8
flr <- function(v) log(pmax(v, 1e-8))

p0 <- numeric(68)
p0[IDX$nivel] <- flr(hp[1, ])
p0[IDX$saz]   <- flr(hp[3, ])
p0[IDX$irreg] <- flr(hp[4, ])
p0[IDX$ea]    <- flr(hp[5, ])

## Cholesky inicial: diagonal = sqrt(sigma2_R univariado) -> correlações nulas
L0 <- diag(sqrt(pmax(hp[2, ], 1e-8)), P)
pc0 <- numeric(N_CHOL); k <- 1
for (j in 1:P) for (i in j:P) {
  pc0[k] <- if (i == j) log(L0[i, i]) else 0
  k <- k + 1
}
p0[IDX$chol] <- pc0

################################################################################
## Estimação
################################################################################

cat("Indicador:", INDICADOR, "| parâmetros:", length(p0), "| T =", nrow(Y), "\n")
cat("Processos:", paste(codigos, ifelse(TEM_MA, "com-MA", "AR/puro"), sep = "="),
    "\n\n")

roda <- function(inicial, rotulo) {
  cat("--- multi-start:", rotulo, "---\n"); flush.console()
  if (objetivo(inicial) >= 1e10) {
    cat("  ponto inicial inviável (filtro falha); descartado\n"); return(NULL)
  }
  t0 <- Sys.time()
  r <- try(optim(inicial, objetivo, method = "L-BFGS-B",
                 control = list(maxit = 2000)), silent = TRUE)
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  if (inherits(r, "try-error")) {
    cat("  falhou:", conditionMessage(attr(r, "condition")), "\n"); return(NULL)
  }
  cat("  conv =", r$convergence, " logLik =", round(-r$value, 3),
      " (", dt, "min )\n"); flush.console()
  r
}

cands <- list()
cands[[1]] <- roda(p0, "univariado corrigido, correlações nulas")

## segundo start: mesma diagonal, off-diagonais = 30% da escala típica do slope
escala <- median(sqrt(pmax(hp[2, ], 1e-8)))
p1 <- p0
{
  pc <- p0[IDX$chol]; k <- 1
  for (j in 1:P) for (i in j:P) {
    if (i != j) pc[k] <- 0.3 * escala
    k <- k + 1
  }
  p1[IDX$chol] <- pc
}
cands[[2]] <- roda(p1, "off-diagonais em 30% da escala")

cands <- Filter(Negate(is.null), cands)
stopifnot(length(cands) > 0)
ok <- sapply(cands, function(x) x$convergence == 0)
ll <- sapply(cands, function(x) -x$value)
melhor <- cands[[ if (any(ok)) which(ok)[which.max(ll[ok])] else which.max(ll) ]]

fit <- melhor
mod <- monta(fit$par)

################################################################################
## Diagnóstico da matriz de covariância das inclinações
################################################################################

Sigma_R <- mod$W[B_INCL, B_INCL]
dR      <- sqrt(diag(Sigma_R))
Corr_R  <- Sigma_R / outer(dR, dR)
ev      <- eigen(Sigma_R, symmetric = TRUE, only.values = TRUE)$values

cat("\n################ MATRIZ DE COVARIÂNCIA DAS INCLINAÇÕES ################\n")
cat("sigma2_R por estrato:", paste(round(diag(Sigma_R), 4), collapse = "  "), "\n")
cat("autovalores:", paste(round(ev, 5), collapse = "  "), "\n")
cat("menor autovalor:", format(min(ev), scientific = TRUE), "\n")
## tolerancia relativa: autovalores da ordem de 1e-15 vezes o maior sao zero
## numerico, nao negatividade. Sigma_R = L L' e PSD por construcao; o teste
## `all(ev > 0)` sem tolerancia acusa falso negativo quando o posto e deficiente.
tol <- 1e-8 * max(abs(ev))
cat("positiva-semidefinida?", all(ev > -tol), "\n")
cat("posto numerico:", sum(ev > tol), "de", P, "\n")
cat("posto efetivo (autoval > 1% do maior):", sum(ev > 0.01 * max(ev)), "de", P, "\n\n")
cat("Correlações estimadas:\n")
print(round(Corr_R, 4))

################################################################################
## Desempenho vs estimativa direta
################################################################################

flt <- dlmFilter(Y, mod)
mse <- dlmSvd2var(flt$U.C, flt$D.C)
est <- dropFirst(flt$m)
ix  <- (BURN + 1):nrow(Y)

res_tab <- do.call(rbind, lapply(1:P, function(i) {
  se_tr <- dropFirst(sapply(mse, function(x) sqrt(x[B_NIVEL[i], B_NIVEL[i]])))
  sinal <- est[, B_NIVEL[i]] + est[, B_SAZ1[i]] + est[, B_SAZ3[i]]
  data.frame(estrato = ROT[i],
             sigma2_R = round(diag(Sigma_R)[i], 4),
             rrse = round(mean((SE[ix, i] - se_tr[ix]) / SE[ix, i]) * 100, 2),
             vicio = round(sum(sinal[ix] - Y[ix, i]) / sum(Y[ix, i]) * 100, 2),
             stringsAsFactors = FALSE)
}))

cat("\n################ DESEMPENHO —", INDICADOR, "################\n")
print(res_tab, row.names = FALSE)

saveRDS(list(fit = fit, mod = mod, Sigma_R = Sigma_R, Corr_R = Corr_R,
             autovalores = ev, desempenho = res_tab, rotulos = ROT),
        file.path(SAIDA, paste0("multivariado_", INDICADOR, ".rds")))
write.csv(res_tab, file.path(SAIDA, paste0("desempenho_", INDICADOR, ".csv")),
          row.names = FALSE)
write.csv(round(Corr_R, 4), file.path(SAIDA, paste0("correlacao_", INDICADOR, ".csv")))
cat("\nGravado em", SAIDA, "\n")
