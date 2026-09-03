################################################################################
## VERIFICAÇÃO — positividade-definida da matriz de covariância do erro amostral
##
## Motivação: o issue #18 alega que a `Pcov2` (divisor T-i e média recalculada em
## cada sublag, em vez do estimador usual com divisor T e média única) pode gerar
## uma sequência de autocovariâncias NÃO positiva-semidefinida — o que tornaria a
## componente de erro amostral do modelo em espaço de estados mal definida.
##
## Este script testa a alegação nos dados reais, em duas dimensões:
##   (a) matriz 25x25, usando só os lags efetivamente estimados (0..24);
##   (b) matriz TxT (T = 52 trimestres), que é a que de fato entra no modelo —
##       aqui os lags > 24 são TRUNCADOS em zero, e é esse truncamento que
##       tipicamente destrói a positividade, independentemente do estimador.
##
## Rodar da raiz do repo:
##   Rscript docs/verificacoes/test_pd_autocov.R
################################################################################
options(scipen = 999)

arq_base <- "basealinhada_8reg.rds"   # base commitada (vintage do artigo)
LAGS <- 24
K    <- 5

autocov_pseudo <- function(v, lag, versao) {
  T <- length(v); out <- numeric(lag)
  if (versao == "original") {
    for (i in 0:(lag - 1))
      out[i + 1] <- sum((v[1:(T - i)] - mean(v[1:(T - i)])) *
                        (v[(1 + i):T] - mean(v[(1 + i):T]))) / (T - i)
  } else {
    m <- mean(v)
    for (i in 0:(lag - 1))
      out[i + 1] <- sum((v[1:(T - i)] - m) * (v[(1 + i):T] - m)) / T
  }
  out
}

## menor autovalor relativo da Toeplitz de dimensão n, com gama truncado em zero
## para defasagens além das estimadas
autoval_min_rel <- function(gama, n) {
  g <- c(gama, rep(0, max(0, n - length(gama))))
  M <- outer(1:n, 1:n, function(i, j) g[abs(i - j) + 1])
  min(eigen(M, symmetric = TRUE, only.values = TRUE)$values) / g[1]
}

base <- readRDS(arq_base)
regioes <- names(base)

linhas <- list()
for (nome in regioes) {
  db <- base[[nome]]
  Tn <- nrow(db)
  for (ind in c("ocupada", "desocupada")) {
    cols <- paste0(ind, "_", 1:K)
    media <- rowMeans(replace(db[, cols], db[, cols] == 0, NA), na.rm = TRUE)
    pseudos <- lapply(cols, function(cc) db[[cc]] - media)

    for (versao in c("original", "amostral")) {
      ch <- sapply(pseudos, autocov_pseudo, lag = LAGS + 1, versao = versao)
      gama <- rowSums(ch) / (K^2 - K)
      linhas[[length(linhas) + 1]] <- data.frame(
        regiao = substr(nome, 1, 22), indicador = ind, versao = versao,
        n25 = autoval_min_rel(gama, LAGS + 1),
        nT  = autoval_min_rel(gama, Tn))
    }
  }
}

res <- do.call(rbind, linhas)
res$pd_25 <- ifelse(res$n25 < -1e-10, "NAO-PSD", "ok")
res$pd_T  <- ifelse(res$nT  < -1e-10, "NAO-PSD", "ok")

cat("=== Menor autovalor / gama0 ===\n")
cat("n25 = matriz 25x25 (so os lags estimados)\n")
cat("nT  = matriz 52x52 (a que entra no modelo; lags > 24 truncados em zero)\n\n")
print(res, row.names = FALSE, digits = 4)

cat("\n=== RESUMO ===\n")
for (v in c("original", "amostral")) {
  s <- res[res$versao == v, ]
  cat(sprintf("%-9s : 25x25 -> %d/%d NAO-PSD | 52x52 -> %d/%d NAO-PSD\n",
              v, sum(s$pd_25 == "NAO-PSD"), nrow(s),
              sum(s$pd_T == "NAO-PSD"), nrow(s)))
}
