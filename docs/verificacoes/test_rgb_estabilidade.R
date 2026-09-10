################################################################################
## VERIFICAÇÃO — de onde vem a cauda longa da FAC do pseudo-erro dos ocupados
##
## O teste anterior (test_rotation_group_bias.R) mostrou:
##   - rotation group bias EXISTE nos ocupados (monotônico ent1>...>ent5,
##     amplitude ~6,5 p.p., significativo em 8/9 regiões) e NÃO existe nos
##     desocupados (0/9);
##   - mas removê-lo como efeito CONSTANTE não reduz a cauda da FAC (-5%).
##
## Logo a cauda vem de outra coisa. Duas hipóteses restantes:
##   H1: o efeito de entrevista VARIA no tempo (RGB não estacionário) — a
##       componente de baixa frequência resultante gera FAC de decaimento lento;
##   H2: o pseudo-erro carrega TENDÊNCIA da própria série (contaminação de sinal).
##
## Ambas implicam o mesmo remédio empírico: remover a baixa frequência do
## pseudo-erro deve fazer a FAC morrer no lag 5, como manda a sobreposição.
##
## Rodar da raiz do repo:  Rscript docs/verificacoes/test_rgb_estabilidade.R
################################################################################
options(scipen = 999, width = 150)

rot <- readRDS("baserot8reg.rds")
K <- 5

Pcov2 <- function(v, lag) {
  T <- length(v); out <- numeric(lag)
  for (i in 0:(lag - 1))
    out[i+1] <- sum((v[1:(T-i)] - mean(v[1:(T-i)])) * (v[(1+i):T] - mean(v[(1+i):T]))) / (T-i)
  out
}
alinhar <- function(M) {
  Tn <- nrow(M); sapply(1:K, function(k) sapply(1:Tn, function(i) M[i, ((k+i-2) %% K)+1]))
}
fac_de_pseudos <- function(P, lag) {          # P: T×5 de pseudo-erros
  ch <- sapply(1:K, function(k) Pcov2(P[, k], lag + 1))
  soma <- rowSums(ch); soma[-1] / soma[1]
}
pseudos_de <- function(M) M - rowMeans(M)     # M ja alinhada

## ============================================================================
## H1. O efeito de entrevista é estável no tempo?
## ============================================================================
cat("=== H1. Indice de Bailar dos OCUPADOS por trienio (x100) ===\n")
cat("    Se o RGB fosse constante, as linhas seriam iguais.\n\n")

periodos <- list("2012-2014" = 1:12, "2015-2017" = 13:24,
                 "2018-2020" = 25:36, "2021-2024" = 37:52)

for (rg in c("09 - Minas Gerais", "01-Belo Horizonte", "07-Vale do Rio Doce")) {
  M <- as.matrix(rot[[rg]][, paste0("ocupada_", 1:K)])
  tb <- t(sapply(periodos, function(ii) round(100 * colMeans(M[ii, ] / rowMeans(M[ii, ])), 2)))
  colnames(tb) <- paste0("ent", 1:K)
  tb <- cbind(tb, amplitude = round(apply(tb, 1, function(r) max(r) - min(r)), 2))
  cat("---", rg, "---\n"); print(tb); cat("\n")
}

## deriva do efeito: correlação do índice com o tempo, por região
cat("Deriva temporal do efeito de entrevista (todas as regioes):\n")
deriva <- t(sapply(names(rot), function(rg) {
  M <- as.matrix(rot[[rg]][, paste0("ocupada_", 1:K)])
  idx <- M / rowMeans(M)
  round(sapply(1:K, function(k) cor(idx[, k], seq_len(nrow(M)))), 3)
}))
colnames(deriva) <- paste0("ent", 1:K); rownames(deriva) <- substr(names(rot), 1, 22)
print(deriva)
cat("\n  correlacao com o tempo != 0 => o efeito de entrevista DERIVA (RGB nao estacionario)\n\n")

## ============================================================================
## H2. O pseudo-erro carrega tendência?
##
## NOTA: a primeira tentativa (detrend por loess, span 0,4, e recálculo da FAC)
## foi DESCARTADA por inconclusiva — o filtro é agressivo o bastante para comer
## também a autocorrelação legítima de curto prazo: aplicado aos DESOCUPADOS,
## que são bem-comportados, derrubava rho1 de 0,360 para 0,123 e tornava a FAC
## negativa. O que se mede ali é o filtro, não o dado. O teste abaixo é
## cirúrgico: regride cada pseudo-erro no tempo, sem filtrar nada.
## Um erro amostral legítimo tem média zero e NENHUMA tendência.
## ============================================================================
cat("=== H2. Tendencia linear no pseudo-erro (regressao no tempo) ===\n")
cat("    n_signif = grupos (de 5) com tendencia significativa a 5%\n")
cat("    r2_medio = fracao da variacao do 'erro amostral' explicada por tendencia\n\n")

for (ind in c("ocupada", "desocupada")) {
  cat("---", toupper(ind), "---\n")
  out <- NULL
  for (rg in names(rot)) {
    P <- pseudos_de(alinhar(as.matrix(rot[[rg]][, paste0(ind, "_", 1:K)])))
    tt <- seq_len(nrow(P))
    aj <- lapply(1:K, function(k) summary(lm(P[, k] ~ tt)))
    out <- rbind(out, c(n_signif = sum(sapply(aj, function(s) s$coefficients[2, 4]) < 0.05),
                        r2_medio = round(mean(sapply(aj, function(s) s$r.squared)), 3)))
  }
  rownames(out) <- substr(names(rot), 1, 22)
  print(out)
  cat("  total com tendencia significativa:", sum(out[, 1]), "de", 9 * K, "\n\n")
}

## magnitude relativa, para dimensionar a contaminação
cat("=== Magnitude do pseudo-erro (desvio-padrao medio / nivel medio, %) ===\n")
mg <- t(sapply(names(rot), function(rg)
  sapply(c("ocupada", "desocupada"), function(ind) {
    M <- alinhar(as.matrix(rot[[rg]][, paste0(ind, "_", 1:K)]))
    round(100 * mean(apply(M - rowMeans(M), 2, sd)) / mean(rowMeans(M)), 2)
  })))
rownames(mg) <- substr(names(rot), 1, 22)
print(mg)

cat("\nLeitura: tendencia no pseudo-erro dos ocupados e nao nos desocupados\n")
cat("confirma que o pseudo-erro dos ocupados nao e' erro amostral puro.\n")
