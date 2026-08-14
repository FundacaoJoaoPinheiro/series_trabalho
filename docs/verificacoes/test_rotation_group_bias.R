################################################################################
## VERIFICAÇÃO — rotation group bias (RGB) e sua assinatura nos pseudo-erros
##
## MOTIVAÇÃO
## A FAC do pseudo-erro dos OCUPADOS não morre no lag 5, embora a sobreposição de
## amostra da PNADc (5 entrevistas em trimestres consecutivos) seja zero a partir
## dali. Nos DESOCUPADOS ela morre, como esperado. Hipótese: há efeito sistemático
## de número de entrevista (V1016) — rotation group bias, Bailar (1975) — que os
## pseudo-erros de Silva & Cruz não contemplam.
##
## HIPÓTESE PRECISA (é o que dá poder ao teste)
## Na base ALINHADA, o `organiza_base` permuta ciclicamente os grupos: a coluna k
## no trimestre i corresponde a V1016 = ((k+i-2) mod 5)+1. Logo um efeito FIXO por
## número de entrevista é visto, ao longo do tempo, como componente PERIÓDICA DE
## PERÍODO 5 — que produz repique na FAC nos lags 5, 10, 15, em vez de decaimento.
##
## TESTES
##  A) o efeito de entrevista existe? (índice de Bailar + F de dois fatores)
##  B) a assinatura periódica aparece na FAC? (lags 1..20, repique em 5/10/15)
##  C) DECISIVO: removido o efeito de entrevista, a cauda da FAC desaparece?
##
## Rodar da raiz do repo:  Rscript docs/verificacoes/test_rotation_group_bias.R
################################################################################
options(scipen = 999, width = 150)

rot <- readRDS("baserot8reg.rds")        # coluna j = V1016 = j (numero da entrevista)
ali <- readRDS("basealinhada_8reg.rds")  # colunas permutadas ciclicamente
K <- 5

Pcov2 <- function(v, lag) {
  T <- length(v); out <- numeric(lag)
  for (i in 0:(lag - 1))
    out[i+1] <- sum((v[1:(T-i)] - mean(v[1:(T-i)])) * (v[(1+i):T] - mean(v[(1+i):T]))) / (T-i)
  out
}

## FAC do erro amostral a partir de uma matriz T×5 de estimativas por grupo
fac_pseudo <- function(M, lag) {
  media <- rowMeans(replace(M, M == 0, NA), na.rm = TRUE)
  ch <- sapply(1:K, function(k) Pcov2(M[, k] - media, lag + 1))
  soma <- rowSums(ch)
  soma[-1] / soma[1]
}

## ============================================================================
## 0. Confere a permutação do alinhamento (para a hipótese fazer sentido)
## ============================================================================
cat("=== 0. Conferencia da permutacao ciclica do alinhamento ===\n")
rg <- names(rot)[1]
Mr <- as.matrix(rot[[rg]][, paste0("ocupada_", 1:K)])
Ma <- as.matrix(ali[[rg]][, paste0("ocupada_", 1:K)])
Tn <- nrow(Mr)
prev <- sapply(1:K, function(k) sapply(1:Tn, function(i) Mr[i, ((k + i - 2) %% K) + 1]))
cat("  alinhada == permutacao ciclica da baserot? ",
    isTRUE(all.equal(unname(Ma), unname(prev), tolerance = 1e-9)), "\n")
cat("  (se TRUE, um efeito fixo de entrevista vira periodicidade 5 no pseudo-erro)\n\n")

## ============================================================================
## A. O efeito de número de entrevista existe?
## ============================================================================
cat("=== A. Indice de Bailar: media(grupo j / media do trimestre) x 100 ===\n")
cat("    100 = sem efeito. Desvio sistematico = rotation group bias.\n\n")

for (ind in c("ocupada", "desocupada")) {
  cat("---", toupper(ind), "---\n")
  tabB <- NULL; pvals <- numeric(0)
  for (rg in names(rot)) {
    M <- as.matrix(rot[[rg]][, paste0(ind, "_", 1:K)])
    idx <- 100 * colMeans(M / rowMeans(M))
    tabB <- rbind(tabB, round(idx, 2))

    # F de dois fatores em log(y): trimestre (absorve o nivel) + entrevista
    df <- data.frame(y = log(as.vector(M)),
                     tri = factor(rep(1:nrow(M), times = K)),
                     ent = factor(rep(1:K, each = nrow(M))))
    a <- anova(lm(y ~ tri + ent, data = df))
    pvals <- c(pvals, a["ent", "Pr(>F)"])
  }
  rownames(tabB) <- substr(names(rot), 1, 22)
  colnames(tabB) <- paste0("ent", 1:K)
  tabB <- cbind(tabB, amplitude = round(apply(tabB, 1, function(r) max(r) - min(r)), 2),
                p_efeito_ent = signif(pvals, 3))
  print(tabB)
  cat("  regioes com efeito de entrevista significativo (p<0,05):",
      sum(pvals < 0.05), "de", length(pvals), "\n\n")
}

## ============================================================================
## B. A assinatura periódica aparece na FAC?
## ============================================================================
cat("=== B. FAC do pseudo-erro ate o lag 20 (base ALINHADA, como no artigo) ===\n")
cat("    repique em 5/10/15 = periodicidade 5 = efeito de entrevista\n\n")
for (ind in c("ocupada", "desocupada")) {
  m <- t(sapply(names(ali), function(rg)
    fac_pseudo(as.matrix(ali[[rg]][, paste0(ind, "_", 1:K)]), 20)))
  rownames(m) <- substr(names(ali), 1, 22); colnames(m) <- paste0("l", 1:20)
  cat("---", toupper(ind), "--- (media das 9 regioes)\n")
  med <- colMeans(m)
  print(round(med, 3))
  cat("  lag5 > lag4 em", sum(m[, 5] > m[, 4]), "de 9 regioes",
      "| lag10 > lag9 em", sum(m[, 10] > m[, 9]), "de 9\n\n")
}

## ============================================================================
## C. TESTE DECISIVO — remover o efeito de entrevista e recalcular a FAC
## ============================================================================
cat("=== C. DECISIVO: FAC antes x depois de remover o efeito de entrevista ===\n")
cat("    Correcao multiplicativa: X*_j(t) = X_j(t) / b_j, com b_j o indice de\n")
cat("    Bailar do grupo j (media do proprio grupo / media geral). Depois refaz\n")
cat("    o alinhamento diagonal e recalcula a FAC do pseudo-erro.\n\n")

alinhar <- function(M) {   # mesma permutacao ciclica do organiza_base
  Tn <- nrow(M)
  sapply(1:K, function(k) sapply(1:Tn, function(i) M[i, ((k + i - 2) %% K) + 1]))
}

for (ind in c("ocupada", "desocupada")) {
  cat("---", toupper(ind), "---\n")
  antes <- depois <- NULL
  for (rg in names(rot)) {
    M <- as.matrix(rot[[rg]][, paste0(ind, "_", 1:K)])
    b <- colMeans(M / rowMeans(M))          # efeito relativo de cada entrevista
    Mc <- sweep(M, 2, b, "/")               # remove o efeito de entrevista
    antes  <- rbind(antes,  fac_pseudo(alinhar(M),  20))
    depois <- rbind(depois, fac_pseudo(alinhar(Mc), 20))
  }
  cmp <- rbind(antes = colMeans(antes), depois = colMeans(depois))
  colnames(cmp) <- paste0("l", 1:20)
  print(round(cmp[, 1:12], 3))
  cat(sprintf("  cauda media (lags 5-12): antes = %.3f | depois = %.3f  (reducao de %.0f%%)\n\n",
              mean(colMeans(antes)[5:12]), mean(colMeans(depois)[5:12]),
              100 * (1 - mean(colMeans(depois)[5:12]) / mean(colMeans(antes)[5:12]))))
}

cat("Leitura: se a cauda dos OCUPADOS cair muito e a dos DESOCUPADOS pouco\n")
cat("(ja era baixa), o rotation group bias esta confirmado como a causa.\n")
