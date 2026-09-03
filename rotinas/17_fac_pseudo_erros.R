################################################################################
## 17_fac_pseudo_erros.R   — PASSO 0 e 1 do pipeline de revisão
##
## Produz a FAC e a FACP do erro amostral, por estrato e indicador, a partir dos
## pseudo-erros, usando o ESTIMADOR PADRÃO da autocovariância.
##
## POR QUE UM ESTIMADOR DIFERENTE DO ATUAL
## O `Pcov2` de funcoes/01 usa divisor (T-h) e recalcula a média em cada sublag.
## Conferindo as fontes primárias:
##   - Silva e Cruz (2002), p. 70: o divisor (T-h) aparece na §5.2.1, que trata
##     do caso em que o analista tem os MICRODADOS LIGADOS e calcula covariâncias
##     baseadas no desenho, uma por par de tempos. Ali (T-h) é a contagem de
##     termos da média, não divisor de autocovariância de série temporal.
##   - O método dos pseudo-erros é a §5.2.2 (p. 71, eq. 5.4-5.6), e o que ela
##     pede é a função de autocovariância AMOSTRAL das séries de pseudo-erros.
##   - Pfeffermann, Feder e Signorelli (1998), p. 340: as médias subtraídas são
##     UMA POR PAINEL, e existem para remover o viés de grupo de rotação -- não
##     duas médias por janela temporal.
## Logo: média única por painel, divisor T. É o estimador consistente e padrão,
## e o único que garante sequência positiva-semidefinida por construção.
##
## Agregação entre painéis (Silva e Cruz, eq. 5.6; Rosseti e Silva, eq. 21):
##   rho_h = soma_k c_h^(k) / soma_k c_0^(k)
## A constante K(K-1) cancela na razão.
################################################################################

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)

SAIDA <- file.path(RAIZ, "outputs", "fac_pseudo_erros")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

K    <- 5     # grupos de rotação da PNADc
LAGS <- 12    # FAC até o lag 12 (o desenho só sustenta até 4; o resto diagnostica)

## base alinhada por grupo de rotação, com os três indicadores
base <- readRDS(file.path(RAIZ, "dadosalin_txdesoc_8reg.rds"))
REG  <- names(base)[1:8]

IND <- c(desocupados = "desocupada", ocupados = "ocupada", taxa = "txdesoc")

################################################################################
## Estimadores
################################################################################

## PADRÃO: média única da série, divisor T. Garante sequência PSD.
autocov <- function(v, lag) {
  T <- length(v); m <- mean(v)
  sapply(0:(lag), function(h) sum((v[1:(T-h)] - m) * (v[(1+h):T] - m)) / T)
}

## LEGADO (só para comparação): divisor (T-h) e média por janela.
autocov_legado <- function(v, lag) {
  T <- length(v)
  sapply(0:(lag), function(h)
    sum((v[1:(T-h)] - mean(v[1:(T-h)])) * (v[(1+h):T] - mean(v[(1+h):T]))) / (T-h))
}

## FACP a partir da FAC, por razão de determinantes (Durbin-Levinson equivalente)
facp_de_fac <- function(fac, lag) {
  p <- numeric(lag)
  for (i in 1:lag) {
    if (i == 1) { p[i] <- fac[2]; next }
    D <- toeplitz(fac[1:i])
    N <- D; N[, i] <- fac[2:(i+1)]
    p[i] <- det(N) / det(D)
  }
  p
}

################################################################################
## Cálculo
################################################################################

linhas <- list(); facs <- list()
for (ind_nome in names(IND)) {
  col <- IND[[ind_nome]]
  for (r in seq_along(REG)) {
    d <- base[[ REG[r] ]]
    grupos <- paste0(col, "_", 1:K)
    if (!all(grupos %in% names(d))) { cat("faltam colunas de", col, "em", REG[r], "\n"); next }

    G <- as.matrix(d[, grupos])
    G[G == 0] <- NA                      # zero = ausência de informação
    media <- rowMeans(G, na.rm = TRUE)
    P <- sweep(G, 1, media, "-")         # pseudo-erros por grupo

    for (versao in c("padrao", "legado")) {
      f <- if (versao == "padrao") autocov else autocov_legado
      soma <- rowSums(sapply(1:K, function(k) f(P[, k], LAGS)))
      fac  <- soma / soma[1]
      facp <- facp_de_fac(fac, LAGS)
      linhas[[length(linhas)+1]] <- data.frame(
        indicador = ind_nome, estrato = REG[r], versao = versao,
        lag = 1:LAGS, fac = round(fac[-1], 4), facp = round(facp, 4),
        stringsAsFactors = FALSE)
      if (versao == "padrao")
        facs[[paste(ind_nome, REG[r], sep = "|")]] <- fac
    }
  }
}

tab <- do.call(rbind, linhas)
saveRDS(facs, file.path(SAIDA, "fac_padrao.rds"))
write.csv(tab, file.path(SAIDA, "fac_facp.csv"), row.names = FALSE)

################################################################################
## Relatório
################################################################################

lim <- 1.96 / sqrt(nrow(base[[1]]))
cat("limite aproximado de significancia (T =", nrow(base[[1]]), "):", round(lim, 3), "\n")

for (ind_nome in names(IND)) {
  cat("\n\n################", toupper(ind_nome), "— FAC (estimador padrao) ################\n")
  cat(sprintf("%-40s", "estrato"))
  for (l in 1:8) cat(sprintf("%8s", paste0("lag", l)))
  cat("   lags signif.\n")
  M <- NULL
  for (r in REG) {
    x <- tab[tab$indicador == ind_nome & tab$estrato == r & tab$versao == "padrao", ]
    if (!nrow(x)) next
    v <- x$fac[1:8]; M <- rbind(M, v)
    cat(sprintf("%-40s", substr(r, 1, 38)))
    for (u in v) cat(sprintf("%8.3f", u))
    sig <- which(abs(v) > lim)
    cat("   ", if (length(sig)) paste(sig, collapse = ",") else "nenhum", "\n")
  }
  cat(sprintf("%-40s", "MEDIA"))
  for (u in colMeans(M)) cat(sprintf("%8.3f", u))
  cat("\n")

  ## efeito da troca de estimador
  a <- tab[tab$indicador == ind_nome & tab$versao == "padrao", ]
  b <- tab[tab$indicador == ind_nome & tab$versao == "legado", ]
  m <- merge(a, b, by = c("estrato","lag"), suffixes = c("_pad","_leg"))
  cat("\nefeito da troca de estimador na FAC: dif. media abs =",
      round(mean(abs(m$fac_pad - m$fac_leg)), 4),
      "| max =", round(max(abs(m$fac_pad - m$fac_leg)), 4), "\n")
  m1 <- m[m$lag == 1, ]
  cat("no lag 1 (que determina MA(1)/AR(1)): dif. media abs =",
      round(mean(abs(m1$fac_pad - m1$fac_leg)), 4),
      "| max =", round(max(abs(m1$fac_pad - m1$fac_leg)), 4), "\n")
}

cat("\n\nGravado em", SAIDA, "\n")
