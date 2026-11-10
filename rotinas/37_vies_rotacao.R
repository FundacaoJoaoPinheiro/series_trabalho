################################################################################
## 37_vies_rotacao.R — robustez de 10/11/2026, tarefa 3
##
## O artigo supõe E(e_t) = 0 e cita o viés de grupo de rotação (Bailar 1975) só
## como limitação. Aqui se testa se a estimativa da 1ª ENTREVISTA (V1016 = 1)
## difere sistematicamente das demais (2ª a 5ª), por estrato, e se o teste tem
## poder para afirmar ausência de viés relevante.
##
## Medida, por trimestre t:  l_t = log X1_t − log( média de X2_t..X5_t )
##   X_k = estimativa do grupo de visita k (total de desocupados / ocupados, ou
##   taxa), tirada de tx_rot8reg.rds (mesmas colunas de baserot8reg.rds, por
##   V1016, sem o alinhamento diagonal). exp(média de l) − 1 é o viés médio
##   relativo da 1ª entrevista em relação às demais.
## Inferência: média de l com erro-padrão de Newey-West (defasagem 4). A
##   correção é necessária porque o grupo que é 1ª visita em t é 2ª visita em
##   t+1, ..., e a sobreposição de amostra torna l_t autocorrelacionado.
## Poder: menor viés relativo detectável a 5% (bilateral) com 80% de poder,
##   MDE = (z_0,975 + z_0,80) · EP_HAC, na escala log, devolvido em %.
## O efeito sobre a estimativa publicada (soma dos 5 grupos) é ~ 1/5 do viés do
##   grupo, SE as visitas 2 a 5 forem não viesadas -- o viés absoluto do
##   estimador não é identificável sem um padrão-ouro externo; o que se testa é
##   o viés DIFERENCIAL entre visitas.
## O total de ocupados entra como controle positivo: a verificação V2
##   (docs/verificacoes/V2_rotation_group_bias.md) já achou RGB nele.
##
## Saída: outputs/robustez_2026-11-10/vies_rotacao/
################################################################################
suppressMessages(library(sandwich))
RAIZ  <- Sys.getenv("REPO_RAIZ", unset = getwd())
SAIDA <- file.path(RAIZ, "outputs", "robustez_2026-11-10", "vies_rotacao")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

tr  <- readRDS(file.path(RAIZ, "tx_rot8reg.rds"))
REG <- names(tr)                      # 8 estratos + "09 - Minas Gerais"
## conferência: as colunas de tx_rot8reg batem com baserot8reg (vintage do artigo)
br <- readRDS(file.path(RAIZ, "baserot8reg.rds"))
stopifnot(all.equal(tr[[1]]$desocupada_1, br[[1]]$desocupada_1))
stopifnot(nrow(tr[[1]]) == 52)

IND <- c(desocupados = "desocupada_", ocupados = "ocupada_", taxa = "tx_desocup_")
Z   <- qnorm(0.975) + qnorm(0.80)     # 2,80
LAG <- 4

um <- function(l) {
  f  <- lm(l ~ 1)
  ep <- sqrt(NeweyWest(f, lag = LAG, prewhite = FALSE, adjust = TRUE)[1, 1])
  ep_iid <- sd(l) / sqrt(length(l))
  m  <- mean(l)
  c(vies = 100 * (exp(m) - 1),
    li = 100 * (exp(m - qnorm(.975) * ep) - 1), ls = 100 * (exp(m + qnorm(.975) * ep) - 1),
    p = 2 * pnorm(-abs(m / ep)),
    mde = 100 * (exp(Z * ep) - 1), mde_iid = 100 * (exp(Z * ep_iid) - 1),
    dp_l = sd(l), rho1 = acf(l, plot = FALSE, lag.max = 1)$acf[2])
}

res <- list(); series_l <- list()
for (ind in names(IND)) {
  L <- sapply(REG, function(r) {
    X <- as.matrix(tr[[r]][, paste0(IND[ind], 1:5)])
    log(X[, 1]) - log(rowMeans(X[, 2:5]))
  })
  series_l[[ind]] <- L
  for (j in seq_along(REG)) res[[length(res) + 1]] <-
    data.frame(indicador = ind, estrato = REG[j], t(um(L[, j])), check.names = FALSE)
  ## agregado dos 8 estratos (média simples de l_t entre estratos, por trimestre)
  res[[length(res) + 1]] <- data.frame(indicador = ind, estrato = "Média dos 8 estratos",
                                       t(um(rowMeans(L[, 1:8]))), check.names = FALSE)
}
tab <- do.call(rbind, res)
num <- sapply(tab, is.numeric)
tab_r <- tab; tab_r[num] <- lapply(tab[num], round, 3)
write.csv(tab_r, file.path(SAIDA, "vies_1a_entrevista.csv"), row.names = FALSE, fileEncoding = "UTF-8")

## Bailar: índice médio de cada visita (X_k / média das 5, ×100) -- descritivo
bailar <- do.call(rbind, lapply(names(IND), function(ind) do.call(rbind, lapply(REG, function(r) {
  X <- as.matrix(tr[[r]][, paste0(IND[ind], 1:5)])
  v <- round(colMeans(100 * X / rowMeans(X)), 1); names(v) <- paste0("visita", 1:5)
  data.frame(indicador = ind, estrato = r, t(v))
}))))
write.csv(bailar, file.path(SAIDA, "indice_bailar.csv"), row.names = FALSE, fileEncoding = "UTF-8")

options(width = 160)
cat("Viés relativo da 1ª entrevista vs. média das visitas 2-5 (%), IC 95% Newey-West (lag", LAG, ")\n")
cat("MDE = menor viés detectável a 5% com 80% de poder, 52 trimestres\n\n")
for (ind in names(IND)) {
  cat("####", toupper(ind), "####\n")
  print(tab_r[tab_r$indicador == ind, -1], row.names = FALSE)
  s <- tab[tab$indicador == ind & !tab$estrato %in% c("Média dos 8 estratos", "09 - Minas Gerais"), ]
  cat("rejeitam a 5%:", sum(s$p < 0.05), "de 8 | MDE mediano", round(median(s$mde), 1),
      "% (faixa", round(min(s$mde), 1), "a", round(max(s$mde), 1), "%)\n\n")
}
cat("Índice de Bailar médio (100 = sem efeito):\n")
print(bailar[bailar$estrato %in% c("09 - Minas Gerais"), ], row.names = FALSE)
