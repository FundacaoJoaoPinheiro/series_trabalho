################################################################################
## 40_sintese_robustez.R — robustez de 10/11/2026, tarefas 4 e 5
##
## Compara as rodadas de outputs/robustez_2026-11-10/<variante>/ com a rodada C
## publicada (outputs/univariado_final e outputs/multivariado_final, só leitura):
##   DR médio   média de (c_t − EP da tendência)/c_t, t > 8 (a "rrse" do 22_/23_)
##   DR_orig    o mesmo, mas contra o c_t ORIGINAL (útil nas variantes de c_t, em
##              que o próprio c_t foi multiplicado por 0,8 ou 1,2)
##   CV médio   média de 100 · EP(tendência filtrada) / tendência, t > 8
##   posto efetivo  autovalores de Σ_R ≥ 1% do maior (mesma regra do 25_)
##   correlação mediana  mediana das 28 correlações fora da diagonal de Σ_R
## Saída: outputs/robustez_2026-11-10/sintese_rodadas.csv
################################################################################
suppressMessages(library(dlm))
RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
ROB  <- file.path(RAIZ, "outputs", "robustez_2026-11-10")
BURN <- 8; B_NIVEL <- 1:8
VAR <- list(
  C           = list(uni = file.path(RAIZ, "outputs", "univariado_final"),
                     mv  = file.path(RAIZ, "outputs", "multivariado_final"), esc = 1),
  ct_1.0      = list(uni = file.path(ROB, "ct_1.0", "univariado"), mv = NA, esc = 1),
  por_estrato = list(uni = file.path(ROB, "por_estrato", "univariado"),
                     mv  = file.path(ROB, "por_estrato", "multivariado"), esc = 1),
  ct_0.8      = list(uni = file.path(ROB, "ct_0.8", "univariado"),
                     mv  = file.path(ROB, "ct_0.8", "multivariado"), esc = 0.8),
  ct_1.2      = list(uni = file.path(ROB, "ct_1.2", "univariado"),
                     mv  = file.path(ROB, "ct_1.2", "multivariado"), esc = 1.2))

linhas <- list()
for (v in names(VAR)) for (ind in c("desocupados", "ocupados", "taxa")) {
  fu <- file.path(VAR[[v]]$uni, paste0("modelos_", ind, ".rds"))
  if (!file.exists(fu)) next
  uni <- readRDS(fu); ix <- (BURN + 1):length(uni[[1]]$serie$y)
  esc <- VAR[[v]]$esc
  dr  <- sapply(uni, function(u) mean((u$serie$se[ix] - u$corrigido$se_trend[ix]) / u$serie$se[ix]) * 100)
  dro <- sapply(uni, function(u) { s0 <- u$serie$se[ix] / esc
    mean((s0 - u$corrigido$se_trend[ix]) / s0) * 100 })
  cv  <- sapply(uni, function(u) mean(100 * u$corrigido$se_trend[ix] / u$corrigido$trend[ix]))
  cvd <- sapply(uni, function(u) mean(100 * u$serie$se[ix] / esc / u$serie$y[ix]))
  ll  <- sum(sapply(uni, function(u) u$corrigido$loglik))
  linhas[[length(linhas) + 1]] <- data.frame(variante = v, indicador = ind, modelo = "univariado",
    DR = mean(dr), DR_orig = mean(dro), CV = mean(cv), CV_direta = mean(cvd),
    loglik = ll, nivel_livre = sum(!sapply(uni, function(u) isTRUE(u$smooth))),
    posto_efetivo = NA, cor_mediana = NA, conv = NA, stringsAsFactors = FALSE)

  fm <- if (is.na(VAR[[v]]$mv)) "" else file.path(VAR[[v]]$mv, paste0("multivariado_", ind, ".rds"))
  if (!file.exists(fm)) next
  m <- readRDS(fm)
  Y  <- sapply(names(uni), function(k) uni[[k]]$serie$y)
  SE <- sapply(names(uni), function(k) uni[[k]]$serie$se)
  flt <- dlmFilter(Y, m$mod); mse <- dlmSvd2var(flt$U.C, flt$D.C); est <- dropFirst(flt$m)
  setr <- sapply(B_NIVEL, function(i) dropFirst(sapply(mse, function(x) sqrt(x[i, i]))))
  tr   <- est[, B_NIVEL]
  drm  <- colMeans((SE[ix, ] - setr[ix, ]) / SE[ix, ]) * 100
  drmo <- colMeans((SE[ix, ] / esc - setr[ix, ]) / (SE[ix, ] / esc)) * 100
  ev <- m$autovalores; C <- m$Corr_R
  linhas[[length(linhas) + 1]] <- data.frame(variante = v, indicador = ind, modelo = "multivariado",
    DR = mean(drm), DR_orig = mean(drmo), CV = mean(100 * setr[ix, ] / tr[ix, ]),
    CV_direta = mean(cvd), loglik = -m$fit$value, nivel_livre = sum(!m$smooth),
    posto_efetivo = sum(ev >= 0.01 * max(ev)), cor_mediana = median(C[lower.tri(C)]),
    conv = m$fit$convergence, stringsAsFactors = FALSE)
}
tab <- do.call(rbind, linhas)
num <- sapply(tab, is.numeric); tab[num] <- lapply(tab[num], round, 2)
write.csv(tab, file.path(ROB, "sintese_rodadas.csv"), row.names = FALSE, fileEncoding = "UTF-8")
options(width = 160)
print(tab[order(tab$modelo, tab$indicador, tab$variante), ], row.names = FALSE)
