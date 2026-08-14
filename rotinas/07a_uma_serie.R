################################################################################
## 07a - Ajusta UMA série (região × indicador) e grava o resultado.
##
## Existe separado do `07_modelos_coorte.R` por um motivo operacional: séries
## muito ruidosas (nos desocupados o erro-padrão chega a 60% do próprio valor)
## fazem o otimizador se perder DENTRO do filtro de Kalman, que roda em código
## compilado. Ali `setTimeLimit` do R não é checado, então não há como impor um
## teto de dentro do R — o job trava inteiro sem emitir nada (aconteceu duas
## vezes: 8,3 h e 1,6 h de CPU sem sair do lugar).
##
## Rodando cada série num PROCESSO PRÓPRIO, o `timeout` do sistema operacional
## mata o caso patológico e o restante segue. Ver `07_modelos_coorte.sh`.
##
## Uso: Rscript rotinas/07a_uma_serie.R <indice_regiao> <indicador>
################################################################################
suppressMessages(library(dlm))
source("funcoes/41_modelo_rgb_coorte.R")

args <- commandArgs(trailingOnly = TRUE)
i_rg <- as.integer(args[1]); ind <- args[2]

rot <- readRDS("baserot8reg.rds")
rg  <- names(rot)[i_rg]
df  <- rot[[rg]]

y   <- as.matrix(df[, paste0(ind, "_", 1:5)])       / 1000
se  <- as.matrix(df[, paste0("se_", ind, "_", 1:5)]) / 1000
agg <- rowMeans(y)

mods <- list()
for (v in c("fixo", "nao")) {
  m <- try(f.modelo_coorte(y, se, rgb = v), silent = TRUE)
  if (!inherits(m, "try-error") && m$conv == 0) mods[[v]] <- m
}
if (!length(mods)) { cat("SEM_AJUSTE\n"); quit(status = 2) }

## só compara por AIC se as DUAS variantes convergiram (ver V1 §1.1)
comparavel <- length(mods) == 2
aic <- sapply(mods, `[[`, "aic")
escolhido <- if (comparavel) names(which.min(aic)) else names(mods)[1]
m <- mods[[escolhido]]

cv_db <- 100 * mean(sqrt(rowSums(se^2)) / 5 / agg)
d <- 100 * (m$ts.signal - agg) / agg
amp <- if (!is.null(m$lambda)) {
  l <- colMeans(m$lambda); 100 * (max(l) - min(l)) / mean(agg)
} else NA_real_

out <- data.frame(
  indicador = ind, regiao = rg,
  escolhido = if (comparavel) escolhido else paste0(escolhido, "*"),
  comparavel = comparavel,
  delta_aic_lambda = if (comparavel) round(aic[["nao"]] - aic[["fixo"]], 1) else NA_real_,
  rho = round(m$rho, 4),
  amplitude_lambda_pct = round(amp, 2),
  cv_modelo = round(mean(m$cv.signal), 2),
  cv_design = round(cv_db, 2),
  ganho_cv_pct = round(100 * (1 - mean(m$cv.signal) / cv_db), 1),
  dif_max_pct = round(max(abs(d)), 2),
  starts_ok = paste0(m$n_starts_ok, "/", m$n_starts),
  spread_ll = round(m$spread_loglik, 2),
  ruido_medio_pct = round(100 * mean(se / y), 1))

dir.create("outputs/modelos_coorte/series", recursive = TRUE, showWarnings = FALSE)
saveRDS(list(resumo = out, modelo = m),
        sprintf("outputs/modelos_coorte/series/%02d_%s.rds", i_rg, ind))
write.csv(out, sprintf("outputs/modelos_coorte/series/%02d_%s.csv", i_rg, ind),
          row.names = FALSE)

cat(sprintf("OK %-11s %-38s %-5s rho=%.3f cv %.2f->%.2f starts %s ruido %.0f%%\n",
            ind, rg, escolhido, m$rho, cv_db, mean(m$cv.signal),
            out$starts_ok, out$ruido_medio_pct))
