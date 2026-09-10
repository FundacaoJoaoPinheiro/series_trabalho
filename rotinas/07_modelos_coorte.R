################################################################################
## 07 - MODELOS POR ONDA COM EFEITO DE PAINEL E ERRO NA COORTE — 9 regiões
## Fase 3 do caminho C. Aplica `funcoes/41_modelo_rgb_coorte.R` a todas as séries.
##
## Para CADA região × indicador ajusta as duas variantes e escolhe por AIC
## pós-difusão:
##   "fixo" = com efeito de painel (lambda constante)
##   "nao"  = sem efeito de painel
## O diagnóstico V2 prevê que os OCUPADOS precisem de lambda (RGB significativo em
## 8/9 regiões) e os DESOCUPADOS não (0/9). Se o AIC concordar em cada série, é
## confirmação independente do diagnóstico — e é o teste que interessa aqui.
##
## Saída: outputs/modelos_coorte/<indicador>.rds + tabela-resumo em CSV.
## Rodar da raiz:  Rscript rotinas/07_modelos_coorte.R
################################################################################
options(scipen = 999, width = 150)
source("funcoes/40_modelo_rgb_multivariado.R")
source("funcoes/41_modelo_rgb_coorte.R")

TETO_SEG <- 240   # teto de tempo por variante (media observada: ~35 s)
dir_out <- "outputs/modelos_coorte"
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

rot <- readRDS("baserot8reg.rds")
regioes <- names(rot)

linhas <- list()
for (ind in c("ocupada", "desocupada")) {
  ajustes <- list()
  for (rg in regioes) {
    df <- rot[[rg]]
    y  <- as.matrix(df[, paste0(ind, "_", 1:5)])       / 1000
    se <- as.matrix(df[, paste0("se_", ind, "_", 1:5)]) / 1000
    agg <- rowMeans(y)

    ## Teto de tempo POR VARIANTE. Sem isto uma única série patológica trava o
    ## job inteiro sem sinal nenhum — foi o que aconteceu na 1ª rodada robusta
    ## (8,3 h de CPU sem terminar, enquanto a média por série é ~70 s).
    mods <- list()
    for (v in c("fixo", "nao")) {
      t0 <- Sys.time()
      m <- try({
        setTimeLimit(elapsed = TETO_SEG, transient = TRUE)
        on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
        f.modelo_coorte(y, se, rgb = v)
      }, silent = TRUE)
      setTimeLimit(elapsed = Inf)
      dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      if (!inherits(m, "try-error") && m$conv == 0) {
        mods[[v]] <- m
      } else {
        cat(sprintf("    ! %s/%s/%s falhou apos %.0fs: %s\n", ind, rg, v, dt,
                    if (dt >= TETO_SEG) "ESTOUROU O TETO" else "nao convergiu"))
      }
      flush.console()
    }
    if (!length(mods)) { message("FALHOU (nenhuma variante): ", ind, " / ", rg); next }

    ## REGRA: só se compara por AIC quando as DUAS variantes convergiram. Se só
    ## uma convergiu, o resultado é registrado como NAO COMPARAVEL — e não como
    ## "seleção". É exatamente o vício apontado no V1 §1.1 (o Vale do Rio Doce
    ## ficou com uma única especificação candidata e isso passou por escolha).
    comparavel <- length(mods) == 2
    aic <- sapply(mods, `[[`, "aic")
    escolhido <- if (comparavel) names(which.min(aic)) else names(mods)[1]
    m <- mods[[escolhido]]
    ajustes[[rg]] <- m
    if (!comparavel)
      message("NAO COMPARAVEL (so '", escolhido, "' convergiu): ", ind, " / ", rg)

    cv_db <- 100 * mean(sqrt(rowSums(se^2)) / 5 / agg)
    d <- 100 * (m$ts.signal - agg) / agg
    amp <- if (!is.null(m$lambda)) {
      l <- colMeans(m$lambda); 100 * (max(l) - min(l)) / mean(agg)
    } else NA_real_

    linhas[[length(linhas) + 1]] <- data.frame(
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
      spread_ll = round(m$spread_loglik, 2))
    cat(sprintf("  %-11s %-40s %-5s rho=%.3f cv %.2f->%.2f  starts %d/%d\n",
                ind, rg, escolhido, m$rho, cv_db, mean(m$cv.signal),
                m$n_starts_ok, m$n_starts))
  }
  saveRDS(ajustes, file.path(dir_out, paste0(ind, ".rds")))
}

res <- do.call(rbind, linhas)
write.csv(res, file.path(dir_out, "resumo.csv"), row.names = FALSE)

cat("\n=== RESUMO ===\n")
print(res, row.names = FALSE)

cat("\n--- Efeito de painel foi selecionado em quantas series? ---\n")
for (ind in unique(res$indicador)) {
  s <- res[res$indicador == ind, ]
  cat(sprintf("  %-11s: %d de %d  (delta AIC medio a favor do lambda: %+.1f)\n",
              ind, sum(s$escolhido == "fixo"), nrow(s),
              mean(s$delta_aic_lambda, na.rm = TRUE)))
}
cat("\n(previsao do V2: ocupados SIM em ~8/9, desocupados NAO em ~9/9)\n")
cat(sprintf("\nrho medio: ocupados %.3f | desocupados %.3f\n",
            mean(res$rho[res$indicador == "ocupada"]),
            mean(res$rho[res$indicador == "desocupada"])))
cat(sprintf("ganho medio de CV: %.1f%%\n", mean(res$ganho_cv_pct)))
