################################################################################
## V9 — `V` cheia por correlação fixa entre ondas: o caminho C se sustenta?
##
## O V7/V8 mostraram que as 5 ondas são NEGATIVAMENTE correlacionadas (compartilham
## UPAs): -0,215 nos ocupados e -0,110 nos desocupados, estável em 162 medições.
## O modelo assumia independência, tratando as observações como ~2,2x mais
## ruidosas (no erro-padrão) do que são — e por isso não superava a estimativa
## direta.
##
## Aqui a correlação entra como equicorrelação fixa nas inovações do bloco de erro
## amostral (`cor_ondas` em `funcoes/41_`), sem reprocessar microdados.
##
## COMPARAÇÃO CONTRA A REFERÊNCIA CORRETA: o CV design-based do TOTAL, vindo de
## `baseestr8reg.rds` (`sd_o`/`sd_d` ÷ total). NÃO a referência derivada dos `se`
## por onda somados como independentes — foi esse o erro do V5/V6.
##
## Rodar da raiz:  Rscript docs/verificacoes/V9_V_cheia_correlacao_fixa.R
################################################################################
options(scipen = 999, width = 140)
source("funcoes/41_modelo_rgb_coorte.R")

COR <- c(ocupada = -0.215, desocupada = -0.110)   # medidas no V8

rot  <- readRDS("baserot8reg.rds")
estr <- readRDS("baseestr8reg.rds")

## sanidade: a correlação implicada bate com a pedida?
cat("=== Sanidade: correlacao implicada no modelo ===\n")
df <- rot[["09 - Minas Gerais"]]
y  <- as.matrix(df[, paste0("ocupada_", 1:5)]) / 1000
se <- as.matrix(df[, paste0("se_ocupada_", 1:5)]) / 1000
for (cw in c(0, -0.215)) {
  m <- f.modelo_coorte(y, se, rgb = "fixo", cor_ondas = cw,
                       init = c(-7.7, -11.7, -11.7, atanh(0.9)))
  W <- m$mod$W[m$i_a, m$i_a]
  R <- cov2cor(W)
  cat(sprintf("  cor_ondas = %+.3f -> corr. media das inovacoes = %+.3f | rho = %.3f\n",
              cw, mean(R[upper.tri(R)]), m$rho))
}

## ------------------------------------------------------------------ comparação
cat("\n=== Modelo com e sem a correlacao, contra o CV design-based do TOTAL ===\n\n")
linhas <- list()
for (ind in c("ocupada", "desocupada")) {
  for (rg in names(rot)) {
    df <- rot[[rg]]; b <- estr[[rg]]
    if (is.null(b)) next
    y  <- as.matrix(df[, paste0(ind, "_", 1:5)]) / 1000
    se <- as.matrix(df[, paste0("se_", ind, "_", 1:5)]) / 1000

    ## referencia CORRETA: CV design-based do total
    tot  <- if (ind == "ocupada") b$Total.de.ocupados else b$Total.de.desocupados
    sd_t <- if (ind == "ocupada") b$sd_o else b$sd_d
    n <- min(nrow(y), length(tot))
    cv_ref <- 100 * mean(sd_t[1:n] / tot[1:n])

    res <- list()
    for (tag in c("sem", "com")) {
      cw <- if (tag == "com") COR[[ind]] else 0
      ## ponto de partida informado pela variancia empirica (1 start): com W
      ## nao-diagonal o multi-start de 8 fica caro, e aqui o objetivo e' comparar
      ## "com" vs "sem" na mesma serie, nao achar o otimo global.
      v0 <- log(max(var(diff(rowMeans(y / mean(y)))), 1e-10))
      m <- try(f.modelo_coorte(y, se, rgb = "fixo", cor_ondas = cw,
                               init = c(v0, v0 - 4, v0 - 4, atanh(0.9))), silent = TRUE)
      res[[tag]] <- if (inherits(m, "try-error")) NA_real_ else mean(m$cv.signal)
    }
    linhas[[length(linhas)+1]] <- data.frame(
      indicador = ind, regiao = substr(rg, 1, 21),
      cv_design = round(cv_ref, 2),
      cv_sem = round(res$sem, 2), cv_com = round(res$com, 2),
      ganho_sem = round(100 * (1 - res$sem / cv_ref), 1),
      ganho_com = round(100 * (1 - res$com / cv_ref), 1))
    cat(sprintf("  %-11s %-21s design %5.2f%% | sem %6.2f%% (%+6.1f%%) | com %6.2f%% (%+6.1f%%)\n",
        ind, substr(rg,1,21), cv_ref, res$sem, 100*(1-res$sem/cv_ref),
        res$com, 100*(1-res$com/cv_ref)))
    flush.console()
  }
}
out <- do.call(rbind, linhas)
dir.create("outputs/modelos_coorte", showWarnings = FALSE, recursive = TRUE)
write.csv(out, "outputs/modelos_coorte/V9_correlacao_fixa.csv", row.names = FALSE)

cat("\n=== RESUMO (ganho = reducao do CV vs design-based do total) ===\n")
for (ind in unique(out$indicador)) {
  s <- out[out$indicador == ind, ]
  cat(sprintf("  %-11s: sem correlacao %+6.1f%% | com correlacao %+6.1f%% | melhora em %d de %d\n",
      ind, mean(s$ganho_sem, na.rm=TRUE), mean(s$ganho_com, na.rm=TRUE),
      sum(s$ganho_com > s$ganho_sem, na.rm=TRUE), nrow(s)))
}
cat("\n  ganho POSITIVO = modelo mais preciso que a estimativa direta.\n")
cat("  E' este numero que decide se o caminho C se sustenta.\n")
