################################################################################
## V3 — PROVA DE CONCEITO do modelo multivariado por onda com efeito de painel
##
## Testa `funcoes/40_modelo_rgb_multivariado.R` em Minas Gerais / ocupados, que é
## onde o rotation group bias foi medido com mais precisão (p = 2e-10).
##
## CRITÉRIOS DE VALIDAÇÃO (definidos ANTES de rodar, para não virar racionalização)
##  V-1 Convergência das três variantes (rw / fixo / nao).
##  V-2 O lambda estimado tem de REPRODUZIR o índice de Bailar medido em V2:
##      ordem monotônica ent1 > ent2 > ... > ent5. É o teste de que o modelo está
##      capturando o efeito real, e não um artefato numérico.
##  V-3 A variante "rw" tem de vencer "fixo" e "nao" por AIC/BIC — o diagnóstico
##      diz que o viés deriva; se o modelo discordar, o diagnóstico é que está
##      errado.
##  V-4 A amplitude de lambda tem de crescer no tempo (V2 §3: 3,4 -> 9,2 p.p.).
##  V-5 O sinal estimado tem de ficar próximo da série agregada design-based —
##      se divergir muito, algo está errado na especificação, não no dado.
##
## Rodar da raiz do repo:  Rscript docs/verificacoes/V3_poc_modelo_rgb.R
################################################################################
options(scipen = 999, width = 140)
source("funcoes/40_modelo_rgb_multivariado.R")

REG <- "09 - Minas Gerais"
IND <- "ocupada"

rot <- readRDS("baserot8reg.rds")[[REG]]
y  <- as.matrix(rot[, paste0(IND, "_", 1:5)])      / 1000   # milhares
se <- as.matrix(rot[, paste0("se_", IND, "_", 1:5)]) / 1000
Tn <- nrow(y)

cat("=== PoC: ", REG, "/", IND, " — ", Tn, " trimestres x 5 ondas ===\n\n", sep = "")

## ---------------------------------------------------------------- V-1
cat("--- V-1 Convergencia e ajuste das tres variantes ---\n")
mods <- list()
for (v in c("rw", "fixo", "nao")) {
  m <- try(f.modelo_rgb(y, se, rgb = v), silent = TRUE)
  if (inherits(m, "try-error")) { cat(sprintf("  %-5s FALHOU: %s", v, m)); next }
  mods[[v]] <- m
  cat(sprintf("  %-5s conv=%d  npar=%d  loglik=%11.2f  AIC=%10.2f  BIC=%10.2f\n",
              v, m$conv, m$npar, m$loglik, m$aic, m$bic))
}
cat("\n")

## ---------------------------------------------------------------- V-3
cat("--- V-3 Selecao entre variantes ---\n")
if (length(mods) > 1) {
  aic <- sapply(mods, `[[`, "aic")
  cat("  melhor por AIC:", names(which.min(aic)),
      "| melhor por BIC:", names(which.min(sapply(mods, `[[`, "bic"))), "\n")
  cat("  deltaAIC vs melhor:", paste(sprintf("%s=%.1f", names(aic), aic - min(aic)),
                                     collapse = "  "), "\n\n")
}

m <- mods[["rw"]]
if (is.null(m)) { cat("variante rw indisponivel; abortando validacoes seguintes\n"); quit(status = 0) }

## ---------------------------------------------------------------- V-2
cat("--- V-2 O lambda estimado reproduz o indice de Bailar? ---\n")
lam_medio <- colMeans(m$lambda)
nivel <- mean(m$ts.signal)
bailar_modelo <- 100 * (1 + lam_medio / nivel)
bailar_dados  <- 100 * colMeans(y / rowMeans(y))

cmp <- rbind(`lambda medio (mil pessoas)` = round(lam_medio, 1),
             `indice implicado pelo modelo` = round(bailar_modelo, 2),
             `indice medido nos dados (V2)` = round(bailar_dados, 2))
colnames(cmp) <- paste0("ent", 1:5)
print(cmp)
mono <- all(diff(lam_medio) < 0)
cat("  monotonico decrescente (ent1 > ... > ent5)?", mono, "\n")
cat("  correlacao modelo x dados:", round(cor(bailar_modelo, bailar_dados), 4), "\n\n")

## ---------------------------------------------------------------- V-4
cat("--- V-4 A amplitude do efeito cresce no tempo? ---\n")
per <- list("2012-2014" = 1:12, "2015-2017" = 13:24,
            "2018-2020" = 25:36, "2021-2024" = 37:Tn)
amp <- t(sapply(per, function(ii) {
  L <- m$lambda[ii, , drop = FALSE]
  c(round(colMeans(L), 1), amplitude = round(max(colMeans(L)) - min(colMeans(L)), 1))
}))
colnames(amp) <- c(paste0("ent", 1:5), "amplitude")
print(amp)
cat("  amplitude cresce do 1o para o ultimo periodo?",
    amp[nrow(amp), "amplitude"] > amp[1, "amplitude"], "\n\n")

## ---------------------------------------------------------------- V-5
cat("--- V-5 O sinal bate com a serie agregada design-based? ---\n")
agregado <- rowMeans(y)
dif <- 100 * (m$ts.signal - agregado) / agregado
cat(sprintf("  diferenca sinal x agregado: media = %+.2f%% | desvio = %.2f%% | max|.| = %.2f%%\n",
            mean(dif), sd(dif), max(abs(dif))))
cat(sprintf("  CV medio do sinal: %.2f%% (design-based agregado: %.2f%%)\n",
            mean(m$cv.signal), 100 * mean(sqrt(rowSums(se^2))/5 / agregado)))

cat("\n=== RESUMO DA VALIDACAO ===\n")
aic <- sapply(mods, `[[`, "aic")
melhor <- names(which.min(aic))
precisa_lambda <- aic[["nao"]] - min(aic[c("rw", "fixo")])
cat(sprintf("  V-1 convergencia .................. %s\n", if (m$conv == 0) "OK" else "FALHOU"))
cat(sprintf("  V-2 lambda reproduz o Bailar ...... %s (r = %.4f)\n",
            if (mono && cor(bailar_modelo, bailar_dados) > 0.99) "OK" else "FALHOU",
            cor(bailar_modelo, bailar_dados)))
cat(sprintf("  V-3 efeito de painel e necessario . %s (deltaAIC 'nao' vs melhor = %+.1f)\n",
            if (precisa_lambda > 2) "OK" else "NAO", precisa_lambda))
cat(sprintf("  V-4 o efeito DERIVA no tempo? ..... %s (melhor variante: '%s')\n",
            if (melhor == "rw") "SIM" else "NAO - efeito constante", melhor))
cat(sprintf("  V-5 sinal proximo do agregado ..... %s (max %.1f%%, CV %.2f%% vs %.2f%%)\n",
            if (max(abs(dif)) < 6) "OK" else "VERIFICAR", max(abs(dif)),
            mean(m$cv.signal), 100 * mean(sqrt(rowSums(se^2))/5 / agregado)))
cat("\nNOTA: V-3/V-4 usam a verossimilhanca POS-DIFUSAO. A verossimilhanca cheia\n")
cat("nao e' comparavel aqui — os 4 estados lambda entram difusos (C0=1e7) e o\n")
cat("custo da difusao recai sobre o modelo COM efeito de painel, invertendo o\n")
cat("resultado (com ela, 'nao' venceria por 20 de AIC; corrigida, perde por 13).\n")
