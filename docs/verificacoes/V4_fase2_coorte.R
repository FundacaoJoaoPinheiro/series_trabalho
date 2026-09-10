################################################################################
## V4 — FASE 2: erro amostral correlacionado ao longo da coorte
##
## Compara `funcoes/41_modelo_rgb_coorte.R` (erro amostral AR(1) na diagonal da
## coorte) com `funcoes/40_modelo_rgb_multivariado.R` (erro independente), e
## perfila a verossimilhança em rho para checar a identificação.
##
## CRITÉRIOS (definidos antes de rodar)
##  W-1 converge e vence a fase 1 por AIC pós-difusão;
##  W-2 rho tem MÁXIMO INTERIOR — se a verossimilhança só crescer até 1, o erro
##      amostral está competindo com o nível pela variação de baixa frequência e
##      a especificação não está identificada;
##  W-3 o lambda estimado permanece (robustez do achado de RGB à especificação
##      do erro amostral);
##  W-4 o sinal continua próximo da série agregada design-based.
##
## Rodar da raiz do repo:  Rscript docs/verificacoes/V4_fase2_coorte.R
################################################################################
options(scipen = 999, width = 140)
source("funcoes/40_modelo_rgb_multivariado.R")
source("funcoes/41_modelo_rgb_coorte.R")

REG <- "09 - Minas Gerais"; IND <- "ocupada"
rot <- readRDS("baserot8reg.rds")[[REG]]
y   <- as.matrix(rot[, paste0(IND, "_", 1:5)])      / 1000
se  <- as.matrix(rot[, paste0("se_", IND, "_", 1:5)]) / 1000
agg <- rowMeans(y)

cat("=== V4 fase 2: ", REG, " / ", IND, " ===\n\n", sep = "")

## ------------------------------------------------------------------ W-1
m1 <- f.modelo_rgb(y, se, rgb = "fixo")
m2 <- f.modelo_coorte(y, se, rgb = "fixo")

cat("--- W-1 Fase 1 (erro independente) vs Fase 2 (erro na coorte) ---\n")
cat(sprintf("  fase 1: conv=%d npar=%d loglik_pos=%10.2f AIC=%9.2f CV_sinal=%.2f%%\n",
            m1$conv, m1$npar, m1$loglik_pos, m1$aic, mean(m1$cv.signal)))
cat(sprintf("  fase 2: conv=%d npar=%d loglik_pos=%10.2f AIC=%9.2f CV_sinal=%.2f%%  rho=%.4f\n",
            m2$conv, m2$npar, m2$loglik_pos, m2$aic, mean(m2$cv.signal), m2$rho))
cat(sprintf("  deltaAIC (fase2 - fase1): %+.2f\n\n", m2$aic - m1$aic))

## ------------------------------------------------------------------ W-2
cat("--- W-2 Verossimilhanca perfilada em rho (identificacao) ---\n")
cat(sprintf("  %6s %13s %10s\n", "rho", "loglik_pos", "CV_sinal"))
grade <- c(0.50, 0.70, 0.85, 0.95, 0.98, 0.995)
prof <- sapply(grade, function(r) {
  m <- try(f.modelo_coorte(y, se, rgb = "fixo", rho_fixo = r), silent = TRUE)
  if (inherits(m, "try-error")) return(c(NA, NA))
  c(m$loglik_pos, mean(m$cv.signal))
})
for (i in seq_along(grade))
  cat(sprintf("  %6.3f %13.2f %9.2f%%\n", grade[i], prof[1, i], prof[2, i]))
i_max <- which.max(prof[1, ])
interior <- i_max > 1 && i_max < length(grade)
cat(sprintf("  maximo na grade: rho = %.3f | INTERIOR? %s\n\n",
            grade[i_max], if (interior) "SIM" else "NAO - verificar identificacao"))

## ------------------------------------------------------------------ W-3
cat("--- W-3 O lambda sobrevive a mudanca de especificacao do erro? ---\n")
lam <- round(rbind(fase1 = colMeans(m1$lambda), fase2 = colMeans(m2$lambda)), 1)
print(lam)
cat(sprintf("  correlacao entre os dois: %.4f | maior dif. absoluta: %.1f mil\n\n",
            cor(lam[1, ], lam[2, ]), max(abs(lam[1, ] - lam[2, ]))))

## ------------------------------------------------------------------ W-4
cat("--- W-4 Sinal x agregado design-based ---\n")
d <- 100 * (m2$ts.signal - agg) / agg
cv_db <- 100 * mean(sqrt(rowSums(se^2)) / 5 / agg)
cat(sprintf("  dif: media %+.2f%% | max|.| %.2f%%\n", mean(d), max(abs(d))))
cat(sprintf("  CV do sinal %.2f%% contra %.2f%% do design-based agregado\n\n",
            mean(m2$cv.signal), cv_db))

cat("=== RESUMO ===\n")
cat(sprintf("  W-1 fase 2 vence por AIC ........ %s (%+.1f)\n",
            if (m2$aic < m1$aic) "OK" else "NAO", m2$aic - m1$aic))
cat(sprintf("  W-2 rho identificado ............ %s (rho = %.4f)\n",
            if (interior) "OK" else "VERIFICAR", m2$rho))
cat(sprintf("  W-3 lambda robusto .............. %s (r = %.4f)\n",
            if (cor(lam[1, ], lam[2, ]) > 0.99) "OK" else "VERIFICAR", cor(lam[1, ], lam[2, ])))
cat(sprintf("  W-4 sinal proximo do agregado ... %s (max %.1f%%)\n",
            if (max(abs(d)) < 8) "OK" else "VERIFICAR", max(abs(d))))
