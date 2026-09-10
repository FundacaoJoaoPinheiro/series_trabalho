################################################################################
## V7 — AUDITORIA dos erros-padrão por grupo de rotação
##
## O V6 mostrou que somar as variâncias das 5 ondas implica um erro-padrão do
## total 2,2 a 4,8x MAIOR (ocupados) que o `sd` que o survey calcula da amostra
## completa. Isso invalidou a comparação de precisão do caminho C.
##
## Hipótese a testar: as 5 ondas NÃO são independentes. Na PNADc uma mesma UPA
## contribui com domicílios para vários grupos de rotação, então as estimativas
## por onda compartilham clusters e têm COVARIÂNCIA. Se ela for negativa,
## Var(total) < soma das Var(ondas) — e não há erro nenhum nos `se`, apenas uso
## indevido meu ao somá-los como se fossem independentes.
##
##   Var(T) = soma_j Var(j) + 2 * soma_{i<j} Cov(i,j)
##
## `svyby(..., covmat = TRUE)` devolve a matriz de covariância completa entre os
## subdomínios, o que permite decidir isso diretamente.
##
## Uso: Rscript docs/verificacoes/V7_auditoria_se_rotacao.R <caminho_do_zip>
################################################################################
options(scipen = 999, width = 130)
suppressMessages({ library(PNADcIBGE); library(survey); library(dplyr) })

args <- commandArgs(trailingOnly = TRUE)
zip  <- if (length(args)) args[1] else stop("informe o zip de um trimestre")
input <- "data/documentacao/input_PNADC_trimestral.txt"
stopifnot(file.exists(zip), file.exists(input))

td <- file.path(tempdir(), "v7"); dir.create(td, showWarnings = FALSE)
unzip(zip, exdir = td)
txt <- list.files(td, "[.]txt$", recursive = TRUE, full.names = TRUE)
txt <- txt[which.max(file.size(txt))]

cat("lendo", basename(zip), "...\n")
des <- pnadc_design(read_pnadc(txt, input, vars = c("V1016", "VD4002"))) %>%
  update(ocupada = 1 * (VD4002 == 1), desocupada = 1 * (VD4002 == 2))
unlink(td, recursive = TRUE)

mg <- subset(des, UF == "31")

for (v in c("ocupada", "desocupada")) {
  f <- as.formula(paste0("~", v))
  cat("\n==================", toupper(v), "— Minas Gerais ==================\n")

  ## (a) total pela amostra COMPLETA
  tot <- svytotal(f, mg, na.rm = TRUE)
  T_est <- as.numeric(tot); T_se <- as.numeric(SE(tot))
  cat(sprintf("  (a) TOTAL  : %12.0f  se = %10.0f  cv = %5.2f%%\n",
              T_est, T_se, 100 * T_se / T_est))

  ## (b) por onda, COM a matriz de covariância entre os subdomínios
  by <- svyby(f, ~V1016, mg, svytotal, na.rm = TRUE, covmat = TRUE)
  est <- as.numeric(by[[2]])
  V   <- vcov(by)
  se_j <- sqrt(diag(V))

  cat(sprintf("  (b) ondas  : soma = %12.0f  (razao com o total: %.4f)\n",
              sum(est), sum(est) / T_est))
  cat("      estimativas:", paste(sprintf("%.0f", est), collapse = " "), "\n")
  cat("      se por onda:", paste(sprintf("%.0f", se_j), collapse = " "), "\n")

  ## (c) se do total IMPLICADO, ignorando covariancia (o que o V6 fez)
  se_indep <- sqrt(sum(diag(V)))
  ## (d) se do total IMPLICADO, usando a matriz de covariancia completa
  se_full  <- sqrt(sum(V))

  cat(sprintf("\n  (c) se(T) somando so as VARIANCIAS   : %10.0f  (razao vs (a): %.2f)\n",
              se_indep, se_indep / T_se))
  cat(sprintf("  (d) se(T) com a matriz de COVARIANCIA: %10.0f  (razao vs (a): %.2f)\n",
              se_full, se_full / T_se))

  ## correlacoes entre ondas
  R <- cov2cor(V)
  cat(sprintf("\n  correlacao media entre ondas distintas: %+.3f  (min %+.3f, max %+.3f)\n",
              mean(R[upper.tri(R)]), min(R[upper.tri(R)]), max(R[upper.tri(R)])))
  cat("  matriz de correlacao:\n")
  print(round(R, 3))
}

cat("\n=== LEITURA ===\n")
cat("Se (d) ~ (a) e as correlacoes forem NEGATIVAS, os `se` por onda estao CORRETOS\n")
cat("e o erro foi meu no V6/V5: somei variancias de estimativas correlacionadas.\n")
cat("Se (d) continuar >> (a), ha problema real na estimacao por subdominio.\n")
