################################################################################
## V6 — O modelo com efeito de painel vence o método atual do artigo?
##
## Compara pela MESMA métrica que o artigo publica (`funcoes/31_calculo_rrse.R`):
##
##   RRSE = mean( (se_direto - se_modelo) / se_direto ) * 100
##
## isto é, a redução relativa média do erro-padrão contra a estimativa direta
## (design-based). Positivo = o modelo melhora a precisão; NEGATIVO = piora.
##
## Os valores do método atual são os PUBLICADOS nas tabelas do manuscrito
## (`Versão atual/resultados/*/diffvicio*.tex`), colunas univariado e
## multivariado. Os do método novo são recalculados aqui a partir dos ajustes
## salvos em `outputs/modelos_coorte/series/`.
##
## Nota: comparo `se.signal` (erro-padrão do sinal = tendência + sazonal) contra
## o `sd_*` design-based, que é a mesma quantidade que o artigo confronta.
##
## Rodar da raiz:  Rscript docs/verificacoes/V6_comparacao_metodo_atual.R
################################################################################
options(scipen = 999, width = 145)

## ---- valores PUBLICADOS no artigo (diffvicioocup.tex / diffviciodesoc.tex) ---
pub <- data.frame(
  reg = c("01-Belo Horizonte","02-Colar e Entorno Metropolitano de BH","03-Sul de Minas",
          "04-Triângulo Mineiro","05-Mata de Minas Gerais","06-Norte de Minas",
          "07-Vale do Rio Doce","08-Central"),
  ocup_uni  = c(-17.50, -11.06, 49.20, 17.94, 41.34, 18.77, 36.11, 75.66),
  ocup_mult = c(-20.98,  48.17, 51.43, 15.29, 32.50, 16.35, 32.44, 39.57),
  deso_uni  = c( 36.99,  37.20, 37.57, 50.32, 46.99, NA, NA, NA),
  deso_mult = c( 45.56,  39.66, 33.26, 56.09, 42.48, NA, NA, NA),
  stringsAsFactors = FALSE)

base <- readRDS("baseestr8reg.rds")
arqs <- list.files("outputs/modelos_coorte/series", "\\.rds$", full.names = TRUE)

linhas <- list()
for (a in arqs) {
  x <- readRDS(a); m <- x$modelo; r <- x$resumo
  ind <- r$indicador; rg <- r$regiao
  b <- base[[rg]]
  if (is.null(b)) next
  se_dir <- if (ind == "ocupada") b$sd_o else b$sd_d
  se_dir <- se_dir / 1000                      # mesma escala do modelo
  se_mod <- as.numeric(m$se.signal)
  n <- min(length(se_dir), length(se_mod))
  ok <- is.finite(se_dir[1:n]) & is.finite(se_mod[1:n]) & se_dir[1:n] > 0
  linhas[[length(linhas)+1]] <- data.frame(
    indicador = ind, reg = rg,
    rrse_novo = round(100 * mean((se_dir[1:n][ok] - se_mod[1:n][ok]) / se_dir[1:n][ok]), 2),
    ruido = r$ruido_medio_pct, efeito = r$escolhido, stringsAsFactors = FALSE)
}
novo <- do.call(rbind, linhas)

cat("=== RRSE: reducao relativa media do erro-padrao vs estimativa direta (%) ===\n")
cat("    positivo = modelo MELHORA a precisao | negativo = PIORA\n\n")

for (ind in c("ocupada", "desocupada")) {
  cat("---", toupper(ind), "---\n")
  su <- if (ind == "ocupada") "ocup" else "deso"
  n <- novo[novo$indicador == ind & novo$reg != "09 - Minas Gerais", ]
  cmp <- merge(pub[, c("reg", paste0(su, "_uni"), paste0(su, "_mult"))],
               n[, c("reg", "rrse_novo", "efeito", "ruido")], by = "reg", all.x = TRUE)
  names(cmp) <- c("regiao", "atual_uni", "atual_mult", "NOVO", "efeito", "ruido")
  cmp$regiao <- substr(cmp$regiao, 1, 22)
  cmp$ganho_vs_mult <- round(cmp$NOVO - cmp$atual_mult, 2)
  print(cmp, row.names = FALSE)
  v <- cmp[!is.na(cmp$NOVO) & !is.na(cmp$atual_mult), ]
  if (nrow(v)) {
    cat(sprintf("\n  media: atual univ %.1f%% | atual mult %.1f%% | NOVO %.1f%%\n",
                mean(v$atual_uni, na.rm = TRUE), mean(v$atual_mult), mean(v$NOVO)))
    cat(sprintf("  o novo vence o multivariado em %d de %d regioes\n\n",
                sum(v$NOVO > v$atual_mult), nrow(v)))
  }
}

cat("=== O caso Belo Horizonte (ocupados) ===\n")
bh <- novo[novo$reg == "01-Belo Horizonte" & novo$indicador == "ocupada", ]
cat(sprintf("  metodo atual: univariado %.2f%% | multivariado %.2f%%  <- PIORA a precisao\n",
            pub$ocup_uni[1], pub$ocup_mult[1]))
cat(sprintf("  metodo novo : %.2f%%\n", bh$rrse_novo))
cat("  O manuscrito ja registra esse resultado negativo como uma anomalia da\n")
cat("  secao de resultados ('nao houve melhora na estimacao dos erros padrao').\n")
