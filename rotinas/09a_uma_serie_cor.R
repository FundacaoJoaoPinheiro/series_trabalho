################################################################################
## 09a - Ajusta UMA série COM a correlação entre ondas, usando multi-start.
##
## O caso SEM correlação já está em `outputs/modelos_coorte/series/` (fase 3, com
## multi-start) — ali o `cv_modelo` é válido; o que estava errado era só a
## referência de comparação. Por isso aqui só se roda o caso COM correlação.
##
## Um processo por série porque o otimizador pode travar dentro do filtro de
## Kalman (código compilado), onde nenhum teto imposto de dentro do R funciona.
##
## Uso: Rscript rotinas/09a_uma_serie_cor.R <indice_regiao> <indicador>
################################################################################
suppressMessages(library(dlm))
source("funcoes/41_modelo_rgb_coorte.R")

COR <- c(ocupada = -0.215, desocupada = -0.110)   # medidas no V8 (162 casos)

args <- commandArgs(trailingOnly = TRUE)
i_rg <- as.integer(args[1]); ind <- args[2]

rot  <- readRDS("baserot8reg.rds")
estr <- readRDS("baseestr8reg.rds")
rg   <- names(rot)[i_rg]
df   <- rot[[rg]]; b <- estr[[rg]]

y   <- as.matrix(df[, paste0(ind, "_", 1:5)])       / 1000
se  <- as.matrix(df[, paste0("se_", ind, "_", 1:5)]) / 1000

## referência CORRETA: CV design-based do TOTAL (survey, amostra completa)
tot  <- if (ind == "ocupada") b$Total.de.ocupados else b$Total.de.desocupados
sd_t <- if (ind == "ocupada") b$sd_o else b$sd_d
n <- min(nrow(y), length(tot))
cv_design <- 100 * mean(sd_t[1:n] / tot[1:n])

m <- f.modelo_coorte(y, se, rgb = "fixo", cor_ondas = COR[[ind]])   # multi-start

out <- data.frame(
  indicador = ind, regiao = rg,
  cor_ondas = COR[[ind]],
  rho = round(m$rho, 4),
  cv_design = round(cv_design, 3),
  cv_com = round(mean(m$cv.signal), 3),
  ganho_com = round(100 * (1 - mean(m$cv.signal) / cv_design), 1),
  starts_ok = paste0(m$n_starts_ok, "/", m$n_starts),
  spread_ll = round(m$spread_loglik, 2))

dir.create("outputs/modelos_cor", recursive = TRUE, showWarnings = FALSE)
saveRDS(list(resumo = out, modelo = m),
        sprintf("outputs/modelos_cor/%02d_%s.rds", i_rg, ind))
write.csv(out, sprintf("outputs/modelos_cor/%02d_%s.csv", i_rg, ind), row.names = FALSE)

cat(sprintf("OK %-11s %-38s rho=%.3f design %.2f%% -> com %.2f%% (%+.1f%%) starts %s\n",
            ind, rg, m$rho, cv_design, mean(m$cv.signal), out$ganho_com, out$starts_ok))
