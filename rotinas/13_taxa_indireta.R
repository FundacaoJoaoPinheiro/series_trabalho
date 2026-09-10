################################################################################
## 13_taxa_indireta.R
##
## Calcula a taxa de desocupacao de forma INDIRETA, a partir das tendencias
## estimadas para o total de desocupados e o total de ocupados nos modelos
## multivariados corrigidos:
##
##   r_L = D_L / (D_L + O_L),      T_L = D_L + O_L
##   V(r_L) ~ (1/T_L^2) V(D_L) + (D_L^2 / T_L^4) V(T_L) - 2 (D_L/T_L^3) Cov(D_L,T_L)
##
## Como desocupados e ocupados sao estimados em modelos separados, a covariancia
## entre as duas tendencias nao esta disponivel e e assumida nula, como no
## artigo. Sob essa hipotese V(T_L) = V(D_L) + V(O_L) e o termo cruzado cai.
##
## LIMITACAO: a hipotese de covariancia nula e conservadora em uma direcao e
## otimista na outra. Desocupados e ocupados sao negativamente correlacionados
## na pratica (quem sai da desocupacao entra na ocupacao), de modo que
## Cov(D_L, T_L) > 0 e o termo descartado REDUZIRIA a variancia. Ou seja, a
## variancia aqui calculada tende a ser uma sobre-estimativa.
################################################################################

suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)

BURN  <- 8
SAIDA <- file.path(RAIZ, "outputs", "taxa_indireta")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)
B_NIVEL <- 1:8

## --- tendencias e variancias dos dois componentes ----------------------------
componente <- function(ind) {
  uni <- readRDS(file.path(RAIZ, "outputs", "univariado_corrigido",
                           paste0("modelos_", ind, ".rds")))
  m   <- readRDS(file.path(RAIZ, "outputs", "multivariado_cholesky",
                           paste0("multivariado_", ind, ".rds")))
  Y   <- sapply(names(uni), function(k) uni[[k]]$serie$y)
  flt <- dlmFilter(Y, m$mod)
  mse <- dlmSvd2var(flt$U.C, flt$D.C)
  est <- dropFirst(flt$m)
  list(trend = est[, B_NIVEL],
       var   = sapply(B_NIVEL, function(i) dropFirst(sapply(mse, function(x) x[i, i]))),
       rot   = sapply(names(uni), function(k) uni[[k]]$rotulo))
}

D <- componente("desocupados")
O <- componente("ocupados")

## --- taxa indireta e variancia por linearizacao ------------------------------
TL   <- D$trend + O$trend
r    <- D$trend / TL
varT <- D$var + O$var                       # Cov(D,O) assumida nula
varr <- (1 / TL^2) * D$var + (D$trend^2 / TL^4) * varT
se_r <- sqrt(varr)

## --- comparacao com a estimativa direta da taxa ------------------------------
base <- readRDS(file.path(RAIZ, "baseestr8reg.rds"))
nomes_reg <- names(base)[1:8]
tx_dir <- sapply(1:8, function(i) base[[ nomes_reg[i] ]][["Taxa.de.desocupação"]])
se_dir <- sapply(1:8, function(i) base[[ nomes_reg[i] ]][["sd_txd"]])

ix <- (BURN + 1):nrow(r)
tab <- data.frame(
  estrato = D$rot,
  rrse  = round(sapply(1:8, function(i)
            mean((se_dir[ix, i] - se_r[ix, i]) / se_dir[ix, i]) * 100), 2),
  vicio = round(sapply(1:8, function(i)
            sum(r[ix, i] - tx_dir[ix, i]) / sum(tx_dir[ix, i]) * 100), 2),
  cv_direta = round(sapply(1:8, function(i) mean(se_dir[ix, i]/tx_dir[ix, i]) * 100), 2),
  cv_indireta = round(sapply(1:8, function(i) mean(se_r[ix, i]/r[ix, i]) * 100), 2),
  stringsAsFactors = FALSE)

cat("################ TAXA DE DESOCUPAÇÃO — CÁLCULO INDIRETO ################\n")
print(tab, row.names = FALSE)
cat("\nganho médio de erro-padrão:", round(mean(tab$rrse), 2), "%\n")

saveRDS(list(taxa = r, se = se_r, direta = tx_dir, se_direta = se_dir,
             desempenho = tab, rotulos = D$rot),
        file.path(SAIDA, "taxa_indireta.rds"))
write.csv(tab, file.path(SAIDA, "desempenho_taxa_indireta.csv"), row.names = FALSE)
cat("\nGravado em", SAIDA, "\n")
