################################################################################
## 24_taxa_final.R   — PASSO 5 do pipeline de revisão
##
## Compara as duas estratégias para a taxa de desocupação, ambas sob a
## especificação final e Var(ẽ) = 1:
##   INDIRETA  r_L = D_L / (D_L + O_L), a partir das tendências multivariadas
##             de desocupados e ocupados, com variância por linearização
##   DIRETA    tendência do modelo multivariado aplicado à própria taxa
##
## HIPÓTESE DECLARADA: Cov(D_L, O_L) = 0. Os dois componentes vêm de modelos
## separados, então a covariância não está disponível. O viés tem direção
## conhecida: desocupados e ocupados são negativamente correlacionados na
## prática, logo Cov(D_L, T_L) > 0 e o termo descartado REDUZIRIA a variância —
## a variância aqui calculada tende a ser uma sobre-estimativa.
################################################################################

suppressMessages(library(dlm))

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)

BURN  <- 8
SAIDA <- file.path(RAIZ, "outputs", "taxa_final")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)
B_NIVEL <- 1:8; B_SAZ1 <- 17:24; B_SAZ3 <- 33:40

componente <- function(ind) {
  uni <- readRDS(file.path(RAIZ, "outputs", "univariado_final",
                           paste0("modelos_", ind, ".rds")))
  m   <- readRDS(file.path(RAIZ, "outputs", "multivariado_final",
                           paste0("multivariado_", ind, ".rds")))
  Y   <- sapply(names(uni), function(k) uni[[k]]$serie$y)
  flt <- dlmFilter(Y, m$mod)
  mse <- dlmSvd2var(flt$U.C, flt$D.C)
  est <- dropFirst(flt$m)
  list(trend = est[, B_NIVEL],
       var   = sapply(B_NIVEL, function(i) dropFirst(sapply(mse, function(x) x[i,i]))),
       rot   = sapply(names(uni), function(k) uni[[k]]$rotulo))
}

D <- componente("desocupados"); O <- componente("ocupados")

## --- taxa indireta e variância por linearização de Taylor -------------------
TL   <- D$trend + O$trend
r    <- D$trend / TL
varT <- D$var + O$var                       # Cov(D,O) assumida nula
varr <- (1/TL^2) * D$var + (D$trend^2 / TL^4) * varT
se_r <- sqrt(varr)

## --- taxa direta: multivariado aplicado à própria taxa ----------------------
uniT <- readRDS(file.path(RAIZ, "outputs", "univariado_final", "modelos_taxa.rds"))
mT   <- readRDS(file.path(RAIZ, "outputs", "multivariado_final", "multivariado_taxa.rds"))
YT   <- sapply(names(uniT), function(k) uniT[[k]]$serie$y)      # em pontos percentuais
SET  <- sapply(names(uniT), function(k) uniT[[k]]$serie$se)
fltT <- dlmFilter(YT, mT$mod); mseT <- dlmSvd2var(fltT$U.C, fltT$D.C)
estT <- dropFirst(fltT$m)
tr_dir <- estT[, B_NIVEL]
se_dir_mod <- sapply(B_NIVEL, function(i) dropFirst(sapply(mseT, function(x) sqrt(x[i,i]))))

ix <- (BURN+1):nrow(r)
tab <- data.frame(
  estrato = D$rot,
  cv_direta   = round(sapply(1:8, function(i) mean(SET[ix,i]/YT[ix,i])*100), 2),
  cv_indireta = round(sapply(1:8, function(i) mean(se_r[ix,i]*100/(r[ix,i]*100))*100), 2),
  cv_direta_mod = round(sapply(1:8, function(i) mean(se_dir_mod[ix,i]/tr_dir[ix,i])*100), 2),
  ganho_indireta = round(sapply(1:8, function(i)
      mean((SET[ix,i] - se_r[ix,i]*100)/SET[ix,i])*100), 2),
  ganho_direta   = round(sapply(1:8, function(i)
      mean((SET[ix,i] - se_dir_mod[ix,i])/SET[ix,i])*100), 2),
  vicio_indireta = round(sapply(1:8, function(i)
      sum(r[ix,i]*100 - YT[ix,i])/sum(YT[ix,i])*100), 2),
  stringsAsFactors = FALSE)

cat("############ TAXA DE DESOCUPAÇÃO — INDIRETA vs DIRETA ############\n\n")
print(tab, row.names = FALSE)
cat("\nganho médio  — indireta:", round(mean(tab$ganho_indireta), 2),
    "% | direta:", round(mean(tab$ganho_direta), 2), "%\n")
cat("CV médio     — direta do desenho:", round(mean(tab$cv_direta), 2),
    "% | indireta:", round(mean(tab$cv_indireta), 2),
    "% | direta modelada:", round(mean(tab$cv_direta_mod), 2), "%\n")
cat("vício absoluto médio da indireta:", round(mean(abs(tab$vicio_indireta)), 2), "%\n")

saveRDS(list(taxa_indireta = r, se_indireta = se_r, taxa_direta = tr_dir,
             se_direta = se_dir_mod, observada = YT, se_observada = SET,
             desempenho = tab, rotulos = D$rot),
        file.path(SAIDA, "taxa_final.rds"))
write.csv(tab, file.path(SAIDA, "desempenho_taxa.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
cat("\nGravado em", SAIDA, "\n")
