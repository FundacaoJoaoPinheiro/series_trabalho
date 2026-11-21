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
## estimados separadamente, então essa covariância não está disponível.
##
## ATENÇÃO à álgebra, que já esteve errada aqui. Escrita em D e T = D + O, a
## linearização tem três termos:
##     V(r) = V(D)/T² + D²·V(T)/T⁴ − 2·(D/T³)·Cov(D,T)
## e o código calculava só os dois primeiros. Mas Cov(D,T) = Cov(D, D+O) =
## V(D) + Cov(D,O), que SOB A PRÓPRIA HIPÓTESE vale V(D) — não zero. Descartar
## o terceiro termo não era "assumir Cov(D,O) = 0": era assumir, além disso,
## Cov(D,T) = 0, o que contradiz a primeira hipótese.
##
## Escrita em D e O, a linearização com Cov(D,O) = 0 não tem termo a descartar:
##     V(r) = [O²·V(D) + D²·V(O)] / T⁴
## É o que se calcula agora. A diferença para a fórmula antiga é exatamente
## 2·D·V(D)/T³, e ela derrubou o CV médio do cálculo indireto de 7,61 % para
## 7,03 % (de 5,3 % a 10,6 % por estrato). A conclusão do artigo não muda: o
## modelo da própria taxa segue melhor (6,38 %).
##
## LIMITE conhecido: Cov(D,O) não é zero nos dados. Medida design-based nos
## microdados de 2024 (pnadc_design + svytotal dos dois indicadores), a
## correlação vai de −0,25 em Belo Horizonte a +0,27 no Vale do Rio Doce,
## trocando de sinal entre regiões. Não dá para usar esse número aqui — ele é
## dos estimadores diretos, não das tendências do filtro —, mas registra que a
## hipótese é aproximação, e que o erro que ela deixa (−4 % a +10 %) é menor e
## de sinal variável, contra os +2 % a +17 % sistemáticos da fórmula antiga.
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
## linearização em D e O, com Cov(D,O) = 0 (ver cabeçalho)
varr <- (O$trend^2 * D$var + D$trend^2 * O$var) / TL^4
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
