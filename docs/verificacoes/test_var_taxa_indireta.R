################################################################################
## VERIFICAÇÃO — fórmula da variância da taxa de desocupação "indireta"
## (a que deriva a taxa das tendências modeladas de ocupados e desocupados)
##
## O script `35_Taxa de Desocupação.R` calcula, para cada região:
##
##   T   <- trend_desoc + trend_ocup
##   tx  <- trend_desoc / T
##   var <- (1/T^2)*var_desoc + (trend_desoc^2/T^4)*(var_desoc + var_ocup)
##
## Trata-se do método delta para R = D/T. Escrito assim, ele trata D e T como se
## fossem independentes — mas T = D + O, então Cov(D,T) = Var(D) != 0, e falta o
## termo -2*(D/T^3)*Var(D).
##
## Com o termo de covariância, a expressão colapsa na forma canônica:
##   (1/T^2)Var(D) + (D^2/T^4)(Var(D)+Var(O)) - 2(D/T^3)Var(D)
##     = [ (T-D)^2 Var(D) + D^2 Var(O) ] / T^4
##     = [ O^2 Var(D) + D^2 Var(O) ] / T^4          <- delta correto p/ D/(D+O)
##
## Como o termo omitido é NEGATIVO, a fórmula do script SUPERESTIMA a variância —
## o erro é conservador (IC largos demais, CV inflado), mas é erro.
##
## Este script mede o tamanho do viés nas magnitudes reais das séries.
## Rodar da raiz:  Rscript docs/verificacoes/test_var_taxa_indireta.R
################################################################################
options(scipen = 999, width = 130)

var_script  <- function(D, O, vD, vO) { T <- D + O; (1/T^2)*vD + (D^2/T^4)*(vD + vO) }
var_correto <- function(D, O, vD, vO) { T <- D + O; (O^2*vD + D^2*vO) / T^4 }

## Usa os níveis e as precisões reais das séries (base estrutural commitada)
base <- readRDS("baseestr8reg.rds")
cat("=== Vies da formula do script 35, por regiao ===\n")
cat("    (usa os totais e erros-padrao design-based como proxy das tendencias)\n\n")

linhas <- list()
for (rg in names(base)) {
  b <- base[[rg]]
  cD <- "Total.de.desocupados"; cO <- "Total.de.ocupados"
  csD <- "sd_d"; csO <- "sd_o"
  if (!all(c(cD, cO, csD, csO) %in% names(b))) next

  D <- b[[cD]]; O <- b[[cO]]; vD <- b[[csD]]^2; vO <- b[[csO]]^2
  ok <- is.finite(D) & is.finite(O) & is.finite(vD) & is.finite(vO) & D > 0

  vs <- var_script(D[ok], O[ok], vD[ok], vO[ok])
  vc <- var_correto(D[ok], O[ok], vD[ok], vO[ok])
  linhas[[length(linhas)+1]] <- data.frame(
    regiao = substr(rg, 1, 22),
    taxa_media_pct = round(100*mean(D[ok]/(D[ok]+O[ok])), 2),
    var_inflada_pct = round(100*mean(vs/vc - 1), 1),
    se_inflado_pct  = round(100*mean(sqrt(vs/vc) - 1), 1))
}
res <- do.call(rbind, linhas)
print(res, row.names = FALSE)

cat(sprintf("\nMedia: variancia superestimada em %.1f%% -> erro-padrao e CV da taxa\n",
            mean(res$var_inflada_pct)))
cat(sprintf("indireta superestimados em %.1f%%.\n", mean(res$se_inflado_pct)))
cat("\nDirecao do erro: CONSERVADOR (IC mais largos que o devido). Nao invalida\n")
cat("conclusoes de significancia ja obtidas, mas subestima a precisao alcancada\n")
cat("pelo metodo — que e' justamente o que o artigo quer demonstrar.\n")
