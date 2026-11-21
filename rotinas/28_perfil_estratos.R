################################################################################
## 28_perfil_estratos.R   — PASSO 8 do pipeline de revisão
##
## Perfil dos oito estratos para a Seção 2 do artigo. Este script cobre a parte
## que sai da PRÓPRIA PNAD Contínua, já estimada pelo pipeline: peso de cada
## estrato na ocupação do estado, nível de ocupação e de desocupação, taxa de
## desocupação e a precisão da estimativa direta.
##
## O QUE AINDA NÃO ESTÁ AQUI, e por quê:
##   - nº de municípios e cidade-polo  -> dependem da correspondência município x
##     estrato do SIPD, que não está no Drive;
##   - população e composição do VAB   -> SIDRA 6579 e 5938, por município; só
##     agregáveis por estrato com a mesma correspondência;
##   - rendimento médio real           -> exige os microdados trimestrais da
##     PNADc, que não estão nos .rds do pipeline.
## Quando a correspondência aparecer, elas entram aqui, na mesma tabela.
##
## Insumo : outputs/univariado_final/modelos_<indicador>.rds
## Saída  : outputs/perfil_estratos/perfil_estratos.csv
################################################################################

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)

SAIDA <- file.path(RAIZ, "outputs", "perfil_estratos")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

BURN <- 8                      # trimestres de aquecimento do filtro, descartados
T_FIM <- 52                    # 2024Q4; a série começa em 2012Q1
ANO   <- (T_FIM - 3):T_FIM     # os quatro trimestres de 2024
JAN   <- (BURN + 1):T_FIM      # 2014Q1 a 2024Q4, a janela publicada

## rótulos como no artigo (o `rotulo` gravado nos .rds vem da base e ainda traz
## "Mata de Minas Gerais", corrigido para "Zona da Mata" nas saídas do 25_)
ROT <- c("01 - Belo Horizonte",
         "02 - Colar e Entorno Metropolitano de BH",
         "03 - Sul de Minas", "04 - Triângulo Mineiro", "05 - Zona da Mata",
         "06 - Norte de Minas", "07 - Vale do Rio Doce", "08 - Central")

le <- function(ind) {
  u <- readRDS(file.path(RAIZ, "outputs", "univariado_final",
                         paste0("modelos_", ind, ".rds")))
  list(y  = sapply(names(u), function(k) u[[k]]$serie$y),
       se = sapply(names(u), function(k) u[[k]]$serie$se))
}
ocup <- le("ocupados"); desoc <- le("desocupados"); taxa <- le("taxa")

cv <- function(d, ix) colMeans(100 * d$se[ix, ] / d$y[ix, ])

## totais do estado: a soma dos oito estratos cobre Minas Gerais, porque os dez
## estratos do SIPD foram agrupados sem sobra (o trecho mineiro da RIDE de
## Brasília entrou no Norte de Minas, e o Colar foi somado ao Entorno)
ocup_mg  <- rowSums(ocup$y)
desoc_mg <- rowSums(desoc$y)

perfil <- data.frame(
  estrato            = ROT,
  ocupados_2024      = round(colMeans(ocup$y[ANO, ]), 1),
  part_ocup_mg_2024  = round(100 * colMeans(ocup$y[ANO, ]) / mean(ocup_mg[ANO]), 1),
  desocupados_2024   = round(colMeans(desoc$y[ANO, ]), 1),
  taxa_2024          = round(colMeans(taxa$y[ANO, ]), 1),
  taxa_media_periodo = round(colMeans(taxa$y[JAN, ]), 1),
  cv_ocup            = round(cv(ocup, JAN), 1),
  cv_desoc           = round(cv(desoc, JAN), 1),
  cv_taxa            = round(cv(taxa, JAN), 1),
  stringsAsFactors = FALSE, row.names = NULL)

mg <- data.frame(
  estrato            = "Minas Gerais",
  ocupados_2024      = round(mean(ocup_mg[ANO]), 1),
  part_ocup_mg_2024  = 100,
  desocupados_2024   = round(mean(desoc_mg[ANO]), 1),
  taxa_2024          = round(100 * mean(desoc_mg[ANO]) / (mean(desoc_mg[ANO]) + mean(ocup_mg[ANO])), 1),
  taxa_media_periodo = round(100 * mean(desoc_mg[JAN]) / (mean(desoc_mg[JAN]) + mean(ocup_mg[JAN])), 1),
  cv_ocup = NA, cv_desoc = NA, cv_taxa = NA,
  stringsAsFactors = FALSE, row.names = NULL)

perfil <- rbind(perfil, mg)
write.csv(perfil, file.path(SAIDA, "perfil_estratos.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

options(width = 170)
cat("\n############ PERFIL DOS ESTRATOS — parte PNAD Contínua ############\n")
cat("Níveis em mil pessoas, média dos quatro trimestres de 2024.\n")
cat("CV médio da estimativa direta entre 2014Q1 e 2024Q4, em %.\n\n")
print(perfil, row.names = FALSE)
cat("\ngravado:", file.path("outputs", "perfil_estratos", "perfil_estratos.csv"), "\n")
