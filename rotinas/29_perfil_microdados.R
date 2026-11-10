################################################################################
## 29_perfil_microdados.R   — PASSO 9 do pipeline de revisão
##
## As colunas do perfil dos estratos (Seção 2) que exigem os microdados da PNAD
## Contínua: população residente e rendimento médio mensal habitual. Lê os
## arquivos de microdados trimestrais direto do texto de largura fixa, com as
## posições do dicionário do IBGE, e carrega apenas as sete variáveis
## necessárias -- o arquivo de um trimestre tem 1,7 GB e não cabe inteiro na
## memória desta máquina.
##
## Os estratos do SIPD são agrupados nas oito regiões do artigo exatamente como
## em `12_Base PNADC 8reg.R`; o trecho mineiro da RIDE de Brasília (3140010 e
## 3140020) entra no Norte de Minas e o Colar soma-se ao Entorno.
##
## Uso:  PNADC_TXT=<pasta com os .txt> Rscript rotinas/29_perfil_microdados.R
## Saída: outputs/perfil_estratos/perfil_microdados.csv
################################################################################
suppressMessages({library(readr); library(dplyr)})

RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
if (!dir.exists(file.path(RAIZ, "pseudoerros_8reg")) &&
    dir.exists(file.path(dirname(RAIZ), "pseudoerros_8reg"))) RAIZ <- dirname(RAIZ)
DIR  <- Sys.getenv("PNADC_TXT", unset = "C:/Dados/PNADC/2024/txt")
SAIDA <- file.path(RAIZ, "outputs", "perfil_estratos")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

## posições do input_PNADC_trimestral.txt (data/documentacao/)
POS <- fwf_positions(
  start = c(1,   5,   6,   21,  50,  410, 444),
  end   = c(4,   5,   7,   27,  64,  410, 451),
  col_names = c("ano","trimestre","uf","estrato","peso","vd4002","vd4019"))
TIPOS <- cols(ano = col_character(), trimestre = col_character(),
              uf = col_character(), estrato = col_character(),
              peso = col_double(), vd4002 = col_character(),
              vd4019 = col_double())

regiao <- function(e) dplyr::case_when(
  e %in% c("3110213","3110113","3110112","3110212","3110111","3110211") ~ "01 - Belo Horizonte",
  e %in% c("3120011","3120013","3120020","3120012","3130011","3130012","3130020") ~ "02 - Colar e Entorno Metropolitano de BH",
  e %in% c("3151011","3151012","3151013","3151021","3151022","3151023") ~ "03 - Sul de Minas",
  e %in% c("3152011","3152012","3152013","3152021","3152022") ~ "04 - Triângulo Mineiro",
  e %in% c("3153011","3153012","3153013","3153021","3153022","3153023") ~ "05 - Zona da Mata",
  e %in% c("3154011","3154012","3154013","3154021","3154022","3154023","3140010","3140020") ~ "06 - Norte de Minas",
  e %in% c("3155011","3155012","3155013","3155021","3155022","3155023") ~ "07 - Vale do Rio Doce",
  e %in% c("3156011","3156012","3156013","3156021","3156022") ~ "08 - Central",
  TRUE ~ NA_character_)

arqs <- list.files(DIR, pattern = "^PNADC_[0-9]{6}[.]txt$", full.names = TRUE)
stopifnot(length(arqs) > 0)
cat("microdados:", length(arqs), "trimestres em", DIR, "\n")

le <- function(arq) {
  cat("  lendo", basename(arq), "... "); flush.console()
  d <- read_fwf(arq, POS, col_types = TIPOS, progress = FALSE) |>
       filter(uf == "31") |>
       mutate(regiao = regiao(estrato))
  cat(nrow(d), "pessoas em MG\n")
  d
}
mg <- bind_rows(lapply(arqs, le))
stopifnot(!any(is.na(mg$regiao)))        # nenhum estrato de MG fora do agrupamento

## Tudo o que a tabela de perfil mostra como nivel ou razao sai DAQUI, de uma
## fonte so: populacao, ocupados, desocupados, taxa e rendimento, todos dos
## mesmos microdados de 2024. Antes, ocupacao e taxa vinham dos .rds do artigo,
## cujos pesos antecedem a revisao do IBGE, e a tabela misturava dois vintages
## -- os ocupados nao fechavam entre as colunas. Desocupados nao custam leitura
## nova: e a mesma VD4002, categoria 2.
por_tri <- mg |>
  group_by(trimestre, regiao) |>
  summarise(pop = sum(peso),
            ocup = sum(peso[vd4002 == "1"], na.rm = TRUE),
            desoc = sum(peso[vd4002 == "2"], na.rm = TRUE),
            rend = weighted.mean(vd4019[vd4002 == "1" & !is.na(vd4019)],
                                 peso[vd4002 == "1" & !is.na(vd4019)]),
            .groups = "drop")

perfil <- por_tri |>
  group_by(regiao) |>
  summarise(populacao_mil = round(mean(pop)/1000, 1),
            ocupados_mil  = round(mean(ocup)/1000, 1),
            desocupados_mil = round(mean(desoc)/1000, 1),
            rendimento_medio = round(mean(rend), 0), .groups = "drop") |>
  mutate(part_pop_mg  = round(100*populacao_mil/sum(populacao_mil), 1),
         part_ocup_mg = round(100*ocupados_mil/sum(ocupados_mil), 1),
         taxa         = round(100*desocupados_mil/(desocupados_mil + ocupados_mil), 1)) |>
  relocate(part_pop_mg, .after = populacao_mil) |>
  relocate(part_ocup_mg, .after = ocupados_mil)

mgtot <- por_tri |> group_by(trimestre) |>
  summarise(pop = sum(pop), ocup = sum(ocup), desoc = sum(desoc), .groups = "drop")
rend_mg <- mg |> filter(vd4002 == "1", !is.na(vd4019)) |>
  summarise(r = weighted.mean(vd4019, peso)) |> pull(r)

perfil <- bind_rows(perfil, data.frame(
  regiao = "Minas Gerais",
  populacao_mil = round(mean(mgtot$pop)/1000, 1), part_pop_mg = 100,
  ocupados_mil = round(mean(mgtot$ocup)/1000, 1), part_ocup_mg = 100,
  desocupados_mil = round(mean(mgtot$desoc)/1000, 1),
  taxa = round(100*mean(mgtot$desoc)/(mean(mgtot$desoc) + mean(mgtot$ocup)), 1),
  rendimento_medio = round(rend_mg, 0)))

write.csv(perfil, file.path(SAIDA, "perfil_microdados.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
options(width = 150)
cat("\n####### PERFIL — microdados da PNAD Contínua, média dos 4 trimestres de 2024 #######\n")
cat("População e ocupados em mil pessoas; rendimento mensal habitual de todos os trabalhos, em R$ correntes.\n\n")
print(as.data.frame(perfil), row.names = FALSE)
cat("\ngravado:", file.path("outputs","perfil_estratos","perfil_microdados.csv"), "\n")
