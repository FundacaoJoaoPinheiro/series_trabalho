################################################################################
## 01 - AMOSTRA EFETIVA (REALIZADA) DE PESSOAS E DOMICÍLIOS POR ESTRATO
## Versão revisada/limpa (revisão de código, 2026).
## Substitui "1_Evolucao da amostra 2012-2023.R".
##
## O que faz: para cada trimestre da PNADc (2012T1..2024T4), conta o número de
## PESSOAS e de DOMICÍLIOS efetivamente entrevistados por estrato geográfico, nos
## DOIS recortes — 10 estratos originais (SIPD) e 8 regiões de análise (agregadas)
## — e gera tabelas (.xlsx) e figuras (.png), incluindo o total de Minas Gerais.
##
## "Amostra efetiva" = amostra REALIZADA (domicílios/pessoas de fato entrevistados,
## contados nos microdados), em oposição à amostra planejada. NÃO é o "tamanho
## efetivo" ajustado pelo efeito de desenho (n/deff) — este seria outro cálculo.
## A contagem é independente do vintage dos pesos (o IBGE revisa pesos, não quem
## foi entrevistado).
################################################################################

suppressMessages({
  library(PNADcIBGE); library(dplyr); library(tidyr); library(writexl)
})

## ======================= CONFIGURAÇÃO (ajuste os caminhos) ====================
## Pasta com os microdados: subpastas por ano (PNADC_0QYYYY_*.zip) e
## documentacao/input_PNADC_trimestral.txt. Os zips ficam FORA do repositório
## (são grandes); aqui num disco local.
dir_zips <- "E:/Dados/PNADC"
input     <- file.path(dir_zips, "documentacao", "input_PNADC_trimestral.txt")
anos      <- 2012:2025
## Último trimestre incluído. A partir de 2025Q3 o IBGE implanta a NOVA amostra
## mestra (pós-Censo 2022) faseada por painel, com códigos de estrato novos ainda
## não mapeados aqui; por isso a série vai até 2025Q2 (mesmo desenho amostral).
TRI_FINAL <- "2025_02"
## Saídas (relativas à raiz do repositório):
dir_tab  <- "outputs/amostra/tabelas"
dir_fig  <- "outputs/amostra/figuras"
dir.create(dir_tab, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)

## ======================= RECORTES REGIONAIS ===================================
reg10 <- function(e) case_when(
  e %in% c("3110213","3110113","3110112","3110212","3110111","3110211") ~ "01-Belo Horizonte",
  e %in% c("3120011","3120013","3120020","3120012") ~ "02-Entorno metropolitano de BH",
  e %in% c("3130011","3130012","3130020") ~ "03-Colar metropolitano de BH",
  e %in% c("3140010","3140020") ~ "04-RIDE de Brasília em Minas",
  e %in% c("3151011","3151012","3151013","3151021","3151022","3151023") ~ "05-Sul de Minas",
  e %in% c("3152011","3152012","3152013","3152021","3152022") ~ "06-Triângulo Mineiro",
  e %in% c("3153011","3153012","3153013","3153021","3153022","3153023") ~ "07-Mata de Minas Gerais",
  e %in% c("3154011","3154012","3154013","3154021","3154022","3154023") ~ "08-Norte de Minas",
  e %in% c("3155011","3155012","3155013","3155021","3155022","3155023") ~ "09-Vale do Rio Doce",
  e %in% c("3156011","3156012","3156013","3156021","3156022") ~ "10-Central",
  TRUE ~ "11-Minas Gerais")

reg8 <- function(e) case_when(
  e %in% c("3110213","3110113","3110112","3110212","3110111","3110211") ~ "01-Belo Horizonte",
  e %in% c("3120011","3120013","3120020","3120012","3130011","3130012","3130020") ~ "02-Colar e Entorno Metropolitano de BH",
  e %in% c("3151011","3151012","3151013","3151021","3151022","3151023") ~ "03-Sul de Minas",
  e %in% c("3152011","3152012","3152013","3152021","3152022") ~ "04-Triângulo Mineiro",
  e %in% c("3153011","3153012","3153013","3153021","3153022","3153023") ~ "05-Mata de Minas Gerais",
  e %in% c("3154011","3154012","3154013","3154021","3154022","3154023","3140010","3140020") ~ "06-Norte de Minas",
  e %in% c("3155011","3155012","3155013","3155021","3155022","3155023") ~ "07-Vale do Rio Doce",
  e %in% c("3156011","3156012","3156013","3156021","3156022") ~ "08-Central",
  TRUE ~ "09-Minas Gerais")

## ======================= CONTAGEM POR TRIMESTRE ===============================
## Descompacta o zip num diretório temporário, lê o .txt, conta e apaga o temp
## (não acumula ~1,9 GB por trimestre).
conta_tri <- function(zip) {
  base   <- sub("_\\d{8}$", "", sub("\\.zip$", "", basename(zip)))  # trata os 2 padrões:
                                                       # PNADC_012012_20250815.zip e PNADC_012026.zip
  mesano <- sub("^PNADC_0", "", base)                  # 12012 (Q + YYYY)
  per    <- paste0(substr(mesano, 2, 5), "_0", substr(mesano, 1, 1))
  td <- file.path(tempdir(), paste0("x_", mesano))
  unzip(zip, exdir = td)
  txt <- list.files(td, pattern = "\\.txt$", recursive = TRUE, full.names = TRUE)
  txt <- txt[which.max(file.size(txt))]                # o microdado é o maior .txt
  df <- read_pnadc(txt, input, vars = c("V2005", "Estrato", "UF"))
  df <- df[df$UF == "31", ]
  df$dom <- as.integer(as.character(df$V2005) == "01") # 1 = domicílio (pessoa responsável)
  unlink(td, recursive = TRUE)
  um <- function(fun, esq) {
    d <- df; d$regiao <- fun(as.character(d$Estrato))
    r <- d %>% group_by(regiao) %>% summarise(t.pes = n(), t.dom = sum(dom), .groups = "drop")
    bind_rows(r, data.frame(regiao = "Total MG", t.pes = nrow(d), t.dom = sum(d$dom))) %>%
      mutate(periodo = per, esquema = esq)
  }
  message("  ok ", mesano)
  bind_rows(um(reg10, "10reg"), um(reg8, "8reg"))
}

## Lista dos zips (um por trimestre), em ordem:
zips <- unlist(lapply(anos, function(a)
  list.files(file.path(dir_zips, a), pattern = sprintf("^PNADC_0[1-4]%d.*\\.zip$", a),
             full.names = TRUE)))
## mantém só os trimestres até TRI_FINAL
per_de <- function(z) { m <- sub("^PNADC_0", "", sub("_\\d{8}$", "", sub("\\.zip$", "", basename(z))))
                        paste0(substr(m, 2, 5), "_0", substr(m, 1, 1)) }
zips <- zips[vapply(zips, per_de, "") <= TRI_FINAL]
message("Trimestres (até ", TRI_FINAL, "): ", length(zips))
comb <- bind_rows(lapply(zips, conta_tri))

## ======================= TABELAS (largo) ======================================
largo <- function(esq, metrica) {
  comb %>% filter(esquema == esq) %>% arrange(periodo) %>%
    select(periodo, regiao, all_of(metrica)) %>%
    pivot_wider(names_from = regiao, values_from = all_of(metrica)) %>% arrange(periodo)
}

## ======================= TABELAS (.xlsx) ======================================
for (esq in c("10reg", "8reg")) {
  write_xlsx(largo(esq, "t.pes"), file.path(dir_tab, paste0("tam_pessoas_",    esq, ".xlsx")))
  write_xlsx(largo(esq, "t.dom"), file.path(dir_tab, paste0("tam_domicilios_", esq, ".xlsx")))
}

## ======================= FIGURAS (padrão do artigo) ===========================
source("rotinas/00_tema_graficos.R")            # tema_artigo(), grafico_linha(), PAL_ARTIGO
comb$data <- periodo_para_data(comb$periodo)
.ini <- min(comb$periodo); .fim <- max(comb$periodo)   # subtítulo dinâmico
SUB  <- sprintf("PNAD Contínua trimestral, %sº tri %s – %sº tri %s",
                substr(.ini,7,7), substr(.ini,1,4), substr(.fim,7,7), substr(.fim,1,4))
rot <- c("10reg" = "dez estratos originais", "8reg" = "oito regiões de análise")

fig <- function(esq, metrica, ylab, titulo, arq) {
  d <- comb %>% filter(esquema == esq, regiao != "Total MG") %>%
       transmute(data, grupo = factor(regiao), valor = .data[[metrica]])
  ggsave(file.path(dir_fig, arq),
         grafico_linha(d, titulo = titulo, subtitulo = SUB, ylab = ylab),
         width = 10, height = 5.6, dpi = 135, bg = "white")
}
for (esq in c("10reg", "8reg")) {
  fig(esq, "t.dom", "Domicílios entrevistados",
      paste0("Amostra efetiva de domicílios — ", rot[esq]), paste0("amostra_domicilios_", esq, ".png"))
  fig(esq, "t.pes", "Pessoas entrevistadas",
      paste0("Amostra efetiva de pessoas — ",    rot[esq]), paste0("amostra_pessoas_",    esq, ".png"))
}
## Total de Minas Gerais (série única)
mg <- comb %>% filter(esquema == "10reg", regiao == "Total MG")
for (m in c("t.dom", "t.pes")) {
  lab <- if (m == "t.dom") "Domicílios entrevistados" else "Pessoas entrevistadas"
  suf <- if (m == "t.dom") "domicilios"              else "pessoas"
  d <- mg %>% transmute(data, grupo = "Total MG", valor = .data[[m]])
  ggsave(file.path(dir_fig, paste0("amostra_MG_", suf, ".png")),
         grafico_linha(d, titulo = "Amostra efetiva — Total de Minas Gerais", subtitulo = SUB, ylab = lab),
         width = 10, height = 4.8, dpi = 135, bg = "white")
}

message("Concluído. Tabelas em ", dir_tab, " | Figuras em ", dir_fig)
