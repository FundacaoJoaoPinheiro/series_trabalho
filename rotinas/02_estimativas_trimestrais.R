################################################################################
## 02 - ESTIMATIVAS TRIMESTRAIS DIRETAS (ocupados, desocupados, taxa) por estrato
## Versão revisada/limpa. Consolida num único passe (uma leitura por trimestre):
##   - "2_Criacao base PNADC RDS.R"  -> 10 estratos originais  -> data/estimativas/
##   - "12_Base PNADC 8reg.R"        -> 8 regiões de análise   -> data/pnad8reg/
##
## Correções: caminhos relativos (era D:/C:/ absolutos); o script 12 gravava na
## pasta errada (data/estimativas); GUARDA contra estratos de MG não mapeados
## (que geravam catch-all + linha MG duplicada). Série 2012Q1–2025Q2 (amostra
## mestra antiga; 2025Q3+ = transição, fora do escopo aqui).
##
## Estimação design-based (pacote survey): total de ocupados/desocupados por
## svytotal; taxa de desocupação (desocupados/força de trabalho) por svymean.
################################################################################
suppressMessages({ library(PNADcIBGE); library(survey); library(dplyr); library(tibble) })
options(scipen = 999)

## ============================ CONFIGURAÇÃO ====================================
dir_zips  <- "E:/Dados/PNADC"     # zips por ano + documentacao/input_PNADC_trimestral.txt
input     <- file.path(dir_zips, "documentacao", "input_PNADC_trimestral.txt")
anos      <- 2012:2025
TRI_FINAL <- "2025_02"            # último trimestre (amostra mestra antiga)
dir_10    <- "data/estimativas"   # 10 estratos originais
dir_8     <- "data/pnad8reg"      # 8 regiões de análise
dir.create(dir_10, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_8,  recursive = TRUE, showWarnings = FALSE)

## ============================ RECORTES REGIONAIS ==============================
reg10 <- function(e) case_when(
  e %in% c("3110213","3110113","3110112","3110212","3110111","3110211") ~ "01-Belo Horizonte",
  e %in% c("3120011","3120013","3120020","3120012") ~ "02-Entorno metropolitono de BH",
  e %in% c("3130011","3130012","3130020") ~ "03-Colar metropolitano de BH",
  e %in% c("3140010","3140020") ~ "04-RIDE de Brasília em Minas",
  e %in% c("3151011","3151012","3151013","3151021","3151022","3151023") ~ "05-Sul de Minas",
  e %in% c("3152011","3152012","3152013","3152021","3152022") ~ "06-Triângulo Mineiro",
  e %in% c("3153011","3153012","3153013","3153021","3153022","3153023") ~ "07-Mata de Minas Gerais",
  e %in% c("3154011","3154012","3154013","3154021","3154022","3154023") ~ "08-Norte de Minas",
  e %in% c("3155011","3155012","3155013","3155021","3155022","3155023") ~ "09-Vale do Rio Doce",
  e %in% c("3156011","3156012","3156013","3156021","3156022") ~ "10-Central",
  TRUE ~ "99-NAO MAPEADO")

reg8 <- function(e) case_when(
  e %in% c("3110213","3110113","3110112","3110212","3110111","3110211") ~ "01-Belo Horizonte",
  e %in% c("3120011","3120013","3120020","3120012","3130011","3130012","3130020") ~ "02-Colar e Entorno Metropolitano de BH",
  e %in% c("3151011","3151012","3151013","3151021","3151022","3151023") ~ "03-Sul de Minas",
  e %in% c("3152011","3152012","3152013","3152021","3152022") ~ "04-Triângulo Mineiro",
  e %in% c("3153011","3153012","3153013","3153021","3153022","3153023") ~ "05-Mata de Minas Gerais",
  e %in% c("3154011","3154012","3154013","3154021","3154022","3154023","3140010","3140020") ~ "06-Norte de Minas",
  e %in% c("3155011","3155012","3155013","3155021","3155022","3155023") ~ "07-Vale do Rio Doce",
  e %in% c("3156011","3156012","3156013","3156021","3156022") ~ "08-Central",
  TRUE ~ "99-NAO MAPEADO")

## conjunto dos estratos de MG mapeados (para a guarda)
ESTRATOS_MG <- c("3110213","3110113","3110112","3110212","3110111","3110211",
  "3120011","3120013","3120020","3120012","3130011","3130012","3130020",
  "3140010","3140020",
  "3151011","3151012","3151013","3151021","3151022","3151023",
  "3152011","3152012","3152013","3152021","3152022",
  "3153011","3153012","3153013","3153021","3153022","3153023",
  "3154011","3154012","3154013","3154021","3154022","3154023",
  "3155011","3155012","3155013","3155021","3155022","3155023",
  "3156011","3156012","3156013","3156021","3156022")

## ============================ ESTIMAÇÃO =======================================
estima <- function(des, mg_label, per) {
  e  <- svyby(~ocupada,    ~regioes, subset(des, UF == "31"), na.rm = TRUE, svytotal); colnames(e)[3]  <- "se_o"
  d  <- svyby(~desocupada, ~regioes, subset(des, UF == "31"), na.rm = TRUE, svytotal); colnames(d)[3]  <- "se_d"
  td <- svyby(~desocupada, ~regioes, subset(des, UF == "31"), na.rm = TRUE, svymean);  colnames(td)[2] <- "tx_desocupada"; colnames(td)[3] <- "se_td"
  to  <- svytotal(~ocupada,    subset(des, UF == "31"), na.rm = TRUE)
  tdd <- svytotal(~desocupada, subset(des, UF == "31"), na.rm = TRUE)
  ttx <- svymean (~desocupada, subset(des, UF == "31"), na.rm = TRUE)
  mg <- tibble(regioes = mg_label,
               ocupada = coef(to),  se_o = SE(to),
               desocupada = coef(tdd), se_d = SE(tdd),
               tx_desocupada = coef(ttx), se_td = SE(ttx), periodo = per)
  e %>% left_join(d, by = "regioes") %>% left_join(td, by = "regioes") %>%
    mutate(periodo = per) %>% bind_rows(mg)
}

processa <- function(zip) {
  base   <- sub("_\\d{8}$", "", sub("\\.zip$", "", basename(zip)))
  mesano <- sub("^PNADC_0", "", base)
  per    <- paste0(substr(mesano, 2, 5), "_0", substr(mesano, 1, 1))
  o10 <- file.path(dir_10, paste0("resultados_0", mesano, ".RDS"))
  o8  <- file.path(dir_8,  paste0("resultados_0", mesano, ".RDS"))
  if (file.exists(o10) && file.exists(o8)) { message("skip ", mesano); return(invisible()) }
  td <- file.path(tempdir(), paste0("x_", mesano)); unzip(zip, exdir = td)
  txt <- list.files(td, "[.]txt$", recursive = TRUE, full.names = TRUE)
  txt <- txt[which.max(file.size(txt))]
  des0 <- pnadc_design(read_pnadc(txt, input, vars = c("VD4002"))) %>%
          update(ocupada = 1 * (VD4002 == 1), desocupada = 1 * (VD4002 == 2))
  unlink(td, recursive = TRUE)
  ## GUARDA: nenhum estrato de MG pode ficar fora do mapa (senão catch-all -> MG duplicado)
  estr <- unique(as.character(des0$variables$Estrato[des0$variables$UF == "31"]))
  fora <- setdiff(estr, ESTRATOS_MG)
  if (length(fora)) stop(sprintf("Estratos de MG NÃO mapeados em %s: %s", per, paste(fora, collapse = ", ")))
  des10 <- update(des0, regioes = reg10(as.character(Estrato)))
  des8  <- update(des0, regioes = reg8(as.character(Estrato)))
  saveRDS(estima(des10, "11 - Minas Gerais", per), o10)
  saveRDS(estima(des8,  "09 - Minas Gerais", per), o8)
  message("ok ", mesano)
}

## ============================ LOOP ============================================
zips <- unlist(lapply(anos, function(a)
  list.files(file.path(dir_zips, a), pattern = sprintf("^PNADC_0[1-4]%d.*\\.zip$", a), full.names = TRUE)))
per_de <- function(z) { m <- sub("^PNADC_0", "", sub("_\\d{8}$", "", sub("\\.zip$", "", basename(z))))
                        paste0(substr(m, 2, 5), "_0", substr(m, 1, 1)) }
zips <- zips[vapply(zips, per_de, "") <= TRI_FINAL]
message("Trimestres (até ", TRI_FINAL, "): ", length(zips))
invisible(lapply(zips, processa))
message("Concluído. 10 estratos em ", dir_10, " | 8 regiões em ", dir_8)
