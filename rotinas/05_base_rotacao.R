################################################################################
## 05 - BASE POR GRUPO DE ROTAÇÃO (alinhada) — insumo dos pseudo-erros
## Versão revisada/limpa. Consolida "5_Base PNADC ... por rotacao.R" (10 estratos)
## e "14_Base Rotacao 8reg.R" (8 regiões) num único passe.
##
## Por trimestre estima ocupados/desocupados por REGIÃO × GRUPO DE ROTAÇÃO (V1016)
## e o total de MG por rotação; reorganiza para o formato largo (ocupada_1..5, ...);
## e aplica o ALINHAMENTO DIAGONAL (organiza_base) que rastreia a coorte de entrada
## ao longo das 5 entrevistas → base "alinhada" que alimenta os pseudo-erros (06/15).
##
## Correções: caminhos relativos; MG por rotação nos DOIS recortes (o script 5
## original não fazia p/ 10reg); o reshape+gsub frágil (podia gerar NA silencioso)
## foi trocado por pivot_wider robusto; guarda contra estrato não mapeado.
## Série 2012Q1–2025Q2.
################################################################################
suppressMessages({ library(PNADcIBGE); library(survey); library(dplyr); library(tidyr) })
options(scipen = 999)

## ============================ CONFIGURAÇÃO ====================================
dir_zips  <- "E:/Dados/PNADC"
input     <- file.path(dir_zips, "documentacao", "input_PNADC_trimestral.txt")
anos      <- 2012:2025
TRI_FINAL <- "2025_02"
dir_rot10 <- "data/rotacao"        # per-trimestre, 10 estratos
dir_rot8  <- "data/rotacao8reg"    # per-trimestre, 8 regiões
dir_data  <- "data"
dir.create(dir_rot10, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_rot8,  recursive = TRUE, showWarnings = FALSE)

## ============================ RECORTES + GUARDA ===============================
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
ESTRATOS_MG <- c("3110213","3110113","3110112","3110212","3110111","3110211",
  "3120011","3120013","3120020","3120012","3130011","3130012","3130020","3140010","3140020",
  "3151011","3151012","3151013","3151021","3151022","3151023","3152011","3152012","3152013","3152021","3152022",
  "3153011","3153012","3153013","3153021","3153022","3153023","3154011","3154012","3154013","3154021","3154022","3154023",
  "3155011","3155012","3155013","3155021","3155022","3155023","3156011","3156012","3156013","3156021","3156022")

## Estima por região×V1016 + MG por V1016 (rotação)
estima_rot <- function(des, mg_label, per) {
  e <- svyby(~ocupada + desocupada, ~regioes + V1016, subset(des, UF == "31"), svytotal, na.rm = TRUE)
  colnames(e)[3:6] <- c("ocupada","desocupada","se_ocupada","se_desocupada")
  mg <- svyby(~ocupada + desocupada, ~V1016, subset(des, UF == "31"), svytotal, na.rm = TRUE)
  colnames(mg)[2:5] <- c("ocupada","desocupada","se_ocupada","se_desocupada")
  mg$regioes <- mg_label
  rbind(e, mg[, names(e)]) %>% mutate(periodo = per) %>% arrange(regioes, V1016)
}

processa <- function(zip) {
  base   <- sub("_\\d{8}$", "", sub("\\.zip$", "", basename(zip)))
  mesano <- sub("^PNADC_0", "", base)
  per    <- paste0(substr(mesano, 2, 5), "_0", substr(mesano, 1, 1))
  o10 <- file.path(dir_rot10, paste0("resultados_0", mesano, ".RDS"))
  o8  <- file.path(dir_rot8,  paste0("resultados_0", mesano, ".RDS"))
  if (file.exists(o10) && file.exists(o8)) { message("skip ", mesano); return(invisible()) }
  td <- file.path(tempdir(), paste0("r_", mesano)); unzip(zip, exdir = td)
  txt <- list.files(td, "[.]txt$", recursive = TRUE, full.names = TRUE); txt <- txt[which.max(file.size(txt))]
  des0 <- pnadc_design(read_pnadc(txt, input, vars = c("V1016", "VD4002"))) %>%
          update(ocupada = 1 * (VD4002 == 1), desocupada = 1 * (VD4002 == 2))
  unlink(td, recursive = TRUE)
  fora <- setdiff(unique(as.character(des0$variables$Estrato[des0$variables$UF == "31"])), ESTRATOS_MG)
  if (length(fora)) stop(sprintf("Estratos MG não mapeados em %s: %s", per, paste(fora, collapse = ", ")))
  saveRDS(estima_rot(update(des0, regioes = reg10(as.character(Estrato))), "11 - Minas Gerais", per), o10)
  saveRDS(estima_rot(update(des0, regioes = reg8 (as.character(Estrato))), "09 - Minas Gerais", per), o8)
  message("ok ", mesano)
}

## ============================ LOOP (estimação por trimestre) ==================
zips <- unlist(lapply(anos, function(a)
  list.files(file.path(dir_zips, a), pattern = sprintf("^PNADC_0[1-4]%d.*\\.zip$", a), full.names = TRUE)))
per_de <- function(z) { m <- sub("^PNADC_0", "", sub("_\\d{8}$", "", sub("\\.zip$", "", basename(z))))
                        paste0(substr(m, 2, 5), "_0", substr(m, 1, 1)) }
zips <- zips[vapply(zips, per_de, "") <= TRI_FINAL]
message("Trimestres (até ", TRI_FINAL, "): ", length(zips))
invisible(lapply(zips, processa))

## ============================ FUNORG (formato largo, robusto) =================
NOMES10 <- c("01-Belo Horizonte","02-Entorno metropolitono de BH","03-Colar metropolitano de BH",
  "04-RIDE de Brasília em Minas","05-Sul de Minas","06-Triângulo Mineiro","07-Mata de Minas Gerais",
  "08-Norte de Minas","09-Vale do Rio Doce","10-Central","11 - Minas Gerais")
NOMES8  <- c("01-Belo Horizonte","02-Colar e Entorno Metropolitano de BH","03-Sul de Minas",
  "04-Triângulo Mineiro","05-Mata de Minas Gerais","06-Norte de Minas","07-Vale do Rio Doce",
  "08-Central","09 - Minas Gerais")
COLS <- c("periodo", paste0(rep(c("ocupada_","se_ocupada_","desocupada_","se_desocupada_"), each = 5), 1:5))

funorg <- function(dir_rot, nomes) {
  comb <- bind_rows(lapply(list.files(dir_rot, "\\.RDS$", full.names = TRUE, ignore.case = TRUE), readRDS))
  comb <- comb %>% filter(periodo <= TRI_FINAL)
  base <- lapply(nomes, function(rg) {
    d <- comb %>% filter(regioes == rg) %>%
      pivot_wider(id_cols = periodo, names_from = V1016,
                  values_from = c(ocupada, se_ocupada, desocupada, se_desocupada), names_sep = "_") %>%
      arrange(periodo)
    faltantes <- setdiff(COLS, names(d)); d[faltantes] <- NA
    as.data.frame(d[, COLS])
  })
  names(base) <- nomes; base
}

## ============================ ALINHAMENTO DIAGONAL ============================
organiza_base <- function(matriz, t) {           # matriz: T×5 (linhas=trimestres, cols=grupos)
  rep_n <- ceiling((t + 5) / 5) + 1
  org <- do.call(rbind, replicate(rep_n, diag(5), simplify = FALSE))
  cols <- lapply(1:5, function(k) rowSums(matriz * org[k:(t + k - 1), ]))
  do.call(cbind, cols)
}

alinha <- function(base, nomes, arq_baserot, arq_alinhada) {
  saveRDS(base, file.path(dir_data, arq_baserot))
  t <- nrow(base[[1]])
  bloco <- function(df, pref) as.matrix(df[, paste0(pref, 1:5)])
  al <- lapply(nomes, function(rg) {
    df <- base[[rg]]
    oc  <- organiza_base(bloco(df, "ocupada_"),      t); colnames(oc)  <- paste0("ocupada_", 1:5)
    seo <- organiza_base(bloco(df, "se_ocupada_"),   t); colnames(seo) <- paste0("se_ocupada_", 1:5)
    de  <- organiza_base(bloco(df, "desocupada_"),   t); colnames(de)  <- paste0("desocupada_", 1:5)
    sed <- organiza_base(bloco(df, "se_desocupada_"),t); colnames(sed) <- paste0("se_desocupada_", 1:5)
    data.frame(periodo = df$periodo, oc, seo, de, sed, check.names = FALSE)
  })
  names(al) <- nomes
  saveRDS(al, file.path(dir_data, arq_alinhada))
  al
}

## ============================ EXECUÇÃO ========================================
base10 <- funorg(dir_rot10, NOMES10); alinha(base10, NOMES10, "baserot0424.rds", "basealinhada0424.rds")
base8  <- funorg(dir_rot8,  NOMES8);  alinha(base8,  NOMES8,  "baserot8reg.rds", "basealinhada_8reg.rds")
message("Concluído. baserot/basealinhada (10reg e 8reg) em ", dir_data)
