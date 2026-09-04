################################################################################
## 03 - ORGANIZAÇÃO DAS BASES E FIGURAS/TABELAS DE COEFICIENTE DE VARIAÇÃO
## Versão revisada/limpa. Consolida:
##   - "3_Organizacao e leitura base PNADC RDS.R" (10 estratos originais)
##   - "13_Organizacao Base 8reg.R"               (8 regiões de análise)
##
## Para CADA recorte, a partir das estimativas trimestrais (script 02):
##   1) monta o base de séries por região (baseestr0424 / baseestr8reg -> data/);
##   2) gera as figuras de CV (desocupados, ocupados, taxa) no padrão do artigo,
##      com linhas de referência de 15% e 30% (limiar de alta imprecisão do IBGE);
##      -> a de desocupados/10 estratos é a Figura 2 do artigo;
##   3) gera a tabela de CV médio por estrato (a de 8 regiões = Tabela 1 do artigo).
##
## Correções: caminhos relativos; janelas "últimos N anos" calculadas do tamanho
## da série (antes eram índices fixos 33:51/45:51); no script 13 original a seção
## descritiva referenciava objetos de 10 regiões inexistentes (aqui reescrita).
################################################################################
suppressMessages({ library(dplyr); library(writexl) })
source("rotinas/00_tema_graficos.R")

## ============================ CONFIGURAÇÃO ====================================
TRI_FINAL <- Sys.getenv("TRI_FINAL", unset = "2025_02")  # artigo: TRI_FINAL=2024_04
dir_data  <- "data"
dir_tab   <- "outputs/cv/tabelas"
dir_fig   <- "outputs/cv/figuras"
dir.create(dir_tab, recursive = TRUE, showWarnings = FALSE)
dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)

NOMES10 <- c("01-Belo Horizonte","02-Entorno metropolitano de BH","03-Colar metropolitano de BH",
             "04-RIDE de Brasília em Minas","05-Sul de Minas","06-Triângulo Mineiro",
             "07-Mata de Minas Gerais","08-Norte de Minas","09-Vale do Rio Doce",
             "10-Central","11 - Minas Gerais")
NOMES8  <- c("01-Belo Horizonte","02-Colar e Entorno metropolitano de BH","03-Sul de Minas",
             "04-Triângulo Mineiro","05-Mata de Minas Gerais","06-Norte de Minas",
             "07-Vale do Rio Doce","08-Central","09 - Minas Gerais")

## ============================ FUNÇÕES =========================================
monta_base <- function(dir_estim, nomes) {
  comb <- bind_rows(lapply(list.files(dir_estim, "\\.RDS$", full.names = TRUE, ignore.case = TRUE), readRDS))
  comb <- comb %>% filter(periodo <= TRI_FINAL)
  base <- lapply(sprintf("%02d", seq_along(nomes)), function(code) {
    d <- comb[substr(comb$regioes, 1, 2) == code, ]; d <- d[order(d$periodo), ]
    data.frame(check.names = FALSE,
      "Período"              = d$periodo,
      "Total.de.ocupados"    = d$ocupada,    "sd_o" = d$se_o,
      "CV.ocupados"          = d$se_o / d$ocupada * 100,
      "Total.de.desocupados" = d$desocupada, "sd_d" = d$se_d,
      "CV.desocupados"       = d$se_d / d$desocupada * 100,
      "Taxa.de.desocupação"  = d$tx_desocupada, "sd_txd" = d$se_td,
      "CV.taxa"              = d$se_td / d$tx_desocupada * 100)
  })
  names(base) <- nomes; base
}

produz_cv <- function(base, nomes, mg_nome, esq_suf, esq_label) {
  regs <- setdiff(nomes, mg_nome)
  per  <- base[[mg_nome]][["Período"]]
  .fim <- max(per)
  SUB  <- sprintf("PNAD Contínua trimestral, 1º tri 2012 – %sº tri %s", substr(.fim,7,7), substr(.fim,1,4))
  long_cv <- function(col) do.call(rbind, lapply(regs, function(r)
    data.frame(data = periodo_para_data(per), grupo = r, valor = base[[r]][[col]])))
  cols <- c(desocupados="CV.desocupados", ocupados="CV.ocupados", taxa="CV.taxa")
  tits <- c(desocupados="total de desocupados", ocupados="total de ocupados", taxa="taxa de desocupação")
  for (k in names(cols)) {
    ## sem titulo/subtitulo/fonte dentro da imagem: o LaTeX ja traz \caption{} e \fonte{}
    g <- grafico_linha(long_cv(cols[[k]]),
           titulo = NULL, subtitulo = NULL, fonte = NULL,
           ylab = "Coeficiente de variação (%)", hlines = c(15, 30))
    ggsave(file.path(dir_fig, paste0("cv_", k, "_", esq_suf, ".png")), g, width = 10, height = 5.6, dpi = 135, bg = "white")
  }
  ## tabela de CV médio (janelas calculadas do tamanho da série)
  n <- length(per)
  jan <- list("serie completa" = 1:n, "ultimos 4 anos" = max(1,n-15):n, "ultimos 2 anos" = max(1,n-7):n)
  cvmed <- bind_rows(lapply(names(jan), function(j) bind_rows(lapply(nomes, function(r) {
    idx <- jan[[j]]
    data.frame(janela = j, estrato = r,
               CV.ocupados    = mean(base[[r]][["CV.ocupados"]][idx],    na.rm = TRUE),
               CV.desocupados = mean(base[[r]][["CV.desocupados"]][idx], na.rm = TRUE),
               CV.taxa        = mean(base[[r]][["CV.taxa"]][idx],        na.rm = TRUE)) }))))
  cvmed[ ,3:5] <- round(cvmed[ ,3:5], 2)
  write_xlsx(cvmed, file.path(dir_tab, paste0("cv_medio_", esq_suf, ".xlsx")))
}

## ============================ EXECUÇÃO (10reg e 8reg) =========================
base10 <- monta_base("data/estimativas", NOMES10)
saveRDS(base10, file.path(dir_data, "baseestr0424.rds"))
produz_cv(base10, NOMES10, "11 - Minas Gerais", "10reg", "dez estratos originais")

base8  <- monta_base("data/pnad8reg", NOMES8)
saveRDS(base8,  file.path(dir_data, "baseestr8reg.rds"))
produz_cv(base8, NOMES8, "09 - Minas Gerais", "8reg", "oito regiões de análise")

message("Concluído. bases -> ", dir_data, "/{baseestr0424,baseestr8reg}.rds | figuras -> ", dir_fig, " | tabelas -> ", dir_tab)
