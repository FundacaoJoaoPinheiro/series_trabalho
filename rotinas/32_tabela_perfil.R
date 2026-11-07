## 32_tabela_perfil.R
## Tabela de perfil dos oito estratos para a Secao 2 do artigo.
##
## Junta tres insumos ja produzidos:
##   30_correspondencia_municipios.R -> quais municipios compoem cada estrato
##   29_perfil_microdados.R          -> populacao e rendimento (PNADc 2024)
##   28_perfil_estratos.R            -> ocupacao, taxa e CV direto (base do artigo)
## mais a planilha do PIB dos Municipios (IBGE) para o porte economico, a
## atividade predominante e a cidade-polo.
##
## Nota de codificacao: o R no Windows le este arquivo em cp1252, entao os
## padroes de busca dos nomes de coluna sao ASCII e os acentos da saida entram
## por escape Unicode. Nao troque por texto acentuado literal.
##
## Saidas: outputs/perfil_estratos/tabela_perfil.csv
##         outputs/perfil_estratos/tabperfil.tex

suppressMessages({ library(dplyr); library(readxl); library(jsonlite) })

DIR     <- "outputs/perfil_estratos"
XLS     <- "C:/Dados/PIB_Municipios/PIB dos Munic\u00edpios - base de dados 2010-2023.xlsx"
ANO_PIB <- 2023

corr  <- read.csv(file.path(DIR, "correspondencia_municipios.csv"))
micro <- read.csv(file.path(DIR, "perfil_microdados.csv"))
pnadc <- read.csv(file.path(DIR, "perfil_estratos.csv"))

## ---- PIB dos Municipios ----------------------------------------------------
pib0 <- read_excel(XLS, guess_max = 20000)
nm   <- names(pib0)
col  <- function(p) {
  h <- grep(p, nm, fixed = TRUE)
  if (!length(h)) stop("coluna nao encontrada: ", p)
  nm[h[1]]
}
ANO_PIB <- max(pib0[[col("Ano")]][!is.na(pib0[[col("Produto Interno Bruto, ")]])])
ANO_VAB <- max(pib0[[col("Ano")]][!is.na(pib0[[col("bruto da Agropecu")]])])
message("PIB: ", ANO_PIB, " | VAB por atividade: ", ANO_VAB)

pega <- function(ano) pib0 |>
  filter(.data[[col("Ano")]] == ano,
         .data[[col("digo da Unidade da Federa")]] == 31)

pib <- pega(ANO_PIB) |>
  transmute(
    cod_mun   = as.numeric(.data[[col("digo do Munic")]]),
    nome_mun  = .data[[col("Nome do Munic")]],
    hier      = .data[[col("Hierarquia Urbana (principais")]],
    pib       = as.numeric(.data[[col("Produto Interno Bruto, ")]]))

## VAB por atividade, no ultimo ano em que a planilha o traz
vabm <- pega(ANO_VAB) |>
  transmute(
    cod_mun  = as.numeric(.data[[col("digo do Munic")]]),
    vab_agro = as.numeric(.data[[col("bruto da Agropecu")]]),
    vab_ind  = as.numeric(.data[[col("bruto da Ind")]]),
    vab_serv = as.numeric(.data[[col("bruto dos Servi")]]),
    vab_adm  = as.numeric(.data[[col("bruto da Administra")]])) |>
  ## "Valor adicionado bruto total" vem vazia na planilha: soma as componentes
  mutate(vab_tot = vab_agro + vab_ind + vab_serv + vab_adm)

stopifnot(nrow(pib) == 853L, nrow(vabm) == 853L,
          !anyNA(pib$pib), !anyNA(vabm$vab_tot))

base <- corr |> left_join(pib, by = "cod_mun") |> left_join(vabm, by = "cod_mun")
stopifnot(!anyNA(base$vab_tot))

## ---- cidade-polo: topo da hierarquia urbana do REGIC, desempate pela
## populacao. O desempate pelo PIB elegia municipios de PIB alto que nao sao
## polo regional (Extrema no Sul de Minas, Betim no entorno de BH).
ORD_HIER <- c("Grande metr\u00f3pole nacional", "Metr\u00f3pole nacional",
              "Metr\u00f3pole", "Capital regional", "Centro sub-regional",
              "Centro de zona", "Centro local")
arq_pop <- file.path(DIR, "_cache", "pop_mun_sidra.json")
pop <- as.data.frame(fromJSON(arq_pop))[-1, ]
pop <- data.frame(cod_mun = as.numeric(pop$D1C), pop_mun = as.numeric(pop$V))

polo <- base |> left_join(pop, by = "cod_mun") |>
  mutate(rk = match(hier, ORD_HIER), rk = ifelse(is.na(rk), 99L, rk)) |>
  group_by(regiao) |> arrange(rk, desc(pop_mun), .by_group = TRUE) |>
  summarise(polo = first(nome_mun), polo_hier = first(hier),
            polo_pop = first(pop_mun), .groups = "drop")

## ---- composicao do VAB -----------------------------------------------------
ATIV <- c("Agropecu\u00e1ria", "Ind\u00fastria", "Servi\u00e7os",
          "Adm. p\u00fablica")
vab <- base |> group_by(regiao) |>
  summarise(across(c(vab_agro, vab_ind, vab_serv, vab_adm, vab_tot, pib), sum),
            .groups = "drop") |>
  mutate(p_agro = 100 * vab_agro / vab_tot, p_ind = 100 * vab_ind / vab_tot,
         p_serv = 100 * vab_serv / vab_tot, p_adm = 100 * vab_adm / vab_tot,
         pib_bi = pib / 1e6)
M <- as.matrix(vab[, c("p_agro", "p_ind", "p_serv", "p_adm")])
vab$predominante <- ATIV[max.col(M, ties.method = "first")]
vab$p_pred       <- apply(M, 1, max)

## ---- junta tudo ------------------------------------------------------------
k <- function(x) substr(x, 1, 2)
tab <- corr |> count(regiao, name = "n_mun") |>
  left_join(vab |> select(regiao, pib_bi, predominante, p_pred), by = "regiao") |>
  left_join(polo, by = "regiao") |>
  mutate(k = k(regiao)) |>
  left_join(micro |> mutate(k = k(regiao)) |> select(-regiao), by = "k") |>
  left_join(pnadc |> mutate(k = k(estrato)) |> select(-estrato), by = "k") |>
  arrange(k) |> select(-k)

write.csv(tab, file.path(DIR, "tabela_perfil.csv"), row.names = FALSE)

cat("\n#### perfil dos estratos ####\n")
print(as.data.frame(tab |> transmute(
  estrato = regiao, mun = n_mun, polo, hier = polo_hier,
  pop_mi = round(populacao_mil / 1000, 2), part_ocup = part_ocup_mg_2024,
  rend = rendimento_medio, taxa = taxa_2024,
  setor = predominante, pct = round(p_pred), cv_ocup)), row.names = FALSE)

## ---- .tex ------------------------------------------------------------------
B   <- intToUtf8(92)
fmt <- function(v, d = 1) formatC(v, format = "f", digits = d,
                                  big.mark = ".", decimal.mark = ",")
pc  <- paste0(B, "%")

linhas <- with(tab, sprintf("%s & %s & %s & %s & %s & %s & %s & %s (%s) & %s %s",
  sub("^0", "", sub(" - ", ". ", regiao)), n_mun, polo,
  fmt(populacao_mil / 1000, 2), fmt(part_ocup_mg_2024), fmt(rendimento_medio, 0),
  fmt(taxa_2024), predominante, fmt(p_pred, 0), fmt(cv_ocup), paste0(B, B)))

tex <- c(
  paste0(B, "begin{table}[htbp]"), paste0(B, "centering"),
  paste0(B, "caption{Perfil dos estratos geogr\u00e1ficos da PNAD Cont\u00ednua em Minas Gerais}"),
  paste0(B, "label{tab:perfil}"), paste0(B, "footnotesize"),
  paste0(B, "begin{tabular}{lrlrrrrlr}"), paste0(B, "toprule"),
  paste0("Estrato & Mun. & Cidade-polo & Pop. & Ocup. & Rend. & Taxa & ",
         "Atividade predominante & CV ", B, B),
  paste0(" & & & (milh\u00f5es) & (", pc, ") & (R$) & (", pc, ") & (", pc,
         " do VAB) & (", pc, ") ", B, B),
  paste0(B, "midrule"), linhas, paste0(B, "bottomrule"),
  paste0(B, "end{tabular}"),
  paste0(B, "begin{flushleft}", B, "scriptsize"),
  paste0("Nota: a composi\u00e7\u00e3o municipal dos estratos vem da camada de ",
         "estratos geogr\u00e1ficos da PNAD Cont\u00ednua (IBGE, geosservi\u00e7os). ",
         "Popula\u00e7\u00e3o, rendimento m\u00e9dio mensal habitual, taxa de ",
         "desocupa\u00e7\u00e3o e participa\u00e7\u00e3o na ocupa\u00e7\u00e3o de Minas ",
         "Gerais referem-se a 2024; a atividade predominante, ao PIB dos ",
         "Munic\u00edpios de ", ANO_PIB, ". CV \u00e9 o coeficiente de ",
         "varia\u00e7\u00e3o m\u00e9dio da estimativa direta de ocupados no ",
         "per\u00edodo analisado."),
  paste0(B, "end{flushleft}"), paste0(B, "end{table}"))

arq <- file.path(DIR, "tabperfil.tex")
con <- file(arq, open = "wb"); writeLines(tex, con, useBytes = TRUE); close(con)
cat("\ngravado:", arq, "\n")
