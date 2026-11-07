## 30_correspondencia_municipios.R
## Correspondencia exata municipio -> estrato geografico da PNAD Continua em MG.
##
## Fonte: o IBGE publica os estratos geograficos da PNADc como camada de
## poligonos no WFS de geoservicos (PNADC:v_ibge_estpnadc_anual_poligono,
## EPSG:4674 / SIRGAS 2000). Cada estrato e uniao de municipios inteiros, de
## modo que o cruzamento com a malha municipal devolve a correspondencia exata
## -- nao uma aproximacao por mesorregiao ou regiao intermediaria.
##
## Saidas: outputs/perfil_estratos/correspondencia_municipios.csv
##         outputs/perfil_estratos/estratos_mg.gpkg  (geometria, p/ o mapa)

suppressMessages({
  library(sf); library(geobr); library(dplyr); library(jsonlite)
})
sf_use_s2(FALSE)

DIR <- "outputs/perfil_estratos"
dir.create(DIR, recursive = TRUE, showWarnings = FALSE)
CACHE <- file.path(DIR, "_cache"); dir.create(CACHE, showWarnings = FALSE)

## ---- 1. estratos geograficos da PNADc (WFS do IBGE) ------------------------
arq_est <- file.path(CACHE, "estratos_mg_wfs.json")
if (!file.exists(arq_est)) {
  url <- paste0(
    "https://geoservicos.ibge.gov.br/geoserver/wfs?service=WFS&version=1.1.0",
    "&request=GetFeature&typeName=PNADC:v_ibge_estpnadc_anual_poligono",
    "&outputFormat=application/json",
    "&propertyName=geom,cd_estpnadc_num,Nom_Estrato,NomeAbreviado",
    "&CQL_FILTER=cd_estpnadc_num%3E=3110%20AND%20cd_estpnadc_num%3C=3156")
  message("baixando os poligonos dos estratos do WFS do IBGE ...")
  utils::download.file(url, arq_est, mode = "wb", quiet = TRUE)
}
est <- st_read(arq_est, quiet = TRUE) |>
  select(cd_est = cd_estpnadc_num, nome_ibge = Nom_Estrato,
         nome_abrev = NomeAbreviado) |>
  distinct(cd_est, .keep_all = TRUE)       # a camada repete a geometria por ano
stopifnot(nrow(est) == 10L)

## ---- 2. malha municipal ----------------------------------------------------
mun <- read_municipality(code_muni = "MG", year = 2022, showProgress = FALSE) |>
  st_transform(st_crs(est))

## ---- 3. cruzamento ---------------------------------------------------------
pt <- suppressWarnings(st_point_on_surface(st_geometry(mun)))
dentro <- st_within(pt, st_geometry(est), sparse = FALSE)
if (any(rowSums(dentro) != 1L))
  stop("ha municipio sem estrato ou em mais de um estrato")
k <- apply(dentro, 1, which)

## rotulos do artigo: a RIDE (3140) entra no Norte, como em 12_Base PNADC 8reg.R
rotulo <- function(cd) dplyr::case_when(
  cd == 3110            ~ "01 - Belo Horizonte",
  cd %in% c(3120, 3130) ~ "02 - Colar e Entorno Metropolitano de BH",
  cd == 3151            ~ "03 - Sul de Minas",
  cd == 3152            ~ "04 - Triângulo Mineiro",
  cd == 3153            ~ "05 - Zona da Mata",
  cd %in% c(3154, 3140) ~ "06 - Norte de Minas",
  cd == 3155            ~ "07 - Vale do Rio Doce",
  cd == 3156            ~ "08 - Central")

corr <- st_drop_geometry(mun) |>
  transmute(cod_mun = code_muni, municipio = name_muni,
            cd_est = est$cd_est[k], nome_ibge = est$nome_ibge[k],
            nome_abrev = est$nome_abrev[k]) |>
  mutate(regiao = rotulo(cd_est)) |>
  arrange(regiao, municipio)

write.csv(corr, file.path(DIR, "correspondencia_municipios.csv"),
          row.names = FALSE, na = "")
st_write(est, file.path(DIR, "estratos_mg.gpkg"), delete_dsn = TRUE, quiet = TRUE)

cat("\n#### municipios por estrato ####\n")
print(as.data.frame(count(corr, regiao, name = "n_mun")))
cat("total:", nrow(corr), "municipios\n")

## ---- 4. validacao contra a populacao estimada pela PNADc -------------------
## A PNADc e calibrada por pos-estratos geograficos, nao pelos estratos da
## amostra. Em MG os pos-estratos sao a capital, a RM de BH e o resto da UF:
## nesses tres niveis a correspondencia tem de fechar; entre os seis estratos
## do "resto de MG" a diferenca que sobra e erro amostral da propria PNADc.
arq_pop <- file.path(CACHE, "pop_mun_sidra.json")
if (!file.exists(arq_pop))
  utils::download.file(
    "https://apisidra.ibge.gov.br/values/t/6579/n6/in%20n3%2031/v/9324/p/2024",
    arq_pop, mode = "wb", quiet = TRUE)
pop <- as.data.frame(fromJSON(arq_pop))[-1, ]
pop <- data.frame(cod_mun = as.numeric(pop$D1C), pop = as.numeric(pop$V))

arq_pnadc <- file.path(DIR, "perfil_microdados.csv")
if (file.exists(arq_pnadc)) {
  alvo <- read.csv(arq_pnadc) |>
    filter(regiao != "Minas Gerais") |>
    transmute(ord = substr(regiao, 1, 2), pop_pnadc = populacao_mil)
  cmp <- corr |> left_join(pop, by = "cod_mun") |>
    group_by(regiao) |>
    summarise(pop_ibge = sum(pop) / 1000, .groups = "drop") |>
    mutate(ord = substr(regiao, 1, 2)) |>
    left_join(alvo, by = "ord") |> select(-ord) |>
    mutate(dif_pct = round(100 * (pop_ibge / pop_pnadc - 1), 1),
           pop_ibge = round(pop_ibge, 1))
  cat("\n#### validacao: populacao 2024, projecao municipal x PNADc ####\n")
  print(as.data.frame(cmp))
  pe <- function(r) sum(cmp$pop_ibge[r]) / sum(cmp$pop_pnadc[r]) - 1
  cat("\npor pos-estrato da calibracao:\n")
  cat("  capital        ", sprintf("%+.2f%%", 100 * pe(1)), "\n")
  cat("  RM de BH       ", sprintf("%+.2f%%", 100 * pe(2)), "\n")
  cat("  resto de MG    ", sprintf("%+.2f%%", 100 * pe(3:8)), "\n")
} else {
  message("rode 29_perfil_microdados.R antes para a validacao")
}

cat("\ngravado:", file.path(DIR, "correspondencia_municipios.csv"), "\n")
