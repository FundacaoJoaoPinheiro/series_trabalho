## 31_mapa_estratos.R
## Mapa dos oito estratos geograficos da PNAD Continua em Minas Gerais.
##
## Reproduz em R o mapa feito no QGIS, a partir das mesmas fontes, agora com a
## projecao declarada no proprio codigo: malha municipal do IBGE (geobr, 2022)
## dissolvida pela correspondencia de 30_correspondencia_municipios.R, em
## Albers Conica Equivalente / SIRGAS 2000 (o padrao do IBGE para o Brasil).
##
## Insumo : outputs/perfil_estratos/correspondencia_municipios.csv
## Saidas : outputs/figuras_final/mapa_estratos.pdf e .png

suppressMessages({
  library(sf); library(geobr); library(dplyr); library(ggplot2)
})
source("rotinas/00_tema_graficos.R")
sf_use_s2(FALSE)

DIR <- "outputs/figuras_final"; dir.create(DIR, recursive = TRUE, showWarnings = FALSE)

## Albers Conica Equivalente, SIRGAS 2000 (EPSG:4674 como datum de origem)
ALBERS <- paste0("+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 ",
                 "+x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")

corr <- read.csv("outputs/perfil_estratos/correspondencia_municipios.csv")

mun <- read_municipality(code_muni = "MG", year = 2022, showProgress = FALSE) |>
  left_join(corr, by = c("code_muni" = "cod_mun"))
stopifnot(!any(is.na(mun$regiao)))

## dissolve os municipios em estratos
estratos <- mun |> group_by(regiao) |> summarise(.groups = "drop") |>
  st_transform(ALBERS)
munl <- st_transform(mun, ALBERS)

## rotulo curto, na forma usada no texto do artigo
estratos <- estratos |>
  mutate(rot = sub("^0", "", sub(" - ", ". ", regiao)),
         rot = sub("Colar e Entorno Metropolitano de BH",
                   "Colar e Entorno\nMetropolitano de BH", rot))

pos <- st_point_on_surface(st_geometry(estratos)) |> st_coordinates() |>
  as.data.frame() |> mutate(rot = estratos$rot)

g <- ggplot() +
  geom_sf(data = munl, fill = NA, color = "white", linewidth = .12) +
  geom_sf(data = estratos, aes(fill = regiao), color = "grey25",
          linewidth = .45, alpha = .85) +
  geom_sf(data = munl, fill = NA, color = "white", linewidth = .10) +
  ggrepel::geom_label_repel(
    data = pos, aes(X, Y, label = rot), size = 2.9, lineheight = .95,
    label.size = .18, label.r = unit(.12, "lines"), fill = "white",
    alpha = .92, min.segment.length = 0, segment.color = "grey30",
    box.padding = .35, seed = 1) +
  scale_fill_manual(values = PAL_ARTIGO[1:8], guide = "none") +
  labs(x = NULL, y = NULL,
       caption = paste0(
         "Fonte: elaboração própria a partir da malha municipal do IBGE (2022) e da camada de estratos
",
         "geográficos da PNAD Contínua (IBGE, geosserviços, PNADC:v_ibge_estpnadc_anual_poligono).
",
         "Projeção Albers Cônica Equivalente, datum SIRGAS 2000. Linhas brancas: limites municipais.")) +
  coord_sf(datum = NA) +
  tema_artigo(11) +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), plot.caption = element_text(size = 7))

ggsave(file.path(DIR, "mapa_estratos.pdf"), g, width = 7.5, height = 7.2,
       device = cairo_pdf)
ggsave(file.path(DIR, "mapa_estratos.png"), g, width = 7.5, height = 7.2,
       dpi = 300, bg = "white")

cat("gravado:", file.path(DIR, "mapa_estratos.pdf"), "e .png\n")
cat("municipios por estrato:\n"); print(as.data.frame(count(corr, regiao)))
