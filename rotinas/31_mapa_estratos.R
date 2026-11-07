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
  library(ggspatial)
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

## dissolve em estratos. Usa os poligonos ja dissolvidos pelo IBGE (gravados
## por 30_correspondencia_municipios.R): dissolver a malha municipal deixa
## microgaps entre municipios, que apareciam como pontilhado escuro no interior
## dos estratos. Os buffers +-200 m fecham as costuras das duas unioes que
## ainda sao necessarias (3120+3130 e 3154+3140).
rotulo <- function(cd) dplyr::case_when(
  cd == 3110            ~ "01 - Belo Horizonte",
  cd %in% c(3120, 3130) ~ "02 - Colar e Entorno Metropolitano de BH",
  cd == 3151            ~ "03 - Sul de Minas",
  cd == 3152            ~ "04 - Triângulo Mineiro",
  cd == 3153            ~ "05 - Zona da Mata",
  cd %in% c(3154, 3140) ~ "06 - Norte de Minas",
  cd == 3155            ~ "07 - Vale do Rio Doce",
  cd == 3156            ~ "08 - Central")

estratos <- st_read("outputs/perfil_estratos/estratos_mg.gpkg", quiet = TRUE) |>
  st_transform(ALBERS) |> st_make_valid() |>
  mutate(regiao = rotulo(cd_est)) |>
  group_by(regiao) |> summarise(.groups = "drop") |>
  st_buffer(200) |> st_buffer(-200) |> st_make_valid()
munl <- st_transform(mun, ALBERS) |>
  st_intersection(st_union(estratos)) |> suppressWarnings()

## UFs vizinhas, para o estado nao ficar solto na pagina
ufs <- read_state(year = 2020, showProgress = FALSE) |> st_transform(ALBERS)
viz <- ufs |> filter(abbrev_state != "MG")
cx  <- st_bbox(estratos)
marg <- c(x = .22, y = .22)
lim_x <- c(cx["xmin"] - marg["x"] * (cx["xmax"] - cx["xmin"]),
           cx["xmax"] + marg["x"] * (cx["xmax"] - cx["xmin"]))
lim_y <- c(cx["ymin"] - marg["y"] * (cx["ymax"] - cx["ymin"]),
           cx["ymax"] + marg["y"] * (cx["ymax"] - cx["ymin"]))
rot_uf <- st_point_on_surface(st_geometry(viz)) |> st_coordinates() |>
  as.data.frame() |> mutate(uf = viz$abbrev_state) |>
  filter(X > lim_x[1], X < lim_x[2], Y > lim_y[1], Y < lim_y[2])

## rotulo curto, na forma usada no texto do artigo
estratos <- estratos |>
  mutate(rot = sub("^0", "", sub(" - ", ". ", regiao)),
         rot = sub("Colar e Entorno Metropolitano de BH",
                   "Colar e Entorno\nMetropolitano de BH", rot))

pos <- st_point_on_surface(st_geometry(estratos)) |> st_coordinates() |>
  as.data.frame() |> mutate(rot = estratos$rot)

## preenchimento solido e claro: com alpha, os limites municipais desenhados
## por baixo apareciam como chuvisco cinza dentro dos estratos
clarear <- function(cores, p = .45) {
  m <- grDevices::col2rgb(cores)
  grDevices::rgb(t(m + (255 - m) * p), maxColorValue = 255)
}

g <- ggplot() +
  geom_sf(data = viz, fill = "grey93", color = "grey75", linewidth = .3) +
  geom_text(data = rot_uf, aes(X, Y, label = uf), color = "grey55",
            size = 3, fontface = "bold") +
  geom_sf(data = estratos, aes(fill = regiao), color = NA) +
  geom_sf(data = munl, fill = NA, color = "white", linewidth = .22) +
  geom_sf(data = estratos, fill = NA, color = "grey20", linewidth = .5) +
  ggrepel::geom_label_repel(
    data = pos, aes(X, Y, label = rot), size = 2.9, lineheight = .95,
    label.size = .18, label.r = unit(.12, "lines"), fill = "white",
    alpha = .92, min.segment.length = 0, segment.color = "grey30",
    box.padding = .35, seed = 1) +
  scale_fill_manual(values = clarear(PAL_ARTIGO[1:8]), guide = "none") +
  labs(x = NULL, y = NULL) +
  annotation_scale(location = "br", width_hint = .28, height = unit(.12, "cm"),
                   text_cex = .62, line_width = .4, pad_y = unit(.45, "cm")) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         height = unit(1.05, "cm"), width = unit(.95, "cm"),
                         style = north_arrow_fancy_orienteering(
                           text_size = 8, line_width = .8)) +
  coord_sf(xlim = lim_x, ylim = lim_y, datum = NA, expand = FALSE) +
  tema_artigo(11) +
  theme(panel.grid = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "grey98", color = NA),
        panel.border = element_rect(fill = NA, color = "grey70", linewidth = .4))

ggsave(file.path(DIR, "mapa_estratos.pdf"), g, width = 7.5, height = 7.2,
       device = cairo_pdf)
ggsave(file.path(DIR, "mapa_estratos.png"), g, width = 7.5, height = 7.2,
       dpi = 300, bg = "white")

## A nota da figura NAO vai embutida na imagem: o manuscrito ja traz a linha de
## Fonte em LaTeX, e duplicar ficaria feio e inconsistente. O texto e gravado
## aqui para que a linha do .tex continue rastreavel a este script -- quem
## mudar a fonte do dado muda aqui e copia para o manuscrito.
fonte <- paste0(
  "Fonte: elabora\u00e7\u00e3o pr\u00f3pria a partir da malha municipal do ",
  "IBGE (2022) e da camada de estratos geogr\u00e1ficos da PNAD Cont\u00ednua ",
  "(IBGE, geosservi\u00e7os, PNADC:v_ibge_estpnadc_anual_poligono). ",
  "Nota: proje\u00e7\u00e3o Albers C\u00f4nica Equivalente, datum SIRGAS 2000. ",
  "As linhas brancas s\u00e3o limites municipais.")
con <- file(file.path(DIR, "mapa_estratos_fonte.txt"), open = "wb")
writeLines(fonte, con, useBytes = TRUE); close(con)

cat("gravado:", file.path(DIR, "mapa_estratos.pdf"), "e .png\n")
cat("municipios por estrato:\n"); print(as.data.frame(count(corr, regiao)))
