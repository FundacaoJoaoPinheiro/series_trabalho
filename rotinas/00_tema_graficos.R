################################################################################
## 00 - TEMA E PALETA PADRÃO DOS GRÁFICOS DO ARTIGO (Estratos Geográficos)
## source("rotinas/00_tema_graficos.R") antes de plotar. Requer: ggplot2, scales.
##
## Padrão aprovado: theme_minimal limpo, legenda embaixo, eixo com anos (1 em 1)
## + gridlines menores nos trimestres, paleta Tableau-10 (colorblind-friendly),
## números em formato brasileiro.
################################################################################
suppressMessages({ library(ggplot2); library(scales) })

## Paleta qualitativa (até 10 séries), segura para daltonismo
PAL_ARTIGO <- c("#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F",
                "#EDC948","#B07AA1","#FF9DA7","#9C755F","#BAB0AC")

## Tema base
tema_artigo <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 1),
      plot.subtitle    = element_text(color = "grey35", size = base_size - 2),
      plot.caption     = element_text(color = "grey45", size = base_size - 4, hjust = 0),
      axis.title       = element_text(color = "grey25"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      legend.key.width = unit(1.4, "lines"),
      panel.grid.minor = element_line(linewidth = .25, color = "grey93"),
      panel.grid.major = element_line(linewidth = .40, color = "grey86"),
      plot.margin      = margin(10, 14, 8, 10)
    )
}

## periodo "YYYY_0Q" -> Date (1o dia do trimestre)
periodo_para_data <- function(periodo) {
  ano <- as.integer(substr(periodo, 1, 4)); tri <- as.integer(substr(periodo, 7, 7))
  as.Date(sprintf("%d-%02d-01", ano, (tri - 1) * 3 + 1))
}

## Gráfico de linha padrão.
##   d: data.frame com colunas 'data' (Date), 'valor' (numérico), 'grupo' (factor/char)
## Se houver só uma série (grupo único), a legenda é omitida.
grafico_linha <- function(d, titulo = NULL, subtitulo = NULL, ylab = NULL,
                          fonte = "Fonte: elaboração própria com dados da PNAD Contínua/IBGE.",
                          nrow_leg = 3, base_size = 12, hlines = NULL) {
  n_grp <- length(unique(d$grupo))
  p <- ggplot(d, aes(data, valor, color = grupo))
  ## linhas de referência horizontais (ex.: limiares de CV 15%/30%), atrás das séries
  if (!is.null(hlines))
    p <- p + geom_hline(yintercept = hlines, linetype = "dashed", color = "grey45", linewidth = .4)
  p <- p +
    geom_line(linewidth = .75) +
    scale_color_manual(values = PAL_ARTIGO) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y",
                 date_minor_breaks = "3 months", expand = expansion(mult = c(.01, .02))) +
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = titulo, subtitle = subtitulo, x = NULL, y = ylab, caption = fonte) +
    tema_artigo(base_size)
  if (n_grp > 1) p <- p + guides(color = guide_legend(nrow = nrow_leg, byrow = TRUE))
  else          p <- p + theme(legend.position = "none")
  p
}
