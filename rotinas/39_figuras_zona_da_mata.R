################################################################################
## 39_figuras_zona_da_mata.R — robustez de 10/11/2026, tarefa 7
##
## Regera as Figuras 2 e 3 do manuscrito (versão do Overleaf) com o rótulo
## "Zona da Mata" no lugar de "Mata de Minas Gerais", no visual padrão do artigo
## (rotinas/00_tema_graficos.R: grafico_linha, paleta Tableau-10, legenda embaixo).
##
##   Figura 2  Coeficientes de variação - 10 estratos.png
##             CV do total de desocupados, 10 estratos originais, linhas de 15% e
##             30%. É a figura cv_desocupados_10reg.png de rotinas/03_organizacao_cv.R
##             -- mesma chamada a grafico_linha, mesmo tamanho e dpi --, que tirava
##             o rótulo direto do nome do estrato no SIPD ("07-Mata de Minas Gerais").
##   Figura 3  03_TaxaDesoc.png
##             taxa de desocupação DIRETA (%), 8 estratos. Nem a imagem nem o script
##             que a gerou estão no repositório ou no zip do Overleaf; foi refeita
##             no mesmo padrão da Figura 2 (sem linhas de referência).
##
## Insumos (vintage da raiz, a que reproduz o artigo): baseestr0424_10reg.rds,
##          baseestr8reg.rds
## Saída  : outputs/robustez_2026-11-10/figuras/  (as de artigo/ NÃO são tocadas)
################################################################################
RAIZ <- Sys.getenv("REPO_RAIZ", unset = getwd())
source(file.path(RAIZ, "rotinas", "00_tema_graficos.R"))
SAIDA <- file.path(RAIZ, "outputs", "robustez_2026-11-10", "figuras")
dir.create(SAIDA, recursive = TRUE, showWarnings = FALSE)

## rótulo do manuscrito: "Mata de Minas Gerais" (nome do SIPD) -> "Zona da Mata"
## e "Colar e Entorno Metropolitano de BH" com M maiúsculo, como no manuscrito
rotulo <- function(x) sub("Colar e Entorno metropolitano", "Colar e Entorno Metropolitano",
                          sub("Mata de Minas Gerais", "Zona da Mata", x, fixed = TRUE), fixed = TRUE)

longo <- function(base, regs, col, mult = 1) {
  per <- base[[regs[1]]][["Período"]]
  do.call(rbind, lapply(regs, function(r)
    data.frame(data = periodo_para_data(base[[r]][["Período"]]),
               grupo = rotulo(r), valor = base[[r]][[col]] * mult)))
}
ordena <- function(d, regs) { d$grupo <- factor(d$grupo, levels = rotulo(regs)); d }

## ---- Figura 2 -----------------------------------------------------------------
b10  <- readRDS(file.path(RAIZ, "baseestr0424_10reg.rds"))
r10  <- setdiff(names(b10), "11 - Minas Gerais")
d2   <- ordena(longo(b10, r10, "CV.desocupados"), r10)
stopifnot(!any(grepl("Mata de Minas", levels(d2$grupo))), "07-Zona da Mata" %in% levels(d2$grupo))
g2 <- grafico_linha(d2, titulo = NULL, subtitulo = NULL, fonte = NULL,
                    ylab = "Coeficiente de variação (%)", hlines = c(15, 30))
ggsave(file.path(SAIDA, "Coeficientes de variação - 10 estratos.png"), g2,
       width = 10, height = 5.6, dpi = 135, bg = "white")

## ---- Figura 3 -----------------------------------------------------------------
b8 <- readRDS(file.path(RAIZ, "baseestr8reg.rds"))
r8 <- setdiff(names(b8), "09 - Minas Gerais")
d3 <- ordena(longo(b8, r8, "Taxa.de.desocupação", mult = 100), r8)
stopifnot(!any(grepl("Mata de Minas", levels(d3$grupo))), "05-Zona da Mata" %in% levels(d3$grupo))
## formato da figura que está no Overleaf: rótulos sem número, eixo x "Período",
## legenda em 2 linhas e proporção mais achatada (não altera a paginação)
levels(d3$grupo) <- sub("^\\d+ ?- ?", "", levels(d3$grupo))
g3 <- grafico_linha(d3, titulo = NULL, subtitulo = NULL, fonte = NULL,
                    ylab = "Taxa de desocupação (%)", nrow_leg = 2) + ggplot2::labs(x = "Período")
ggsave(file.path(SAIDA, "03_TaxaDesoc.png"), g3, width = 10, height = 4.3, dpi = 135, bg = "white")

cat("Figura 2: ", length(r10), " estratos, ", min(d2$data), " a ", max(d2$data),
    " | CV máximo ", round(max(d2$valor, na.rm = TRUE), 1), "%\n", sep = "")
cat("Figura 3: ", length(r8), " estratos, ", min(d3$data), " a ", max(d3$data),
    " | taxa de ", round(min(d3$valor), 1), "% a ", round(max(d3$valor), 1), "%\n", sep = "")
cat("rótulos:", paste(levels(d2$grupo), collapse = " | "), "\n")
cat("gravado em", SAIDA, "\n")
