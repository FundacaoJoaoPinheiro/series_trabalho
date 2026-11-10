################################################################################
## 41_tabelas_apendice.R
##
## Apêndice A do manuscrito (decisão do Caio em 10/11/2026): os hiperparâmetros
## e os diagnósticos dos resíduos saem do corpo e vão para duas tabelas únicas,
## com um painel por indicador (A desocupados, B ocupados, C taxa).
##
## Não reestima nem recalcula nada: lê as linhas das tabelas por indicador que o
## 25_saidas_artigo.R grava em artigo/resultados/<indicador>/ e as recompõe.
## Rodar depois do 25_. Atenção: os p-valores do multivariado nos diagnósticos
## variam na 3a-4a casa conforme a versão do dlm; o manuscrito usa os da rodada C
## gerados em 07/11/2026 (ver estado_artigo.md, 10/11/2026).
##
## Saídas: artigo/resultados/tabhiper_apendice.tex
##         artigo/resultados/tabdiag_apendice.tex
################################################################################

RAIZ   <- Sys.getenv("REPO_RAIZ", unset = getwd())
ARTIGO <- Sys.getenv("ARTIGO_DIR", unset = file.path(RAIZ, "artigo"))
RES    <- file.path(ARTIGO, "resultados")

IND <- list(
  list(pasta = "Total de desocupados", suf = "desoc", rot = "A. Total de desocupados"),
  list(pasta = "Total de ocupados",    suf = "ocup",  rot = "B. Total de ocupados"),
  list(pasta = "Taxa de desocupação",  suf = "taxa",  rot = "C. Taxa de desocupação"))

le <- function(arq) readLines(arq, encoding = "UTF-8", warn = FALSE)
linhas_estrato <- function(x) x[grepl("^0[1-8] - ", x)]
celulas <- function(l) trimws(strsplit(sub("\\s*\\\\\\\\\\s*$", "", l), " & ", fixed = TRUE)[[1]])
painel  <- function(rot, ncol) sprintf("\\multicolumn{%d}{l}{\\textit{%s}} \\\\", ncol, rot)

grava <- function(tex, nome) {
  con <- file(file.path(RES, nome), open = "w", encoding = "UTF-8")
  writeLines(tex, con); close(con)
  cat("gravado:", nome, "\n")
}

## ---- hiperparâmetros -------------------------------------------------------
corpo_h <- unlist(lapply(seq_along(IND), function(k) {
  i <- IND[[k]]
  l <- linhas_estrato(le(file.path(RES, i$pasta, paste0("tabhiper", i$suf, ".tex"))))
  stopifnot(length(l) == 8)
  c(if (k > 1) "\\midrule", painel(i$rot, 7), l)
}))
tex_h <- c(
  "\\begin{table}[H]", "\\centering", "\\footnotesize",
  "\\captionsetup{justification=centering}",
  "\\caption{Hiperparâmetros estimados - modelos univariado e multivariado}",
  "\\label{tab:apx_hiper}",
  "\\adjustbox{max width=\\linewidth}{%", "\\renewcommand{\\arraystretch}{0.85}",
  "\\begin{tabular}{llccccc}", "\\toprule",
  paste0("& & & \\multicolumn{2}{c}{\\textbf{Modelo Univariado}} & ",
         "\\multicolumn{2}{c}{\\textbf{Modelo Multivariado}} \\\\"),
  "\\cmidrule(lr){4-5} \\cmidrule(lr){6-7}",
  paste0("\\textbf{Estrato geográfico} & \\textbf{Processo} & \\textbf{Coeficientes} & ",
         "\\(\\hat{\\sigma}_R^2\\) & \\(\\hat{\\sigma}_I^2\\) & ",
         "\\(\\hat{\\sigma}_R^2\\) & \\(\\hat{\\sigma}_I^2\\) \\\\"),
  "\\midrule", corpo_h, "\\bottomrule", "\\end{tabular}}",
  "\\fonte{Elaboração própria, com base nos dados da PNAD Contínua.}",
  "\\end{table}")
grava(tex_h, "tabhiper_apendice.tex")

## ---- diagnósticos: univariado e multivariado lado a lado --------------------
corpo_d <- unlist(lapply(seq_along(IND), function(k) {
  i <- IND[[k]]
  l <- linhas_estrato(le(file.path(RES, i$pasta, paste0("diag", i$suf, ".tex"))))
  stopifnot(length(l) == 16)                      # 8 univariado + 8 multivariado
  u <- lapply(l[1:8], celulas); m <- lapply(l[9:16], celulas)
  lin <- vapply(1:8, function(j) {
    stopifnot(u[[j]][1] == m[[j]][1])
    paste(c(u[[j]], m[[j]][-1]), collapse = " & ")
  }, "")
  c(if (k > 1) "\\midrule", painel(i$rot, 7), paste(lin, "\\\\"))
}))
tex_d <- c(
  "\\begin{table}[H]", "\\centering", "\\footnotesize",
  "\\captionsetup{justification=centering}",
  "\\caption{Testes de diagnóstico dos resíduos - modelos univariado e multivariado}",
  "\\label{tab:apx_diag}",
  "\\adjustbox{max width=\\linewidth}{%", "\\renewcommand{\\arraystretch}{0.85}",
  "\\begin{tabular}{lcccccc}", "\\toprule",
  paste0("& \\multicolumn{3}{c}{\\textbf{Modelo Univariado}} & ",
         "\\multicolumn{3}{c}{\\textbf{Modelo Multivariado}} \\\\"),
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  paste0("\\textbf{Estrato geográfico} & Shapiro-Wilk & Ljung-Box & H & ",
         "Shapiro-Wilk & Ljung-Box & H \\\\"),
  "\\midrule", corpo_d, "\\bottomrule", "\\end{tabular}}",
  paste0("\\fonte{Elaboração própria, com base nos dados da PNAD Contínua. ",
         "Nota: p-valores; * rejeição de \\(H_0\\) a 10\\%, ** a 5\\% e *** a 1\\%; ",
         "descartadas as oito primeiras observações (estabilização do filtro de Kalman).}"),
  "\\end{table}")
grava(tex_d, "tabdiag_apendice.tex")
