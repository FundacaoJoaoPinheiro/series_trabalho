################################################################################
## 41_tabelas_apendice.R
##
## Monta as duas tabelas consolidadas do manuscrito, com um painel por indicador
## (A desocupados, B ocupados, C taxa), a partir das tabelas por indicador que o
## 25_saidas_artigo.R grava em artigo/resultados/<indicador>/. Nao reestima nem
## recalcula nada. Rodar depois do 25_.
##
## Historico das duas decisoes do Caio que definem o formato atual:
##  - 10/11/2026: hiperparametros e diagnosticos saem do corpo do texto e viram
##    duas tabelas unicas, de tres paineis, no Apendice A.
##  - 20/11/2026: a tabela dos HIPERPARAMETROS volta para o corpo do texto, sem
##    as duas colunas de sigma_I^2 -- a variancia do componente irregular foi
##    estimada em zero nos 24 casos, nos dois modelos, e a Secao 4 explica por
##    que (verossimilhanca quase plana em sigma_I^2). Com isso a tabela cabe na
##    largura do corpo. O arquivo passou a se chamar tabhiper_modelos.tex e o
##    rotulo, tab:hiper. Os DIAGNOSTICOS continuam no apendice, que passou a ser
##    so deles.
##
## O stopifnot da coluna suprimida e intencional: se uma reestimacao futura
## produzir sigma_I^2 diferente de zero, o script PARA em vez de esconder o
## numero ao suprimir a coluna.
##
## Atencao: os p-valores do multivariado nos diagnosticos variam na 3a-4a casa
## conforme a versao do dlm; o manuscrito usa os da rodada C gerados em
## 07/11/2026 (ver estado_artigo.md, 10/11/2026).
##
## Saidas: artigo/resultados/tabhiper_modelos.tex   (corpo do texto, Secao 4)
##         artigo/resultados/tabdiag_apendice.tex   (Apendice A)
################################################################################

RAIZ   <- Sys.getenv("REPO_RAIZ", unset = getwd())
ARTIGO <- Sys.getenv("ARTIGO_DIR", unset = file.path(RAIZ, "artigo"))
RES    <- file.path(ARTIGO, "resultados")

IND <- list(
  list(pasta = "Total de desocupados", suf = "desoc", rot = "A. Total de desocupados"),
  list(pasta = "Total de ocupados",    suf = "ocup",  rot = "B. Total de ocupados"),
  list(pasta = "Taxa de desocupa\u00e7\u00e3o",  suf = "taxa",  rot = "C. Taxa de desocupa\u00e7\u00e3o"))

le <- function(arq) readLines(arq, encoding = "UTF-8", warn = FALSE)
linhas_estrato <- function(x) x[grepl("^0[1-8] - ", x)]
celulas <- function(l) trimws(strsplit(sub("\\s*\\\\\\\\\\s*$", "", l), " & ", fixed = TRUE)[[1]])
painel  <- function(rot, ncol) sprintf("\\multicolumn{%d}{l}{\\textit{%s}} \\\\", ncol, rot)

grava <- function(tex, nome) {
  con <- file(file.path(RES, nome), open = "w", encoding = "UTF-8")
  writeLines(tex, con); close(con)
  cat("gravado:", nome, "\n")
}

## ---- hiperparametros (corpo do texto) --------------------------------------
## Colunas da fonte: 1 estrato | 2 processo | 3 coeficientes |
##                   4 sigma_R^2 uni | 5 sigma_I^2 uni |
##                   6 sigma_R^2 multi | 7 sigma_I^2 multi
## Ficam 1-4 e 6; 5 e 7 sao suprimidas depois de conferidas.
corpo_h <- unlist(lapply(seq_along(IND), function(k) {
  i <- IND[[k]]
  l <- linhas_estrato(le(file.path(RES, i$pasta, paste0("tabhiper", i$suf, ".tex"))))
  stopifnot(length(l) == 8)
  lin <- vapply(l, function(x) {
    cel <- celulas(x)
    stopifnot(length(cel) == 7)
    ## so suprime a coluna do irregular se ela for de fato nula
    stopifnot(as.numeric(sub(",", ".", cel[c(5, 7)], fixed = TRUE)) == 0)
    paste(paste(cel[c(1, 2, 3, 4, 6)], collapse = " & "), "\\\\")
  }, "")
  c(if (k > 1) "\\midrule", painel(i$rot, 5), unname(lin))
}))
tex_h <- c(
  "\\begin{table}[H]", "\\centering", "\\footnotesize",
  "\\captionsetup{justification=centering}",
  "\\caption{Hiperpar\u00e2metros estimados - modelos univariado e multivariado}",
  "\\label{tab:hiper}",
  "\\adjustbox{max width=\\linewidth}{%", "\\renewcommand{\\arraystretch}{0.85}",
  "\\begin{tabular}{llccc}", "\\toprule",
  paste0("& \\multicolumn{2}{c}{Erro amostral \\(\\tilde{e}_t\\)} & ",
         "\\multicolumn{2}{c}{Inclina\u00e7\u00e3o: \\(\\hat{\\sigma}_R^2\\)} \\\\"),
  "\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}",
  paste0("\\textbf{Estrato geogr\u00e1fico} & \\textbf{Processo} & \\textbf{Coeficientes} & ",
         "\\textbf{Univariado} & \\textbf{Multivariado} \\\\"),
  "\\midrule", corpo_h, "\\bottomrule", "\\end{tabular}}",
  paste0("\\fonte{Elabora\u00e7\u00e3o pr\u00f3pria, com base nos dados da PNAD Cont\u00ednua. ",
         "Nota: \\(\\tilde{e}_t\\) \u00e9 o erro amostral padronizado (Equa\u00e7\u00e3o ",
         "\\ref{eq:erro_escalonado}) e \\(\\hat{\\sigma}_R^2\\), a vari\u00e2ncia do dist\u00farbio ",
         "da inclina\u00e7\u00e3o (Equa\u00e7\u00e3o \\ref{eq:inclinacao}); processo e coeficientes s\u00e3o ",
         "comuns aos dois modelos. \\(\\hat{\\sigma}_I^2\\) foi estimada em valor ",
         "praticamente nulo em todos os casos e \u00e9 omitida.}"),
  "\\end{table}")
grava(tex_h, "tabhiper_modelos.tex")

## ---- diagnosticos: univariado e multivariado lado a lado (Apendice A) -------
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
  "\\caption{Testes de diagn\u00f3stico dos res\u00edduos - modelos univariado e multivariado}",
  "\\label{tab:apx_diag}",
  "\\adjustbox{max width=\\linewidth}{%", "\\renewcommand{\\arraystretch}{0.85}",
  "\\begin{tabular}{lcccccc}", "\\toprule",
  paste0("& \\multicolumn{3}{c}{\\textbf{Modelo Univariado}} & ",
         "\\multicolumn{3}{c}{\\textbf{Modelo Multivariado}} \\\\"),
  "\\cmidrule(lr){2-4} \\cmidrule(lr){5-7}",
  paste0("\\textbf{Estrato geogr\u00e1fico} & Shapiro-Wilk & Ljung-Box & H & ",
         "Shapiro-Wilk & Ljung-Box & H \\\\"),
  "\\midrule", corpo_d, "\\bottomrule", "\\end{tabular}}",
  paste0("\\fonte{Elabora\u00e7\u00e3o pr\u00f3pria, com base nos dados da PNAD Cont\u00ednua. ",
         "Nota: p-valores; * rejei\u00e7\u00e3o de \\(H_0\\) a 10\\%, ** a 5\\% e *** a 1\\%; ",
         "descartadas as oito primeiras observa\u00e7\u00f5es (estabiliza\u00e7\u00e3o do filtro de Kalman).}"),
  "\\end{table}")
grava(tex_d, "tabdiag_apendice.tex")
